module Main where

import Grav2ty.Core
import Grav2ty.Control
import qualified Grav2ty.Protocol as GP

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Lens ((%=), use)
import Control.Monad (forM_, unless, forever)
import Control.Monad.IO.Class (liftIO, MonadIO ())
import Control.Monad.Trans.State.Lazy
import Data.Attoparsec.ByteString (parseOnly)
import Data.Hashable
import qualified Data.Map as M
import Data.Maybe (isJust, maybeToList)
import Linear.V2
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendManyTo)
import System.Clock
import System.Posix.Signals (installHandler, Handler (..), sigTERM, sigINT)
import System.Ticked

instance Hashable SockAddr where
  hashWithSalt s (SockAddrUnix str) = s
    `hashWithSalt` str `hashWithSalt` (1 :: Int)
  hashWithSalt s (SockAddrInet p h) = s
    `hashWithSalt` (2 :: Int)
    `hashWithSalt` (fromIntegral p :: Int)
    `hashWithSalt` h
  hashWithSalt s (SockAddrInet6 p f h scope) = s
    `hashWithSalt` (3 :: Int)
    `hashWithSalt` (fromIntegral p :: Int)
    `hashWithSalt` f
    `hashWithSalt` h
    `hashWithSalt` scope


initialState :: Grav2tyState Double ()
initialState = Grav2tyState 0 (10^6) mempty ()
  (M.fromList
    [ (0, Dynamic (centeredCircle 10) 0 5000 (V2 0 200) (V2 15 0) (V2 0 0) NoMod Nothing Nothing)
    , (1, Static (centeredCircle 80) 0 8e14 (V2 0 0)) ])
  1

data TickThreadMsg a
  = TickThreadUpdates [Grav2tyUpdate a]
  | TickThreadDone Tick TimeSpec
  | TickThreadSendWorld SockAddr
  deriving (Show, Eq, Ord)

data NetThreadMsg a
  = NetThreadBroadcast [GP.Message a]
  | NetThreadSendTo SockAddr [GP.Message a]
  deriving (Show, Eq, Ord)

processTick :: TMVar (Grav2tyState Double ()) -> TChan (TickThreadMsg Double) -> IO ()
processTick svar chan = do
  state <- atomically $ takeTMVar svar
  before <- getTime Monotonic

  forM_ (tickUpdates state) $ \updates ->
    atomically $ writeTChan chan (TickThreadUpdates updates)

  after <- getTime Monotonic
  atomically
    . writeTChan chan
    $ TickThreadDone (_tick state) (diffTimeSpec before after)

processUpdates :: TMVar (Grav2tyState Double ())
               -> TChan (TickThreadMsg Double)
               -> TChan (NetThreadMsg Double)
               -> Grav2ty Double () IO ()
processUpdates svar tickChan netChan = forever $ do
  msg <- liftIO . atomically $ readTChan tickChan

  case msg of
    TickThreadUpdates updates -> forM_ updates $ \u -> do
      update' <-
        case u of
          DeleteObject i -> delObject i >> pure u
          UpdateObject i o -> setObject (Just i) o >> pure u
          NewObject o -> do
            i <- addObject o
            pure $ UpdateObject i o
          _ -> error "Update type not implemented" -- TODO

      tick <- use tick
      case GP.updateMessageServer tick update' of
        Just m -> liftIO . atomically
          . writeTChan netChan $ NetThreadBroadcast [m]
        Nothing -> pure ()

    -- TODO this is a bit ugly actually and a lot of indirection
    TickThreadSendWorld addr -> do
      world <- use world
      tick <- use tick
      time <- use timePerTick

      liftIO . atomically . writeTChan netChan
        $ NetThreadSendTo addr [GP.TimePerTick time, GP.NewWorld tick world]
    TickThreadDone t timespec -> do
      tick %= (+ 1)
      state <- get
      liftIO . putStrLn $ "Tick " ++ show t ++ " took "
        ++ show (toNanoSecs timespec) ++ "ns"
      liftIO . atomically $ putTMVar svar state

netOut :: Socket
       -> TVar (M.Map SockAddr Modifier)
       -> TChan (NetThreadMsg Double)
       -> IO ()
netOut s clients netChan = forever $ do
  cmd <- atomically $ readTChan netChan

  case cmd of
    NetThreadBroadcast msgs -> readTVarIO clients >>= \addrs -> do
      flip M.traverseWithKey addrs $ \addr _ ->
        sendManyTo s (map GP.renderMessage msgs) addr
      pure ()
    NetThreadSendTo addr msgs ->
      sendManyTo s (map GP.renderMessage msgs) addr

-- 100 MB
maxPacketLen :: Int
maxPacketLen = 100 * 1024^2

modifierForAddr :: SockAddr -> Modifier
modifierForAddr = Mod . fromIntegral . hash

netIn :: Socket
      -> TVar (M.Map SockAddr Modifier)
      -> TChan (NetThreadMsg Double)
      -> TChan (TickThreadMsg Double)
      -> IO ()
netIn s clients netChan tickChan = forever $ do
  (bytes, addr) <- recvFrom s maxPacketLen

  case parseOnly GP.messagesParser bytes of
    Left err -> do
      putStrLn $ "Could not parse message from " ++ show addr ++ ": " ++ err
      atomically . writeTChan netChan
        $ NetThreadSendTo addr [GP.Error GP.ErrorNoParse]
    Right msgs -> forM_ msgs $ \msg ->
      case msg of
        GP.ProtocolVersion v ->
          if v == GP.protocolVersion
            then let modifier = modifierForAddr addr
                  in atomically $ do
                    modifyTVar clients $ M.insert addr modifier
                    writeTChan tickChan $ TickThreadSendWorld addr
                    writeTChan netChan
                      $ NetThreadSendTo addr [GP.AssignMods [modifier]]
            else do
              putStrLn $ "Incompatible protocol version from " ++ show addr
              atomically . writeTChan netChan
                $ NetThreadSendTo addr [GP.Error GP.ErrorVersionMismatch]
        x -> do
          clientMod <- maybeToList . M.lookup addr <$> readTVarIO clients
          atomically . writeTChan tickChan . TickThreadUpdates
            $ GP.messageUpdateServer clientMod x

getGrav2tyAddr :: IO AddrInfo
getGrav2tyAddr = head <$> getAddrInfo hints (Just "::") (Just "2001")
  where hints = Just $ defaultHints
          { addrFlags = [ AI_NUMERICSERV ]
          , addrFamily = AF_INET6
          , addrSocketType = Datagram
          }

main :: IO ()
main = do
  addr <- getGrav2tyAddr
  sock <- socket (addrFamily addr) (addrSocketType addr) $ addrProtocol addr
  setSocketOption sock IPv6Only 0

  bind sock (addrAddress addr)
  putStrLn $ "Listening on " ++ show (addrAddress addr)

  shutdown <- newEmptyTMVarIO

  _ <- installHandler sigTERM (Catch (atomically $ putTMVar shutdown ())) Nothing
  _ <- installHandler sigINT  (Catch (atomically $ putTMVar shutdown ())) Nothing

  stateForTick <- newTMVarIO initialState
  clients <- newTVarIO mempty

  tickChan <- atomically $ newBroadcastTChan
  netOutChan <- atomically $ newBroadcastTChan

  tickThreads <- async
    . runTicked (_timePerTick initialState)
    $ processTick stateForTick tickChan

  link tickThreads

  stateThread <- async $ do
    tickChan' <- atomically $ dupTChan tickChan
    execStateT (processUpdates stateForTick tickChan' netOutChan) initialState
    pure ()

  link stateThread

  netInThread <- async $ netIn sock clients netOutChan tickChan

  netOutThread <- async $ do
    netOutChan' <- atomically $ dupTChan netOutChan
    netOut sock clients netOutChan'
    pure ()

  link netOutThread

  link2 tickThreads stateThread
  link2 stateThread netOutThread

  -- wait for shutdown signal
  atomically $ takeTMVar shutdown

  -- stop tickThreads
  cancel tickThreads

  -- close socket after killing netThreads
  cancel netOutThread
  cancel netInThread
  close sock

  cancel stateThread
