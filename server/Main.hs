module Main where

import Grav2ty.Core
import Grav2ty.Control

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TChan
import Control.Lens ((%=))
import Control.Monad (forever, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy
import qualified Data.Map as M
import Linear.V2
import System.Clock
import System.Ticked

initialState :: Grav2tyState Double ()
initialState = Grav2tyState 0 (10^6) mempty ()
  (M.fromList
    [ (0, Dynamic (centeredCircle 10) 0 5000 (V2 0 200) (V2 15 0) (V2 0 0) NoMod Nothing Nothing)
    , (1, Static (centeredCircle 80) 0 8e14 (V2 0 0)) ])
  1

data TickThreadMsg a
  = TickThreadUpdates [Grav2tyUpdate a]
  | TickThreadDone Tick TimeSpec
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

processUpdates :: TMVar (Grav2tyState Double ()) -> TChan (TickThreadMsg Double)
               -> Grav2ty Double () IO ()
processUpdates svar chan = forever $ do
  msg <- liftIO . atomically $ readTChan chan
  liftIO $ print msg

  case msg of
    TickThreadUpdates updates -> forM_ updates $ \u -> do
      liftIO $ print u
      case u of
        DeleteObject i -> delObject i
        UpdateObject i o -> setObject (Just i) o >> pure ()
        NewObject o -> addObject o >> pure ()
    TickThreadDone t timespec -> do
      tick %= (+ 1)
      state <- get
      liftIO . putStrLn $ "Tick " ++ show t ++ " took "
        ++ show (toNanoSecs timespec) ++ "ns"
      liftIO . atomically $ putTMVar svar state

main :: IO ()
main = do
  stateForTick <- newTMVarIO initialState
  tickChan <- newTChanIO

  tickThreads <- async
    . runTicked (_timePerTick initialState)
    $ processTick stateForTick tickChan

  link tickThreads

  stateThread <- async $ do
    execStateT (processUpdates stateForTick tickChan) initialState
    pure ()

  link stateThread
  link2 tickThreads stateThread

  wait tickThreads
  wait stateThread
