{-# LANGUAGE OverloadedStrings #-}
module Main where

import Grav2ty.Core
import Grav2ty.Control (Grav2tyUpdate (..))
import Grav2ty.Simulation (translateHitbox, scaleHitbox, rotateHitbox)
import Grav2ty.Protocol (messagesParser, renderMessage, protocolVersion
                        , Message (ProtocolVersion), messageUpdateClient)

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception (bracket)
import Control.Lens (set, (.~), (&), (%~))
import Control.Monad (unless, forM_, forever)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Foreign.C.Types (CInt (..))
import Linear.V2
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified SDL as SDL
import SDL (($=))
import SDL.Primitive as GFX
import System.Environment
import System.Exit

emptyState :: Grav2tyState a ()
emptyState = Grav2tyState 0 (5*10^3) mempty () mempty 0

initialWorld :: Fractional a => Grav2tyState a ()
initialWorld = flip (Grav2tyState 0 (10^6) mempty ()) 2 $ M.fromList
  [ (0, Dynamic (centeredCircle 10) 0 5000 (V2 0 200) (V2 15 0) (V2 0 0) NoMod Nothing Nothing)
  , (1, Static (centeredCircle 80) 0 8e14 (V2 0 0))
  , (2, Dynamic shipHitbox 0 300 (V2 200 300) (V2 0 0) (V2 0 0) NoMod Nothing Nothing)
  ]

data Viewport
  = Viewport
  { vpOffset :: V2 CInt
  , vpScale  :: Double
  } deriving (Show, Eq, Ord)

calculateViewport :: SDL.Window -> V2 Double -> Double -> IO Viewport
calculateViewport w (V2 x y) scale = do
  (V2 xo yo) <- fmap (`div` 2) <$> SDL.get (SDL.windowSize w)
  pure
    $ Viewport
    { vpOffset = V2 (xo + round x) (yo + round y)
    , vpScale  = scale
    }

drawObject :: SDL.Renderer -> Viewport ->  Object Double -> IO ()
drawObject r vp = drawHitbox r . viewportHitbox vp

viewportHitbox :: Viewport -> Object Double -> Hitbox CInt
viewportHitbox vp obj =
   fmap round
 . translateHitbox (objectLoc obj + fmap fromIntegral (vpOffset vp))
 . scaleHitbox (vpScale vp)
 . rotateHitbox (objectRot obj)
 $ objectHitbox obj

drawHitbox :: SDL.Renderer -> Hitbox CInt -> IO ()
drawHitbox r (HCombined b) = forM_ b $ drawHitbox r
drawHitbox r (HCircle pos radius) =
  GFX.smoothCircle r pos radius (SDL.V4 255 255 255 255)
drawHitbox r (HLine a b) =
  GFX.smoothLine r a b (SDL.V4 255 255 255 255)

draw :: SDL.Window -> SDL.Renderer -> Grav2tyState Double s -> IO ()
draw w r state = do
  viewport <- calculateViewport w (V2 0 0) 1
  SDL.rendererDrawColor r $= SDL.V4 0 0 0 255
  SDL.clear r

  forM_ (_world state) $ drawObject r viewport

  SDL.present r

noModifier :: SDL.KeyModifier
noModifier = SDL.KeyModifier False False False False False False False False False False False

needExit :: SDL.Event -> Bool
needExit ev =
  case SDL.eventPayload ev of
    SDL.QuitEvent -> True
    SDL.KeyboardEvent d ->
      let keysym = SDL.keyboardEventKeysym d
       in SDL.keysymModifier keysym == noModifier &&
          SDL.keysymKeycode keysym == SDL.KeycodeEscape
    _ -> False

appLoop :: TVar (Grav2tyState Double ()) -> SDL.Window -> SDL.Renderer -> IO ()
appLoop state w r = do
  s <- readTVarIO state
  draw w r s
  ev <- SDL.pollEvent
  let exit = fromMaybe False $ fmap needExit ev

  unless exit $ appLoop state w r

windowSettings :: SDL.WindowConfig
windowSettings = SDL.defaultWindow
  { SDL.windowInitialSize = V2 1024 768
  , SDL.windowResizable = True
  }

netThread :: Socket -> TVar (Grav2tyState Double ()) -> IO ()
netThread sock state = do
  sendAll sock . renderMessage $ (ProtocolVersion protocolVersion :: Message Double)

  forever $ do
    bytes <- recv sock (1024^2 * 100)

    case parseOnly messagesParser bytes of
      Left e -> putStrLn $ "Parse error: " ++ e
      Right m -> do
        current <- _tick <$> readTVarIO state
        forM_ (concatMap (messageUpdateClient current) m) $ \update -> do
          print update
          atomically . modifyTVar state $ \s ->
            case update of
              DeleteObject i -> s & world %~ M.delete i
              UpdateObject i o -> s & world %~ M.insert i o
              SetWorld w -> set world w s
              SetTick t -> set tick t s
              SetTimePerTick tm -> set timePerTick tm s
              -- TODO UpdateMod, NewObject
              _ -> s

run :: String -> String -> IO ()
run host port = do
  state <- newTVarIO emptyState

  let hints = defaultHints { addrFlags = [AI_NUMERICSERV], addrSocketType = Datagram }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr

  putStrLn $ "Using " ++ show (addrAddress addr)

  net <- async $ netThread sock state

  SDL.initializeAll
  bracket (SDL.createWindow "grav2ty" windowSettings) SDL.destroyWindow
    $ \window -> do
      renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
      appLoop state window renderer

  cancel net
  close sock

main :: IO ()
main = do
  args <- getArgs
  name <- getProgName
  case args of
    [host, port] -> run host port
    _ -> do
      putStrLn $ "Usage: " ++ name ++ " HOST PORT"
      exitFailure
