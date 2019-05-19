module Main where

import Grav2ty.Simulation
import Grav2ty.Control

import Linear.V2
import Data.Maybe
import Data.Tuple (uncurry)
import Debug.Trace
import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data GlossState
  = GlossState
  { glossViewPort :: (Int, Int)
  , glossViewPortCenter :: (Float, Float)
  , glossCenterView :: Bool
  } deriving (Show, Eq, Ord)

vectorToPoint :: V2 a -> (a, a)
vectorToPoint (V2 x y) = (x, y)

tupleMap :: (a -> b) -> (a, a) -> (b, b)
tupleMap f (a, b) = (f a, f b)

renderHitbox :: Hitbox Float -> Picture
renderHitbox box =  Color white $
  case box of
    HCircle (V2 x' y') r -> translate x' y' $ Circle r
    HLine a b -> Line . map vectorToPoint $ [a, b]
    HCombined boxes -> Pictures $ map renderHitbox boxes

renderObject :: Object Float -> Picture
renderObject obj = translate x y . rot . renderHitbox . objectHitbox $ obj
  where (V2 x y)  = objectLoc obj
        rot       =  rotate (clockwise . toDegree . objectRot $ obj)
        toDegree  = (*) (360 / (2 * pi))
        clockwise = (*) (-1)

renderObjectsCenter :: World Float -> ([Picture], Maybe (Float, Float))
renderObjectsCenter w = accum w ([], Nothing)
  where isLocal Dynamic { objectMod = LocalMod } = True
        isLocal _ = False
        accum [] acc = acc
        accum (w:ws) (l, c) = accum ws
          (renderObject w : l, if isLocal w then Just (vectorToPoint (objectLoc w)) else c)

renderUi :: (Show a, Num a) => State a GlossState -> Picture
renderUi state = (uncurry translate) (tupleMap ((+ 50) . (* (-1)) . (/ 2) . fromIntegral)
    . glossViewPort . graphics $ state)
  . scale 0.3 0.3 . Color green . Text . show
  . fromMaybe 0 . fmap snd . Map.lookup LocalMod . controlInputs . control $ state

renderStars :: (Float, Float) -> Picture
renderStars center = undefined

renderGame :: State Float GlossState -> Picture
renderGame state = Pictures [ renderUi  state
                            , if centeredView then centeredWorld else objs ]
  where objs = Pictures . map renderObject . world $ state
        centeredWorld = applyViewPortToPicture viewport objs
        centeredView = glossCenterView . graphics $ state
        viewport = ViewPort (invert . glossViewPortCenter . graphics $ state) 0 1
        invert (x, y) = (-x, -y)

eventHandler :: Floating a => Event -> State a GlossState -> State a GlossState
eventHandler (EventKey key Down _ _) state = state
  { control = ControlState . Map.alter f LocalMod . controlInputs . control $ state
  , graphics = (graphics state) { glossCenterView = centerView }
  }
    where f = Just . f' . fromMaybe (0, 0)
          f' = case key of
                SpecialKey KeyUp -> \(rot, acc) -> (rot, acc + 1)
                SpecialKey KeyDown -> \(rot, acc) -> (rot, acc - 1)
                SpecialKey KeyLeft -> \(rot, acc) -> (rot + 0.1, acc)
                SpecialKey KeyRight -> \(rot, acc) -> (rot - 0.1, acc)
                _                   -> id
          centerView = (if key == (Char 'c') then not else id)
            . glossCenterView . graphics $ state
eventHandler (EventResize vp) state = state
  { graphics = (graphics state) { glossViewPort = vp } }
eventHandler _ s = s

updateWorld :: Float -> State Float GlossState -> State Float GlossState
updateWorld timeStep (State ctrl g world) = State ctrl
  (g { glossViewPortCenter = fromMaybe (0, 0) center }) newWorld
  where (newWorld, center) = updateAndExtract world extractCenter ([], Nothing)
        extractCenter :: Object Float -> Maybe (Float, Float)
                      -> Maybe (Float, Float)
        extractCenter o@(Dynamic { objectMod = LocalMod }) _ =
          Just . vectorToPoint . objectLoc $ o
        extractCenter _ c = c
        updateAndExtract :: World Float -> (Object Float -> i -> i)
                         -> (World Float, i) -> (World Float, i)
        updateAndExtract [] f acc = acc
        updateAndExtract (x:xs) f (xs', i) = updateAndExtract xs f
          (updateObject' x : xs', f x i)
        updateObject' :: Object Float -> Object Float
        updateObject' obj = updateObject timeStep (gravitationForces world obj)
          . applyControls ctrl $ obj

initialWorld :: Fractional a => State a GlossState
initialWorld = State (ControlState Map.empty) (GlossState (800, 800) (0, 0) True)
  [ Dynamic shipHitbox 0 10000 (V2 200 0) (V2 0 0) (V2 0 0) LocalMod
  , Static (centeredCircle 80) 0 moonMass (V2 0 0)
--  , Static (centeredCircle 40) 0 (0.5 * moonMass) (V2 250 120)
  ]
  where moonMass = 8e14

main :: IO ()
main = play
  (InWindow "grav2ty" (glossViewPort . graphics $ initialWorld) (0,0))
  black
  300
  initialWorld
  renderGame
  eventHandler
  updateWorld
