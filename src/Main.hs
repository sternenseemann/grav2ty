module Main where

import Grav2ty.Simulation
import Grav2ty.Control

import Linear.V2
import Data.Maybe
import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

vectorToPoint :: V2 a -> (a, a)
vectorToPoint (V2 x y) = (x, y)

renderHitbox :: Hitbox Float -> Picture
renderHitbox box =  Color white $
  case box of
    HCircle (V2 x' y') r -> translate x' y' $ Circle r
    HLine a b -> Line . map vectorToPoint $ [a, b]
    HMultiple boxes -> Pictures $ map renderHitbox boxes

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

renderUi :: (Show a, Num a) => ControlState a -> Picture
renderUi = translate (-350) (350) . scale 0.3 0.3 . Color green . Text . show
  . fromMaybe 0 . fmap snd . Map.lookup LocalMod . controlInputs

renderWorld :: State Float -> Picture
renderWorld (State ps world) = Pictures [renderUi ps, centeredWorld]
  where (objs, center) = renderObjectsCenter world
        centeredWorld  = applyViewPortToPicture viewport $ Pictures objs
        viewport       = ViewPort (invert . fromMaybe (0, 0) $ center) 0 1
        invert (x, y)  = (-x, -y)

eventHandler :: Floating a => Event -> State a -> State a
eventHandler (EventKey key Down _ _) state = state
  { control = ControlState . Map.alter f LocalMod . controlInputs . control $ state
  }
    where f = Just . f' . fromMaybe (0, 0)
          f' = case key of
                SpecialKey KeyUp -> \(rot, acc) -> (rot, acc + 1)
                SpecialKey KeyDown -> \(rot, acc) -> (rot, acc - 1)
                SpecialKey KeyLeft -> \(rot, acc) -> (rot + 0.1, acc)
                SpecialKey KeyRight -> \(rot, acc) -> (rot - 0.1, acc)
                _                   -> id
eventHandler _ s = s

updateWorld :: Float -> State Float -> State Float
updateWorld timeStep (State ctrl world) = State ctrl $ map
  (\obj ->
    updateObject timeStep (gravitationForces world obj) .
      applyControls ctrl $ obj)
  world

initialWorld :: Fractional a => State a
initialWorld = State (ControlState Map.empty)
  [ Dynamic shipHitbox 0 10000 (V2 200 0) (V2 0 0) (V2 0 0) LocalMod
  , Static (centeredCircle 80) 0 moonMass (V2 0 0)
--  , Static (centeredCircle 40) 0 (0.5 * moonMass) (V2 250 120)
  ]
  where moonMass = 8e14

main :: IO ()
main = play
  (InWindow "grav2ty" (800, 800) (0,0))
  black
  300
  initialWorld
  renderWorld
  eventHandler
  updateWorld
