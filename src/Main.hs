{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE Rank2Types       #-}
module Main where

import Grav2ty.Simulation
import Grav2ty.Control

import Control.Lens
import Linear.V2
import Data.Maybe
import Data.Tuple (uncurry)
import qualified Data.Map as Map
import Data.Map.Lens

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

data GlossState
  = GlossState
  { _glossViewPort :: (Int, Int)
  , _glossViewPortCenter :: (Float, Float)
  , _glossViewPortScale :: Float
  , _glossCenterView :: Bool
  } deriving (Show, Eq, Ord)

makeLenses ''GlossState

vectorToPoint :: V2 a -> (a, a)
vectorToPoint (V2 x y) = (x, y)

homBimap :: Bifunctor f => (a -> b) -> f a a -> f b b
homBimap f = bimap f f

renderHitbox :: Hitbox Float -> Picture
renderHitbox box =  Color white $
  case box of
    HCircle (V2 x' y') r -> translate x' y' $ Circle r
    HLine a b -> Line . map vectorToPoint $ [a, b]
    HCombined boxes -> Pictures $ map renderHitbox boxes

renderObject :: Object Float -> Picture
renderObject obj = renderHitbox . realHitbox $ obj

renderUi :: (Show a, Num a) => State a GlossState -> Picture
renderUi state = (uncurry translate) (homBimap ((+ 50) . (* (-1)) . (/ 2) . fromIntegral)
    . view (graphics . glossViewPort) $ state)
  . scale 0.3 0.3 . Color green . Text . show
  . fromMaybe 0 $ state^?control.ctrlInputs.at LocalMod ._Just.modAcc

renderStars :: (Float, Float) -> Picture
renderStars center = undefined

renderGame :: State Float GlossState -> Picture
renderGame state = Pictures [ renderUi  state, applyViewPort objs ]
  where objs = Pictures . map renderObject $ state^.world
        applyViewPort = if state^.graphics . glossCenterView
                           then applyViewPortToPicture viewport
                           else id
        viewport = ViewPort
          (homBimap negate $ state^.graphics.glossViewPortCenter)
          0
          (state^.graphics.glossViewPortScale)

eventHandler :: (Show a, Floating a) => Event -> State a GlossState -> State a GlossState
eventHandler (EventKey key Down _ _) state = action state
  where updateLocalMod :: Lens' (Modification a) a -> (a -> a)
                       -> State a GlossState -> State a GlossState
        updateLocalMod l f = over (control.ctrlInputs.at LocalMod ._Just.l) f
        accStep = 1
        rotStep = pi / 10
        scaleStep = 0.05
        action = case key of
                   SpecialKey KeyUp -> updateLocalMod modAcc (+ accStep)
                   SpecialKey KeyDown -> updateLocalMod modAcc (subtract accStep)
                   SpecialKey KeyLeft -> updateLocalMod modRot (+ rotStep)
                   SpecialKey KeyRight -> updateLocalMod modRot (subtract rotStep)
                   Char 'c' -> over (graphics.glossCenterView) not
                   Char '+' -> over (graphics.glossViewPortScale) (+ scaleStep)
                   Char '-' -> over (graphics.glossViewPortScale) (subtract scaleStep)
                   _ -> id
eventHandler (EventResize vp) state = set (graphics.glossViewPort) vp state
eventHandler _ s = s

-- TODO make code more generic and move to Grav2ty.Simulation
updateWorld :: Float -> State Float GlossState -> State Float GlossState
updateWorld timeStep (State ctrl g world) = State ctrl
  (set glossViewPortCenter (fromMaybe (0, 0) center) g) uncollidedWorld
  where uncollidedWorld = foldl collideFolder [] newWorld
        collideFolder res obj =
          if isDynamic obj && collisionWithWorld newWorld obj
             then res
             else obj : res
        (newWorld, center) = updateAndExtract world extractCenter ([], Nothing)
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
initialWorld = State
  (ControlState (Map.fromList [(LocalMod, zeroModification)]) 1)
  (GlossState (800, 800) (0, 0) 1 True)
  [ Dynamic shipHitbox 0 10000 (V2 200 0) (V2 0 0) (V2 0 0) LocalMod
  , Dynamic (centeredCircle 10) 0 5000 (V2 0 200) (V2 15 0) (V2 0 0) NoMod
  , Static (centeredCircle 80) 0 moonMass (V2 0 0)
--  , Static (centeredCircle 40) 0 (0.5 * moonMass) (V2 250 120)
  ]
  where moonMass = 8e14

main :: IO ()
main = play
  (InWindow "grav2ty" (initialWorld^.graphics.glossViewPort) (0,0))
  black
  300
  initialWorld
  renderGame
  eventHandler
  updateWorld
