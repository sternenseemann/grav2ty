{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Grav2ty.Core
import Grav2ty.Simulation
import Grav2ty.Control

import Control.Lens
import Linear.V2
import Control.Monad.Trans.State.Strict
import Data.Fixed (mod')
import Data.Foldable
import Data.Maybe
import Data.Tuple (uncurry)
import qualified Data.Map.Strict as M
import Data.Map.Lens
import Text.Printf

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

renderUi :: (PrintfArg a, Num a) => Grav2tyState a GlossState -> Picture
renderUi state = (uncurry translate) (homBimap ((+ 50) . (* (-1)) . (/ 2) . fromIntegral)
  . (^. graphics.glossViewPort) $ state)
  . scale 0.2 0.2 . Color green . Text $ uiText
  where uiText = printf "Acceleration: %.0f Time/Tick: %f Tick: %d" acc tpt t
        acc = fromMaybe 0 $ state^?inputs.at LocalMod ._Just.modAcc
        t = state^.tick
        tpt = state^.timePerTick

renderStars :: (Float, Float) -> Picture
renderStars center = undefined

renderGame :: Grav2tyState Float GlossState -> Picture
renderGame state = Pictures [ renderUi  state, applyViewPort objs ]
  where objs = Pictures . foldl' (\l x -> renderObject x : l) [] $ state^.world
        applyViewPort = if state^.graphics.glossCenterView
                           then applyViewPortToPicture viewport
                           else id
        viewport = ViewPort
          (homBimap negate $ state^.graphics.glossViewPortCenter)
          0
          (state^.graphics.glossViewPortScale)

boundSub :: (Ord a, Num a) => a -> a -> a -> a
boundSub min a x = if res < min then min else res
  where res = x - a

boundAdd :: (Ord a, Num a) => a -> a -> a -> a
boundAdd max a x = if res > max then max else res
  where res = x + a

eventHandler :: (Show a, Ord a, Real a, Floating a) => Event
             -> Grav2tyState a GlossState -> Grav2tyState a GlossState
eventHandler (EventKey key Down _ _) state = action state
  where updateLocalMod :: Lens' (Modification a) b -> (b -> b)
                       -> Grav2tyState a GlossState -> Grav2tyState a GlossState
        updateLocalMod l f = over (inputs.at LocalMod ._Just.l) f
        accStep = 1
        rotStep = pi / 10
        scaleStep = 1.1
        timeStep = 1.0
        mod2pi = flip mod' (2 * pi)
        action =
          case key of
            SpecialKey KeyUp -> updateLocalMod modAcc (+ accStep)
            SpecialKey KeyDown -> updateLocalMod modAcc (boundSub 0 accStep)
            SpecialKey KeyLeft -> updateLocalMod modRot (mod2pi . (+ rotStep))
            SpecialKey KeyRight -> updateLocalMod modRot (mod2pi . (subtract rotStep))
            SpecialKey KeySpace -> updateLocalMod modFire (const $ state^.tick + 10)
            Char 'c' -> over (graphics.glossCenterView) not
            Char '+' -> over (graphics.glossViewPortScale) (* scaleStep)
            Char '-' -> over (graphics.glossViewPortScale) (/ scaleStep)
            _ -> id
eventHandler (EventResize vp) state = set (graphics.glossViewPort) vp state
eventHandler _ s = s

updateWorld :: Float -> Grav2tyState Float GlossState -> Grav2tyState Float GlossState
updateWorld ts state = snd . flip runState state $ timePerTick .= ts >> processTick hook
  where hook obj@Dynamic { objectMod = LocalMod } =
          graphics.glossViewPortCenter .= (vectorToPoint . objectLoc $ obj)
        hook _ = pure ()

initialWorld :: Fractional a => Grav2tyState a GlossState
initialWorld = snd . flip runState (Grav2tyState 0 (1/300)
  (M.fromList [(LocalMod, zeroModification)])
  (GlossState (800, 800) (0, 0) 1 True)
  mempty 0) $ do
    addObject $ Dynamic shipHitbox 0 10000 (V2 200 0) (V2 0 0) (V2 0 0) LocalMod (Just (V2 15 0, V2 1 0)) Nothing
    addObject $ Dynamic (centeredCircle 10) 0 5000 (V2 0 200) (V2 15 0) (V2 0 0) NoMod Nothing Nothing
    addObject $ Static (centeredCircle 80) 0 moonMass (V2 0 0)
--  addObject $ Static (centeredCircle 40) 0 (0.5 * moonMass) (V2 250 250)
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
