{-# LANGUAGE TemplateHaskell #-}
import Linear.V2
import Linear.Vector
import Linear.Metric
import Debug.Trace
import Control.FRPNow hiding ((*^), (^/), norm)
import Control.FRPNow.Gloss
import Graphics.Gloss
import Control.Lens

fps :: Num a => a
fps = 30

data Object
  = Static {
    _loc  :: V2 Float
  , _pic  :: Picture
  , _mass :: Float
  }
  | Dynamic {
    _loc  :: V2 Float
  , _pic  :: Picture
  , _mass :: Float
  , _acc  :: V2 Float
  } deriving (Eq, Show)

makeLenses ''Object

type Universe = [Object]

data CompSt
  = MkCompSt {
    _lastUni :: Universe
  , _currUni :: Universe
  }

takePicture :: Object -> Picture
takePicture obj = let (V2 x y) = _loc obj
                      pic      = _pic obj
                    in translate x y pic

dropFirst :: Eq a => a -> [a] -> [a]
dropFirst _ []     = []
dropFirst x (y:ys) = if x == y then ys else dropFirst x ys

toTuple :: V2 a -> (a, a)
toTuple (V2 x y) = (x, y)

fromTuple :: (a, a) -> V2 a
fromTuple (x, y) = V2 x y

scaleBoth :: Float -> Picture -> Picture
scaleBoth f = scale f f

timeStep :: Fractional a => a
timeStep = 1 / fps

unitChange :: Eq a => Behavior a -> Behavior (Event ())
unitChange behavior = (() <$) <$> change behavior

-- TODO: think about *best* approximation
nextUniverse :: Universe -> Universe -> Universe
nextUniverse last curr = next last curr last curr
  where next last curr (l:ls) (c:cs)
          = nextObject l c (gravityForces c curr)
            : next last curr ls cs
        next _ _ [] [] = []

nextObject :: Object -> Object -> [V2 Float] -> Object
nextObject last@(Static {}) curr@(Static {})   _ = curr
nextObject last@(Dynamic {}) curr@(Dynamic {}) forces
  = curr & loc .~
      2 * _loc curr - _loc last + (timeStep ** 2) *^ (_acc curr + sum forces)

gravityForce :: Object -> Object -> V2 Float
gravityForce a b = (gravityConst * ((_mass a * _mass b) / (absDistance ** 2)))
  *^ (distance ^/ absDistance)
  where gravityConst = 6.67408e-11
        distance = _loc b - _loc a
        absDistance = norm distance

gravityForces :: Object -> Universe -> [V2 Float]
gravityForces obj uni = map (gravityForce obj) $ dropFirst obj uni

universeB :: CompSt
          -> Behavior Time
          -> Behavior (V2 Float)
          -> Behavior (Behavior Universe)
universeB state time accB = do
  let newState = MkCompSt {
        _lastUni = _currUni state
      , _currUni = nextUniverse (_lastUni state) (_currUni state)
      }
  e <- sample $ unitChange time
  e' <- snapshot (universeB newState time accB) e
  return $ pure (_currUni state) `switch` e'

exampleUniverse :: Universe
exampleUniverse =
  [ Dynamic (V2 0 (earthRadius + 100000)) (color (makeColor 1 0 0 1) $ circleSolid (earthRadius / 100)) spaceshipMass (V2 0 0)
  , Static (V2 0 0) (color (makeColor 0 0 1 1) $ circleSolid (earthRadius)) earthMass
  ]

mainFRP :: Universe
        -> Behavior Time
        -> EvStream GEvent
        -> Behavior (Behavior Picture)
mainFRP uni time evs = do
  let initialState = MkCompSt uni uni
  uniB <- universeB initialState time $ pure (V2 0 0)
  return $ pictures . map (scaleBoth 0.00005 . takePicture) <$> uniB

main :: IO ()
main = runNowGlossPure (InWindow "grav1ty" (800,600) (10,10)) white fps
  (mainFRP exampleUniverse)
