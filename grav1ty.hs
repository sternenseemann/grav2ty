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

timeStep :: Fractional a => a
timeStep = 1 / fps

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
  where gravityConst = 6.67408 * (10 ** (-11))
        distance = _loc b - _loc a
        absDistance = norm distance

gravityForces :: Object -> Universe -> [V2 Float]
gravityForces obj uni = map (gravityForce obj) $ dropFirst obj uni

universeB :: Universe
          -> Universe
          -> Behavior Time
          -> Behavior Float
          -> Behavior (Behavior Universe)
universeB last curr time acc = do
  e <- sample $ (() <$) <$> change time
  e' <- snapshot (universeB curr (nextUniverse last curr) time acc) e
  return $ pure curr `switch` e'

exampleUniverse :: Universe
exampleUniverse =
  [ Dynamic (V2 0 0) (color (makeColor 255 0 0 255) $ circleSolid 10) 10000 (V2 (-3) 0)
  , Static (V2 0 (-300)) (color (makeColor 0 0 255 255) $ circleSolid 100) 200000000000
  , Static (V2 0 300) (color (makeColor 0 0 255 255) $ circleSolid 100)    100000000000
  ]

mainFRP :: Universe
        -> Behavior Time
        -> EvStream GEvent
        -> Behavior (Behavior Picture)
mainFRP uni time evs = do
  uniB <- universeB uni uni time (pure 0)
  return $ pictures . map takePicture <$> uniB

main :: IO ()
main = runNowGlossPure (InWindow "grav1ty" (800,600) (10,10)) white fps
  (mainFRP exampleUniverse)
