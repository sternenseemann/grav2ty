{-# LANGUAGE TemplateHaskell #-}
import           Control.FRPNow       hiding (norm, (*^), (^/))
import           Control.FRPNow.Gloss
import           Control.Lens
import           Graphics.Gloss
import           Linear.Metric
import           Linear.V2
import           Linear.Vector

-- | Statically definition of the frames
--   per second the simulation should run at.
fps :: Num a => a
fps = 30

-- | Representation of the objects in this simulation.
--   An Object is either a static object or a dynamic
--   object which gets influenced by the gravity force
--   of other objects.
--   Dynamic objects may but don't have to be controlled
--   by an user. Also multiplayer scenarios are possible.
--   By control I mean controlling the manual acceleration
--   _acc.
data Object
  = Static {
    _loc  :: V2 Float -- ^ location of the object (V2 0 0) is in the centre
                      --   of the simulation
  , _pic  :: Picture  -- ^ the gloss picture to draw the object
  , _mass :: Float    -- ^ mass in kilograms
  }
  | Dynamic {
    _loc  :: V2 Float -- ^ location of the object (V2 0 0) is in the centre
                      --   of the simulation
  , _pic  :: Picture  -- ^ the gloss picture to draw the object
  , _mass :: Float    -- ^ mass in kilograms
  , _acc  :: V2 Float -- ^ current manual acceleration of the object. Think of
                      --   this as the acceleration caused by a spaceship's
                      --   thrusters or similar.
  } deriving (Eq, Show)

makeLenses ''Object

-- | A Universe is a list of objects.
--   Ordering or similar does not matter
type Universe = [Object]

-- | CompSt describes the state of the main
--   computation that is mainly described by
--   nextUniverse. In the FRP context we keep
--   the necessary state as argument of type
--   CompSt. It holds the current and the last
--   Universe. Given these two we can compute
--   the next universe
data CompSt
  = MkCompSt {
    _lastUni :: Universe -- ^ last universe
  , _currUni :: Universe -- ^ current universe
  }

data AccInSt
  = MkAccInSt {
    _angle :: Float -- ^ current angle of the ship
  , _norm  :: Float -- ^ current thruster power / norm of
                    --   the acceleration vector
  }

-- | takePicture extracts the picture
--   of an Object and moves it to its
--   correct position
takePicture :: Object -> Picture
takePicture obj = let (V2 x y) = _loc obj
                      pic      = _pic obj
                    in translate x y pic

-- | dropFirst drops only the first occurence
--   of an element in a list.
dropFirst :: Eq a => a -> [a] -> [a]
dropFirst _ []     = []
dropFirst x (y:ys) = if x == y then ys else dropFirst x ys

-- | scaleBoth is a wrapper around scale from
--   gloss which scales a Picture by the same
--   ratio in both x and y dimension.
scaleBoth :: Float -> Picture -> Picture
scaleBoth f = scale f f

-- | Timestep holds the time that passes between
--   two computation steps in order to compute
--   the next universe correctly. Time changes
--   every frame in frpnow-gloss, so it is 1/fps
--   seconds.
timeStep :: Fractional a => a
timeStep = 1 / fps

-- | Given a Behavior unitChange returns a Behavior of
--   Events that fire an unit if the given behavior
--   changed. Convenience wrapper around frpnow's
--   change.
unitChange :: Eq a => Behavior a -> Behavior (Event ())
unitChange behavior = (() <$) <$> change behavior

-- TODO: think about *best* approximation
-- | Calls nextObject on every object of the
--   universe. Needs the last and the
--   current state of the universe.
nextUniverse :: Universe -> Universe -> Universe
nextUniverse last curr = next last curr last curr
  where next last curr (l:ls) (c:cs)
          = nextObject l c (gravityForces c curr)
            : next last curr ls cs
        next _ _ [] [] = []

-- | Calculates the next position of an object using
--   given gravity forces and the manual acceleration
--   of the given object. It needs the last and the
--   current state of the object since this is just
--   an approximation.
nextObject :: Object -> Object -> [V2 Float] -> Object
nextObject last@(Static {}) curr@(Static {})   _ = curr
nextObject last@(Dynamic {}) curr@(Dynamic {}) forces
  = curr & loc .~
      2 * _loc curr - _loc last + (timeStep ** 2) *^ (_acc curr + sum forces)

-- | Calculates the gravity force between
--   two objectts based on the standard
--   formula.
gravityForce :: Object -> Object -> V2 Float
gravityForce a b = (gravityConst * ((_mass a * _mass b) / (absDistance ** 2)))
  *^ (distance ^/ absDistance)
  where gravityConst = 6.67408e-11
        distance = _loc b - _loc a
        absDistance = norm distance

-- | Calculates the gravity forces between an
--   object and  the rest of the universe.
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

-- | A basic universe for testing purposes.
exampleUniverse :: Universe
exampleUniverse =
  [ Dynamic (V2 0 (earthRadius + 100000)) (color (makeColor 1 0 0 1) $ circleSolid (earthRadius / 100)) spaceshipMass (V2 0 0)
  , Static (V2 0 0) (color (makeColor 0 0 1 1) $ circleSolid (earthRadius)) earthMass
  ]
  where earthMass = 5.974e24
        earthRadius = 6378137
        spaceshipMass = 1935

--acceleration :: EvStream GEvent -> AccInSt -> Behavior (Behavior (V2 Float))
--acceleration evs state = do
--  interestingEvs <- filterEs isInteresting evs
--  e' <- snapshot (acceleration evs newState
--  return $ pure state `switch` e'
--    where isInteresting (EventKey k _ _ _) = if k `elem` [Char '
--          isInteresting _ = False

-- | The main FRP routine. It basically warps
--   universeB and renders the gloss picture
--   for every computation step.
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
