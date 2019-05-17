module Grav2ty.Simulation where

import Linear.Metric (norm, distance)
import Linear.V2
import Linear.Vector

data Hitbox a
  = HMultiple [Hitbox a]
  | HLine
  { lineStart  :: V2 a
  , lineEnd    :: V2 a
  }
  | HCircle
  { circleLoc    :: V2 a
  , circleRadius :: a
  } deriving (Eq, Show, Ord)

shipHitbox :: Num a => Hitbox a
shipHitbox = HMultiple
  [ HLine (V2 (-10) (-5)) (V2 (-10) 5)
  , HLine (V2 (-10) (-5)) (V2 10 0)
  , HLine (V2 (-10) 5)    (V2 10 0)
  ]

centeredCircle :: Num a => a -> Hitbox a
centeredCircle r = HCircle (V2 0 0) r

data Modifier
  = NoMod
  | LocalMod
  deriving (Eq, Ord, Show)

data Object a
  = Dynamic
  { objectHitbox :: Hitbox a  -- ^  hitbox of the object. Hitbox points at
                              --    (V2 0 0) will always be at the center of the
                              --    object
  , objectRot    :: a         -- ^ Radial angle
  , objectMass   :: a         -- ^ mass of the object in kg
  , objectLoc    :: V2 a      -- ^ Current location of the object.
  , objectSpeed  :: V2 a      -- ^ Current speed of the Object. Used for
                              --   simulation approximation
  , objectAcc    :: V2 a      -- ^ Current static Acceleration of the object.
                              --   0 unless controlled by the player or
                              --   projectile.
  , objectMod    :: Modifier  -- ^ If and how the Object can be modified during
                              --   the simulation.
  }
  | Static
  { objectHitbox :: Hitbox a -- ^ See above.
  , objectRot    :: a        -- ^ See above.
  , objectMass   :: a        -- ^ See above.
  , objectLoc    :: V2 a     -- ^ See above.
  } deriving (Show, Eq, Ord)

type World a = [Object a]

separated :: (Floating a, Ord a) => Object a -> Object a -> Bool
separated a b = distance (objectLoc a) (objectLoc b) > 3

gravitationForce :: Floating a => Object a -> Object a -> V2 a
gravitationForce a b = (gravityConst *
  ((objectMass a * objectMass b) / (absDistance ** 2)))
  *^ (distance ^/ absDistance)
  where gravityConst = 6.67408e-11
        distance = objectLoc b - objectLoc a
        absDistance = norm distance

gravitationForces :: (Ord a, Floating a) => [Object a] -> Object a -> [V2 a]
gravitationForces []     _   = []
gravitationForces (w:ws) obj =
  if separated obj w
     then gravitationForce obj w : gravitationForces ws obj
     else gravitationForces ws obj

updateObject :: Fractional a =>  a -> [V2 a] -> Object a -> Object a
updateObject _ _ obj@Static {} = obj
updateObject timeStep forces obj@Dynamic {} = obj
    { objectLoc   = objectLoc obj + (objectSpeed obj ^* timeStep)
    , objectSpeed = objectSpeed obj +
      (((sum forces ^/ objectMass obj) + objectAcc obj) ^* timeStep)
    }

