{-# LANGUAGE TemplateHaskell #-}
module Grav2ty.Core
  ( -- * Basic Types
    Id (..)
  , Tick (..)
  , World (..)
  -- ** Object
  , Object (..)
  , isDynamic
  , Cannon (..)
  , Modifier (..)
  -- *** Hitboxes
  , Hitbox (..)
  , shipHitbox
  , centeredCircle
  -- ** Modification handling
  , Modification (..)
  , modAcc, modRot, modFire
  , ModMap (..)
  , zeroModification
  -- * The Grav2ty Monad
  , Grav2ty (..)
  -- ** State
  , Grav2tyState (..)
  , tick, timePerTick, inputs, graphics, world, highestId
  -- ** Operations
  , setObject
  , getObject
  , addObject
  , delObject
  ) where

import Control.Lens
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as M
import Linear.V2

-- | Identifier used for 'Object's in 'World'.
type Id = Integer

-- | A tick is a simulation step. This type represents the ascending number of simulation steps.
type Tick = Integer

-- | The 'Object's are stored in a strict 'Map'. We need to access all Objects relatively frequently
--   and also add 'Object's from time to time as well as access 'Object's by their 'Id'. 'Map'
--   seems to provide a good compromise in terms of performance for these operations.
type World a = Map Id (Object a)

data Modifier
  = NoMod            -- ^ Not modified, purely physics based.
  | LocalMod         -- ^ Object is modified by local client / player.
  | External Integer -- ^ Object is modified by an external source / other players.
  deriving(Eq, Ord, Show)

-- | @Just (<cannon position>, <cannon direction>)@ describes origin and
--   trajectory of projectiles of this object. Note that both position and
--   direction are rotated by 'objectRot'. @Nothing@ means that projectiles
--   are disabled for the particular 'Object'.
type Cannon a = Maybe (V2 a, V2 a)

-- | Objects come in two flavors: 'Static' Objects don't change in the course
--   of the simulation, but will influence other Objects either by collision
--   or gravity. They also can never be destroyed.
--
--   'Dynamic' objects are affected by physics and are destroyed on collision.
--   They also may be controlled by a player depending on their 'Modifier'.
data Object a
  = Dynamic
  { objectHitbox :: Hitbox a      -- ^ hitbox of the object. Hitbox points at
                                  --   @(V2 0 0)@ will always be at the center of
                                  --   the object
  , objectRot    :: a             -- ^ Radial angle
  , objectMass   :: a             -- ^ mass of the object in kg
  , objectLoc    :: V2 a          -- ^ Current location of the object.
  , objectSpeed  :: V2 a          -- ^ Current speed of the Object. Used for
                                  --   simulation approximation
  , objectAcc    :: V2 a          -- ^ Current static Acceleration of the object.
                                  --   0 unless controlled by the player or
                                  --   projectile.
  , objectMod    :: Modifier      -- ^ If and how the Object can be modified
                                  --   during the simulation.
  , objectCannon :: Cannon a      -- ^ Point and Direction projectiles can or
                                  --   can not be fired from.
  , objectLife   :: Maybe Integer -- ^ Tick the Object will be destroyed at.
  }
  | Static
  { objectHitbox :: Hitbox a -- ^ See above.
  , objectRot    :: a        -- ^ See above.
  , objectMass   :: a        -- ^ See above.
  , objectLoc    :: V2 a     -- ^ See above.
  } deriving (Show, Eq, Ord)

-- | Wether the 'Object' is Dynamic, i. e. affected by physics
isDynamic :: Object a -> Bool
isDynamic Dynamic {} = True
isDynamic _ = False

-- | Hitboxes are the basis for collision detection and also may be
--   used as a basis for the graphical representation of 'Object's,
--   although they probably should be replaced by a more appealing
--   alternative.
--
--   They can be combined from lines and circles and are always
--   centered around position of the corresponding 'Object',
--   i. e. @V2 0 0@ of the Hitbox is always at the center of
--   the object. Also they naturally rotate with the object.
data Hitbox a
  = HCombined [Hitbox a]
  | HLine
  { lineStart  :: V2 a
  , lineEnd    :: V2 a
  }
  | HCircle
  { circleLoc    :: V2 a
  , circleRadius :: a
  } deriving (Eq, Show, Ord)

-- | Example 'Hitbox' for a triangular, asteroids-like spaceship
shipHitbox :: Num a => Hitbox a
shipHitbox = HCombined
  [ HLine (V2 (-10) (-5)) (V2 (-10) 5)
  , HLine (V2 (-10) (-5)) (V2 10 0)
  , HLine (V2 (-10) 5)    (V2 10 0)
  ]

-- | Generates a 'Hitbox' with a given radius centered around (0,0).
centeredCircle :: Num a => a -> Hitbox a
centeredCircle = HCircle (V2 0 0)

-- | A Modification contains the attributes of an Object that can
--   be controlled by a player: Rotation, acceleration and firing
--   of projectiles.
data Modification a
  = Modification
  { _modRot :: a        -- ^ Rotation (angle in radiant) set by the modification
  , _modAcc :: a        -- ^ Acceleration set by the modification
  , _modFire :: Integer -- ^ Tick a projectile should be fired at
  } deriving (Show, Eq, Ord)

makeLenses ''Modification

-- | 'Modification' that represents the default state of an 'Object'.
zeroModification :: Num a => Modification a
zeroModification = Modification 0 0 (-1)

-- | Used to store the 'Modification's for every controllable 'Object'
--   that is being simulated.
type ModMap a = Map Modifier (Modification a)

data Grav2tyState a g = Grav2tyState
  { _tick        :: Tick               -- ^ The 'Tick' the game is at currently.
  , _timePerTick :: a                  -- ^ The time between two 'Tick's.
  , _inputs      :: ModMap a           -- ^ 'Modification's that have to be processed in the next tick.
  , _graphics    :: g                  -- ^ Graphics state. Use @()@ if non-graphical.
  , _world       :: World a            -- ^ All objects.
  , _highestId   :: Id                 -- ^ Highest 'Id' used in 'World'. This is updated by 'addObject'
                                       --   in Order to prevent accidental overwrites.
  } deriving (Show, Eq)

makeLenses ''Grav2tyState

-- | The 'Grav2ty' Monad is a renamed 'StateT' holding a 'Grav2tyState'.
type Grav2ty p g m a = StateT (Grav2tyState p g) m a

-- | Shortcut for @'setObject' Nothing@.
addObject :: Monad m => Object a -> Grav2ty a g m ()
addObject = setObject Nothing

-- | setObject overwrites or sets the 'Object' at the given 'Id'.
--   If no 'Id' is given it picks a new 'Id' using '_highestId'
--   that is guaranteed to be unused (if nothing messed with the 'World').
setObject :: Monad m => Maybe Id -> Object a -> Grav2ty a g m ()
setObject id obj = do
  id <- case id of
          Just id -> pure id
          Nothing -> do
            highestId += 1
            use highestId
  world %= M.insert id obj

-- | Returns the 'Object' at 'Id'.
getObject :: Monad m => Id -> Grav2ty a g m (Maybe (Object a))
getObject id = use (world.at id)

-- | Deletes the 'Object' at 'Id'. Note: This doesn't influence '_highestId'
--   which only ever increases.
delObject :: Monad m => Id -> Grav2ty a g m ()
delObject id = world %= M.delete id
