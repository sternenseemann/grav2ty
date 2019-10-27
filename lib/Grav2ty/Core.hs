{-# LANGUAGE TemplateHaskell #-}
module Grav2ty.Core
  ( -- * Basic Types
    Id (..)
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
  , setObject
  , getObject
  , addObject
  , delObject
  -- ** State
  , Grav2tyState (..)
  , tick, timePerTick, inputs, graphics, world, highestId
  , Tick (..)
  ) where

import Control.Lens
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as M
import Linear.V2

type Id = Integer
type Tick = Integer

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

data Object a
  = Dynamic
  { objectHitbox :: Hitbox a      -- ^ hitbox of the object. Hitbox points at
                                  --   (V2 0 0) will always be at the center of
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

data Modification a
  = Modification
  { _modRot :: a        -- ^ Rotation (angle in radiant) set by the modification
  , _modAcc :: a        -- ^ Acceleration set by the modification
  , _modFire :: Integer -- ^ Set to tick a projectile should be fired at
  } deriving (Show, Eq, Ord)

makeLenses ''Modification

zeroModification :: Num a => Modification a
zeroModification = Modification 0 0 (-1)

type ModMap a = Map Modifier (Modification a)

data Grav2tyState a g = Grav2tyState
  { _tick        :: Tick               -- ^ The 'Tick' the game is at currently.
  , _timePerTick :: a                  -- ^ The time between two 'Tick's.
  , _inputs      :: ModMap a           -- ^ 'Modification's that have to be processed in the next tick.
  , _graphics    :: g                  -- ^ Graphics state. Use @()@ if non-graphical.
  , _world       :: World a
  , _highestId   :: Id
  } deriving (Show, Eq)

makeLenses ''Grav2tyState

-- | The 'Grav2ty' Monad is a renamed 'StateT' holding a 'Grav2tyState'.
type Grav2ty p g m a = StateT (Grav2tyState p g) m a

addObject :: Monad m => Object a -> Grav2ty a g m ()
addObject = setObject Nothing

setObject :: Monad m => Maybe Id -> Object a -> Grav2ty a g m ()
setObject id obj = do
  id <- case id of
          Just id -> pure id
          Nothing -> do
            highestId += 1
            use highestId
  world %= M.insert id obj

getObject :: Monad m => Id -> Grav2ty a g m (Maybe (Object a))
getObject id = use (world.at id)

delObject :: Monad m => Id -> Grav2ty a g m ()
delObject id = world %= M.delete id
