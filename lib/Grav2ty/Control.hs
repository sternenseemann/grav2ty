{-# LANGUAGE TemplateHaskell #-}
module Grav2ty.Control
  ( State (..)
  , control, graphics, world
  , ControlState (..)
  , ctrlInputs, ctrlTimeScale
  , applyControls
  , Modification (..)
  , zeroModification
  , modAcc, modRot
  ) where

import Grav2ty.Simulation

import Control.Lens
import Linear.V2
import Linear.Vector
import qualified Data.Map as Map

data Modification a
  = Modification
  { _modRot :: a -- ^ Rotation (angle in radiant) set by the modification
  , _modAcc :: a -- ^ Acceleration set by the modification
  } deriving (Show, Eq, Ord)

makeLenses ''Modification

zeroModification :: Num a => Modification a
zeroModification = Modification 0 0

data ControlState a
  = ControlState
  { _ctrlInputs :: Map.Map Modifier (Modification a)
  -- ^ Map containing the Modifier and the modified values, mainly the
  -- Radial angle the object is rotated at and the current acceleration
  -- of the ship.
  , _ctrlTimeScale :: a
  -- ^ Scaling of time allowing for the simulation to be sped up or slowed down
  } deriving (Show, Eq)

makeLenses ''ControlState

data State a g
  = State
  { _control  :: ControlState a
  , _graphics :: g
  , _world    :: World a
  } deriving (Show, Eq)

makeLenses ''State

applyControls :: Floating a => ControlState a -> Object a -> Object a
applyControls _ obj@Static {} = obj
applyControls cs obj@Dynamic {} =
  case objectMod obj of
    NoMod -> obj
    LocalMod -> case Map.lookup LocalMod (cs ^. ctrlInputs) of
               Nothing -> obj
               Just (Modification rot acc) -> obj
                 { objectRot = rot
                 , objectAcc = angle rot ^* acc
                 }

