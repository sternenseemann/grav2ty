module Grav2ty.Control where

import Grav2ty.Simulation

import Linear.V2
import Linear.Vector
import qualified Data.Map as Map

data State a g
  = State
  { control  :: ControlState a
  , graphics :: g
  , world    :: World a
  } deriving (Show, Eq)

data ControlState a
  = ControlState
  { controlInputs :: Map.Map Modifier (a, a) -- ^ Map containing the Modifier
                                             --   and the modified values,
                                             --   mainly the Radial angle the
                                             --   object is rotated at and
                                             --   the current acceleration
                                             --   of the ship.
  } deriving (Show, Eq)

applyControls :: Floating a => ControlState a -> Object a -> Object a
applyControls _ obj@Static {} = obj
applyControls cs obj@Dynamic {} =
  case objectMod obj of
    NoMod -> obj
    LocalMod -> case Map.lookup LocalMod (controlInputs cs) of
               Nothing -> obj
               Just (rot, acc) -> obj
                 { objectRot = rot
                 , objectAcc = angle rot ^* acc
                 }

