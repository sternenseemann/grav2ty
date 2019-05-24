{-# LANGUAGE TemplateHaskell #-}
module Grav2ty.Control
  ( State (..)
  , control, graphics, world
  , ControlState (..)
  , ctrlInputs, ctrlTimeScale, ctrlTick
  , applyControls
  , Modification (..)
  , zeroModification
  , modAcc, modRot, modFire
  , ExtractFunction (..)
  , updateState
  ) where

import Grav2ty.Simulation

import Control.Lens
import Data.Maybe
import Linear.V2
import Linear.Vector
import qualified Data.Map as Map

data Modification a
  = Modification
  { _modRot :: a        -- ^ Rotation (angle in radiant) set by the modification
  , _modAcc :: a        -- ^ Acceleration set by the modification
  , _modFire :: Integer -- ^ Set to tick a projectile should be fired at
  } deriving (Show, Eq, Ord)

makeLenses ''Modification

zeroModification :: Num a => Modification a
zeroModification = Modification 0 0 (-1)

data ControlState a
  = ControlState
  { _ctrlInputs :: Map.Map Modifier (Modification a)
  -- ^ Map containing the Modifier and the modified values, mainly the
  -- Radial angle the object is rotated at and the current acceleration
  -- of the ship.
  , _ctrlTimeScale :: a
  -- ^ Scaling of time allowing for the simulation to be sped up or slowed down
  , _ctrlTick :: Integer
  -- ^ Current tick. Ticks are not of constant length, but depend on time scale
  --   and the simulation steps per second.
  } deriving (Show, Eq)

makeLenses ''ControlState

data State a g
  = State
  { _control  :: ControlState a
  , _graphics :: g
  , _world    :: World a
  } deriving (Show, Eq)

makeLenses ''State

projectile :: RealFloat a => (V2 a, V2 a) -> Integer -> Object a -> Object a
projectile (pos,speed) tick ship =
  Dynamic (centeredCircle 1) 0 1000 pPos pSpeed 0 NoMod Nothing . Just $ tick + 5000
  where pPos = objectLoc ship + rotateV2 (objectRot ship) pos
        pSpeed = (15 * rotateV2 (objectRot ship) speed) + objectSpeed ship

applyControls :: RealFloat a => ControlState a -> Object a -> [Object a]
applyControls _ obj@Static {} = [obj]
applyControls cs obj@Dynamic {} =
  if isNothing life || fromJust life >= cs^.ctrlTick
     then moddedObjs
     else []
  where life = objectLife obj
        moddedObjs =
          case objectMod obj of
            NoMod -> [obj]
            LocalMod ->
              case Map.lookup (objectMod obj) (cs^.ctrlInputs) of
                Nothing -> [obj]
                Just (Modification rot acc fire) ->
                  let newObj = obj
                       { objectRot = rot
                       , objectAcc = angle rot ^* acc
                       }
                      -- Note: we are relying on laziness here: if objectCannon
                      -- is Nothing the pObj never gets evaluated.
                      pObj = projectile (fromJust . objectCannon $ obj) (cs^.ctrlTick) newObj
                      pList = if cs^.ctrlTick /= fire || isNothing (objectCannon obj)
                                 then []
                                 else [pObj]
                   in newObj : pList

type ExtractFunction a b = Object a -> (State a b -> State a b)

updateState :: (RealFloat a, Ord a) => a -> ExtractFunction a b
                -> State a b -> State a b
updateState t extract state =
  over (control.ctrlTick) (+ 1)
  . set world newWorld
  . updateState' $ state
  where oldWorld = state^.world
        (newWorld, updateState') = tailCall oldWorld ([], id)
        tailCall [] acc = acc
        tailCall (x:xs) (nw, f) = tailCall xs $
          if coll x
             then (nw, f)
             else (updateObject' x ++ nw, extract x . f)
        coll obj = isDynamic obj && collisionWithWorld oldWorld obj
        scaledT = state^.control^.ctrlTimeScale * t
        updateObject' obj =
          map (updateObject scaledT (gravitationForces oldWorld obj))
          . applyControls (state^.control) $ obj
