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
import Grav2ty.Util.UGraph

import Control.Lens
import Data.Foldable
import Data.Maybe
import Data.Sequence ((<|), (|>), (><))
import qualified Data.Sequence as S
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

applyControls :: RealFloat a => ControlState a -> Object a -> World a
applyControls _ obj@Static {} = S.singleton obj
applyControls cs obj@Dynamic {} =
  if isNothing life || fromJust life >= cs^.ctrlTick
     then moddedObjs
     else S.empty
  where life = objectLife obj
        moddedObjs =
          case objectMod obj of
            NoMod -> S.singleton obj
            LocalMod ->
              case Map.lookup (objectMod obj) (cs^.ctrlInputs) of
                Nothing -> S.singleton obj
                Just (Modification rot acc fire) ->
                  let newObj = obj
                       { objectRot = rot
                       , objectAcc = angle rot ^* acc
                       }
                      -- Note: we are relying on laziness here: if objectCannon
                      -- is Nothing the pObj never gets evaluated.
                      pObj = projectile (fromJust . objectCannon $ obj) (cs^.ctrlTick) newObj
                   in if cs^.ctrlTick /= fire || isNothing (objectCannon obj)
                                 then S.singleton newObj
                                 else S.fromList [pObj, newObj]

type ExtractFunction a b = Object a -> Maybe (State a b -> State a b)

updateState :: (RealFloat a, Ord a) => a -> ExtractFunction a b
                -> State a b -> State a b
updateState t extract state =
  over (control.ctrlTick) (+ 1)
  . set world newWorld
  . fromMaybe id updateState' $ state
  where oldWorld = state^.world
        (newWorld, updateState') = foldl' updateAndExtract (S.empty, Nothing) oldWorld
        updateAndExtract acc@(seq, f) x =
          if isDynamic x && (anyU relColl x objectRel == Just True)
             then acc
             else (updateObject' x >< seq, (.) <$> extract x <*> f)
        objectRel = objectRelGraph oldWorld
        scaledT = state^.control^.ctrlTimeScale * t
        updateObject' obj =
          fmap (updateObject scaledT (gravitationForces oldWorld obj))
          . applyControls (state^.control) $ obj
