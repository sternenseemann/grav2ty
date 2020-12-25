{-# LANGUAGE RecordWildCards #-}
module Grav2ty.Control (tickUpdates, Grav2tyUpdate (..)) where

import Grav2ty.Core
import Grav2ty.Simulation
import Grav2ty.Util.RelGraph

import Control.Lens
import Control.Monad (when, unless)
import Data.Bifunctor (first)
import Data.Map (Map (..))
import Data.Maybe
import Linear.V2
import Linear.Vector
import qualified Data.Map.Strict as M

data Grav2tyUpdate a
  = DeleteObject Id              -- ^ Delete an object
  | NewObject (Object a)         -- ^ Add an object
  | UpdateObject Id (Object a)   -- ^ Change object with given id to new value
--  | TickDone Tick                -- ^ Last update of a tick signifying it's fully computed
  deriving (Show, Eq, Ord)

projectile :: RealFloat a => (V2 a, V2 a) -> Integer -> Object a -> Object a
projectile (pos,speed) tick ship =
  Dynamic (centeredCircle 1) 0 1000 pPos pSpeed 0 NoMod Nothing . Just $ tick + 5000
  where pPos = objectLoc ship + rotateV2 (objectRot ship) pos
        pSpeed = (15 * rotateV2 (objectRot ship) speed) + objectSpeed ship

getForce :: (Ord a, Num a) =>  ObjRelGraph a -> Id -> V2 a
getForce objRel id = foldlFrom' (\f r -> f + _relForce r) (V2 0 0) id objRel

deletionNecessary :: Tick -> ObjRelGraph a -> Id -> Object a -> Bool
deletionNecessary tick rels id obj =
  isDynamic obj &&                          -- only dynamic objs are deleted
  (maybe False (< tick) (objectLife obj) || -- life span expired?
  (anyFrom _relColl id rels == Just True))  -- collision?

applyModification :: RealFloat a => Grav2tyState a s -> Id -> Object a -> (Object a, [Grav2tyUpdate a])
applyModification _ _ obj@(Static {}) = (obj, [])
applyModification (Grav2tyState {..}) id obj
  | not (doesModify (objectMod obj)) = (obj, [])
  | otherwise =
    case modObj of
      Nothing -> (obj, [])
      Just (newObj, fire) ->
        ( newObj
        , if _tick /= fire
            then []
            else case objectCannon newObj of
                   Just c  -> [NewObject $ projectile c _tick newObj]
                   Nothing -> [])
  where modObj = do
          (Modification rot acc fire) <- M.lookup (objectMod obj) _inputs
          pure (obj { objectRot = rot, objectAcc = angle rot ^* acc }, fire)

objectUpdates :: (Ord a, RealFloat a) => Grav2tyState a s -> ObjRelGraph a -> Id -> Object a -> [Grav2tyUpdate a]
objectUpdates s@(Grav2tyState {..}) rels id obj =
  case obj of
    Static {}      -> []
    d@(Dynamic {}) ->
      let (modObject, newObjects) = applyModification s id obj
          updatedObject = updateObject (fromIntegral _timePerTick / (10**6)) (getForce rels id) modObject
       in if deletionNecessary _tick rels id obj
            then [DeleteObject id]
            else UpdateObject id updatedObject : newObjects

tickUpdates :: (Ord a, RealFloat a) => Grav2tyState a s -> [[Grav2tyUpdate a]]
tickUpdates s@(Grav2tyState {..}) =
  M.foldlWithKey' (\updates id obj -> objectUpdates s objRel id obj : updates) [] _world
  where objRel = objectRelGraph _world
