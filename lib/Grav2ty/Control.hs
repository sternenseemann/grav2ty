{-# LANGUAGE BlockArguments #-}
module Grav2ty.Control (processTick) where

import Grav2ty.Core
import Grav2ty.Simulation
import Grav2ty.Util.RelGraph

import Control.Lens
import Control.Monad (when, unless)
import Data.Foldable (traverse_)
import Data.Map (Map (..))
import Data.Maybe
import Linear.V2
import Linear.Vector
import qualified Data.Map.Strict as M


projectile :: RealFloat a => (V2 a, V2 a) -> Integer -> Object a -> Object a
projectile (pos,speed) tick ship =
  Dynamic (centeredCircle 1) 0 1000 pPos pSpeed 0 NoMod Nothing . Just $ tick + 5000
  where pPos = objectLoc ship + rotateV2 (objectRot ship) pos
        pSpeed = (15 * rotateV2 (objectRot ship) speed) + objectSpeed ship

getForce :: (Ord a, Num a) =>  ObjRelGraph a -> Id -> V2 a
getForce objRel id = foldlFrom' (\f r -> f + _relForce r) (V2 0 0) id objRel

applyControls :: (Monad m, RealFloat a)
              => Id -> Object a -> Grav2ty a g m (Maybe (Object a))
applyControls _ obj@Static {} = pure $ Just obj
applyControls id obj@Dynamic {} = use tick >>= \currentTick ->
  if fromMaybe False ((< currentTick) <$> objectLife obj)
     then delObject id >> pure Nothing
     else do
       let mod = objectMod obj
       modOfObj <- use (inputs.at mod)
       if mod == NoMod || modOfObj == Nothing
          then pure $ Just obj
          else do
            let Just (Modification rot acc fire) = modOfObj
            -- inputs.at mod .= Nothing (doesn't work with current gloss impl,
            --                           also not necessary…)

            -- TODO: lenses for Object
            let newObj = obj { objectRot = rot, objectAcc = angle rot ^* acc }
            when (currentTick == fire && isJust (objectCannon obj)) $
              addObject (projectile (fromJust (objectCannon obj)) currentTick newObj)
            pure $ Just newObj

processObject :: (Monad m, RealFloat a)
              => World a -> ObjRelGraph a
              -> (Object a -> Grav2ty a g m ())
              -> Id -> Object a
              -> Grav2ty a g m ()
processObject old rels hook id obj =
  if isDynamic obj && (anyFrom _relColl id rels == Just True)
     -- delete any dynamic object that collided with another object
     then delObject id
     else do
       timeStep <- use timePerTick
       newObj <- fmap (updateObject timeStep (getForce rels id)) <$> applyControls id obj
       traverse (setObject (Just id)) newObj
       traverse_ hook newObj

processTick :: (Monad m, RealFloat a)
            => (Object a -> Grav2ty a g m ())
            -> Grav2ty a g m ()
processTick objHook = do
  oldWorld <- use world
  let objRel = objectRelGraph oldWorld

  use world >>= M.foldlWithKey' (\action id obj ->
    action >> processObject oldWorld objRel objHook id obj) (pure ())

  tick %= (+1)
