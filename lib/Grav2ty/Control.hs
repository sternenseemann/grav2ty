{-# LANGUAGE BlockArguments #-}
module Grav2ty.Control (processTick) where

import Grav2ty.Core
import Grav2ty.Simulation
import Grav2ty.Util.RelGraph

import Control.Lens
import Control.Monad (when, unless)
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

modifyObject :: (Monad m, RealFloat a)
             => Id -> Object a -> Grav2ty a g m (Object a)
modifyObject id obj@Static {} = pure obj
modifyObject id obj@Dynamic {} = use tick >>= \currentTick ->
  let mod = objectMod obj in use (inputs.at mod) >>= \modOfObj ->
    if mod == NoMod || isNothing modOfObj
       then pure obj
       else do
         let Just (Modification rot acc fire) = modOfObj
         -- inputs.at mod .= Nothing (doesn't work with current gloss impl,
         --                           also prob not necessary…)

         -- TODO: lenses for Object
         let newObj = obj { objectRot = rot, objectAcc = angle rot ^* acc }
         when (currentTick == fire && isJust (objectCannon obj)) $
           addObject (projectile (fromJust (objectCannon obj)) currentTick newObj)
         pure newObj

deletionNecessary :: Monad m
                  => ObjRelGraph a -> Id -> Object a
                  -> Grav2ty a g m Bool
deletionNecessary rels id obj = do
  currentTick <- use tick
  pure $
    isDynamic obj &&                                 -- only dynamic objs are deleted
    (maybe False (< currentTick) (objectLife obj) || -- life span expired?
    (anyFrom _relColl id rels == Just True))         -- collision?

processObject :: (Monad m, RealFloat a)
              => World a -> ObjRelGraph a
              -> (Object a -> Grav2ty a g m ())
              -> Id -> Object a
              -> Grav2ty a g m ()
processObject old rels hook id obj =
  deletionNecessary rels id obj >>= \del ->
    if del
       then delObject id
       else do
         timeStep <- use timePerTick
         newObj <- updateObject timeStep (getForce rels id) <$> modifyObject id obj
         setObject (Just id) newObj
         hook newObj

-- | If called advances the simulation by one 'Tick' relying on the 'Grav2tyState'.
--
--   It also calls the provided hook-Action once for every remaining 'Object'. This
--   action can be used to update the '_graphics' state @g@ or modify the behaviour
--   of @processTick@ altogether.
processTick :: (Monad m, RealFloat a)
            => (Object a -> Grav2ty a g m ())
            -> Grav2ty a g m ()
processTick objHook = do
  oldWorld <- use world
  let objRel = objectRelGraph oldWorld

  use world >>= M.foldlWithKey' (\action id obj ->
    action >> processObject oldWorld objRel objHook id obj) (pure ())

  tick %= (+1)
