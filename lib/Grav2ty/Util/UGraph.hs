module Grav2ty.Util.UGraph
  (
  -- * Interface
    UGraph ()
  , emptyU
  , insertU
  , insertSeq
  , lookupU
  -- * Props
  , prop_undirected
  , prop_undirected'
  , prop_insertLookup
  ) where

import Data.Foldable
import Data.Map.Strict (Map (..))
import Data.Sequence (Seq (..), (<|))
import qualified Data.Map.Strict as M

newtype UGraph a v = UGraph { unUGraph :: Map a (Map a v) } deriving (Show, Read, Eq)

emptyU :: UGraph a v
emptyU = UGraph M.empty

insertU :: Ord a => a -> a -> v -> UGraph a v -> UGraph a v
insertU x y v = UGraph . M.alter (alterOuter x) y . M.alter (alterOuter y) x . unUGraph
  where alterOuter i Nothing  = Just $ M.singleton i v
        alterOuter i (Just m) = Just $ M.insert i v m

insertSeq :: Ord a => Seq a -> (a -> a -> v) -> UGraph a v -> UGraph a v
insertSeq seq f g = ins seq g
  where ins (x :<| s) acc = ins s (foldl' (\g el -> insertU x el (f x el) g) acc s)
        ins mempty acc = acc

lookupU :: Ord a => a -> a -> UGraph a v -> Maybe v
lookupU x y = (>>= (M.lookup y)) . M.lookup x . unUGraph

prop_undirected' :: (Ord a, Eq v) => UGraph a v -> a -> a -> Bool
prop_undirected' g x y = lookupU x y g == lookupU y x g

prop_undirected :: (Ord a, Eq v) => UGraph a v -> Bool
prop_undirected g = and . map (uncurry (prop_undirected' g)) $ allCombinations g

prop_insertLookup :: (Ord a, Eq v) => a -> a -> v ->  Bool
prop_insertLookup x y v = Just v == lookupU x y (insertU x y v emptyU)

allCombinations :: UGraph a v -> [(a, a)]
allCombinations (UGraph m) = foldl' (\l k -> map (\x -> (k, x)) keys ++ l) [] keys
  where keys = M.keys m
