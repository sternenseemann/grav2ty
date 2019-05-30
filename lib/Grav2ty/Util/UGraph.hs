module Grav2ty.Util.UGraph
  (
  -- * Interface
    UGraph ()
  , emptyU
  , insertU
  , insertSeq
  , lookupU
  , anyU
  -- * Props
  , prop_undirected
  , prop_undirected'
  , prop_insertLookup
  , prop_insertLookupNoOv
  ) where

import Data.Foldable
import Data.Map.Strict (Map (..))
import Data.Sequence (Seq (..), (<|))
import qualified Data.Map.Strict as M

-- | Representation of an undirected Graph.
newtype UGraph a v = UGraph { unUGraph :: Map a (Map a v) } deriving (Show, Read, Eq)

-- | Undirected Graph without any Edges or Vertices.
emptyU :: UGraph a v
emptyU = UGraph M.empty

alterOuter :: Ord a => (a -> v -> Map a v -> Map a v) -> v -> a
           -> Maybe (Map a v) -> Maybe (Map a v)
alterOuter f v i Nothing  = Just $ M.singleton i v
alterOuter f v i (Just m) = Just $ f i v m

-- | Insert an edge with a connected value into an 'UGraph'
insertU :: Ord a => a -> a -> v -> UGraph a v -> UGraph a v
insertU x y v = UGraph . M.alter (alter x) y
  . M.alter (alter y) x . unUGraph
  where alter = alterOuter M.insert v

insertUNoOv :: Ord a => a -> a -> v -> UGraph a v -> UGraph a v
insertUNoOv x y v = UGraph . M.alter (alter y) x
  . M.alter (alter x) y . unUGraph
  where alter = alterOuter (M.insertWith (\_ old -> old)) v

insertSeq :: Ord a => (a -> a -> v) -> UGraph a v -> Seq a -> UGraph a v
insertSeq f g seq = ins seq g
  where ins (x :<| s) acc =
          ins s (foldl' (\g el -> insertUNoOv x el (f x el) g) acc s)
        ins mempty acc = acc

lookupU :: Ord a => a -> a -> UGraph a v -> Maybe v
lookupU x y = (>>= (M.lookup y)) . M.lookup x . unUGraph

anyU :: Ord a => (v -> Bool) -> a -> UGraph a v -> Maybe Bool
anyU f x = fmap (foldl (\b x -> b || f x) False) . M.lookup x . unUGraph

prop_undirected' :: (Ord a, Eq v) => UGraph a v -> a -> a -> Bool
prop_undirected' g x y = lookupU x y g == lookupU y x g

prop_undirected :: (Ord a, Eq v) => UGraph a v -> Bool
prop_undirected g = and . map (uncurry (prop_undirected' g)) $ allCombinations g

prop_insertLookupNoOv :: (Ord a, Eq v) => a -> a -> v ->  Bool
prop_insertLookupNoOv x y v = Just v == lookupU x y (insertUNoOv x y v emptyU)

prop_insertLookup :: (Ord a, Eq v) => a -> a -> v -> UGraph a v ->  Bool
prop_insertLookup x y v g = Just v == lookupU x y (insertU x y v g)

allCombinations :: UGraph a v -> [(a, a)]
allCombinations (UGraph m) = foldl' (\l k -> map (\x -> (k, x)) keys ++ l) [] keys
  where keys = M.keys m
