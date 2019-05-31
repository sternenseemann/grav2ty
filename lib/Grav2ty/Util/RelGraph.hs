module Grav2ty.Util.RelGraph
  (
  -- * Interface
    RelGraph ()
  , emptyRel
  , insertRel
  , insertRelNoOv
  , insertSeq
  , lookupRel
  , anyFrom
  , foldlFrom'
  -- * Props
  , prop_relCorrectness
  , prop_insertLookup
  , prop_insertLookupNoOv
  ) where

import Data.Foldable
import Data.Map.Strict (Map (..))
import Data.Maybe
import Data.Sequence (Seq (..), (<|))
import qualified Data.Map.Strict as M

-- | Representation of a directed Relation Graph.
--   Every Edge between two vertices @a@ has a connected value @v@.
--   Every Edge must have an Edge in the opposite direction.
--   A Vertice must not be connected to itself.
--
--   These “laws” are enforced by the functions of this interface.
newtype RelGraph a v = RelGraph { unRelGraph :: Map a (Map a v) }
  deriving (Show, Read, Eq)

-- | Relation Graph without any Edges or Vertices.
emptyRel :: RelGraph a v
emptyRel = RelGraph M.empty

insertInternal :: Ord a => (a -> v -> Map a v -> Map a v) -> a -> a -> v -> v
               -> RelGraph a v -> RelGraph a v
insertInternal f x y v v' =
  if x == y
     then id
     else RelGraph . M.alter (alter v' x) y
            . M.alter (alter v y) x . unRelGraph
  where alter v i Nothing  = Just $ M.singleton i v
        alter v i (Just m) = Just $ f i v m

-- | Insert an edge with two connected values (for either direction)
--   into an 'RelGraph'. Will ignore identity relations.
insertRel :: Ord a => a -> a -> v -> v -> RelGraph a v -> RelGraph a v
insertRel = insertInternal M.insert

-- | Like 'insertRel', but won't overwrite any existing values.
insertRelNoOv :: Ord a => a -> a -> v -> v -> RelGraph a v -> RelGraph a v
insertRelNoOv = insertInternal (M.insertWith (\_ old -> old))

-- | Takes a 'Seq' of Vertices and a function that returns the relations for
--   the associated edge and inserts them into a 'RelGraph'
insertSeq :: Ord a => (a -> a -> (v, v)) -> RelGraph a v -> Seq a -> RelGraph a v
insertSeq f g seq = ins seq g
  where ins (x :<| s) acc = ins s (foldl' (folder x) acc s)
        ins mempty acc = acc
        folder x g el = let (v, v') = f x el
                       in insertRelNoOv x el v v' g

-- | Lookup the Relation between two given Vertices.
lookupRel :: Ord a => a -> a -> RelGraph a v -> Maybe v
lookupRel x y = (>>= (M.lookup y)) . M.lookup x . unRelGraph

-- | Wether any of the Edges from a given Vertex satisfy
--   the given condition.
anyFrom :: Ord a => (v -> Bool) -> a -> RelGraph a v -> Maybe Bool
anyFrom f x = fmap (foldl (\b x -> b || f x) False) . M.lookup x . unRelGraph

-- | Strict foldl over the Edges from a given Vertex
foldlFrom' :: Ord a => (b -> v -> b) -> b -> a -> RelGraph a v -> b
foldlFrom' f res x =
  foldl' f res . fromMaybe M.empty . M.lookup x . unRelGraph

prop_relCorrectness :: Seq Integer -> Bool
prop_relCorrectness seq = and . map cond $ allCombinations g
  where g = insertSeq (\x y -> let r = x * y in (r, negate r)) emptyRel seq
        cond (x, y) = lookupRel x y g == fmap negate (lookupRel y x g)

prop_insertLookupNoOv :: (Ord a, Eq v) => a -> a -> v ->  Bool
prop_insertLookupNoOv x y v =
  x == y || Just v == lookupRel x y (insertRelNoOv x y v v emptyRel)

prop_insertLookup :: (Ord a, Eq v) => a -> a -> v -> RelGraph a v -> Bool
prop_insertLookup x y v g =
  x == y || Just v == lookupRel x y (insertRel x y v v g)

allCombinations :: RelGraph a v -> [(a, a)]
allCombinations (RelGraph m) = foldl' (\l k -> map (\x -> (k, x)) keys ++ l) [] keys
  where keys = M.keys m
