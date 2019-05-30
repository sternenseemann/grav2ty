module Main where

import Test.Tasty as QC
import Test.Tasty.QuickCheck as QC

import Grav2ty.Util.UGraph

times :: Monad m => Int -> a -> (a -> m a) -> m a
times i res f = if i > 0
                   then f res >>= flip (times (i - 1)) f
                   else pure res

instance (Ord a, Arbitrary a, Arbitrary v) => Arbitrary (UGraph a v) where
  arbitrary = do
    let g = emptyU
    c <- arbitrary `suchThat` (>= 0)
    times c g $ (\g -> do
      x <- arbitrary
      y <- arbitrary
      v <- arbitrary
      pure $ insertU x y v g)

uGraph :: TestTree
uGraph = testGroup "Grav2ty.Util.UGraph"
  [ QC.testProperty "Check undirectedness" (prop_undirected :: UGraph Int Bool -> Bool)
  , QC.testProperty "Check undirectedness (with vertices not in the graph)" (prop_undirected' :: UGraph Char Int -> Char -> Char -> Bool)
  , QC.testProperty "Check insertion and lookup correctness" (prop_insertLookup :: Integer -> Integer ->  String -> Bool)
  ]

libTests :: TestTree
libTests = testGroup "Grav2ty Library Tests" [ uGraph ]

main :: IO ()
main = defaultMain libTests
