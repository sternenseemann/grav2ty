module Main where

import Test.Tasty as QC
import Test.Tasty.QuickCheck as QC

import Grav2ty.Util.RelGraph

times :: Monad m => Int -> a -> (a -> m a) -> m a
times i res f = if i > 0
                   then f res >>= flip (times (i - 1)) f
                   else pure res

instance (Ord a, Arbitrary a, Arbitrary v) => Arbitrary (RelGraph a v) where
  arbitrary = do
    let g = emptyRel
    c <- arbitrary `suchThat` (>= 0)
    times c g (\g -> do
      x <- arbitrary
      y <- arbitrary
      v <- arbitrary
      v' <- arbitrary
      pure $ insertRel x y v v' g)

relGraph :: TestTree
relGraph = testGroup "Grav2ty.Util.UGraph"
  [ QC.testProperty "Check relational properties of insertMapKey" prop_relCorrectness
  , QC.testProperty "Check insertion and lookup correctness" (prop_insertLookup :: Integer -> Integer ->  String -> RelGraph Integer String -> Bool)
  , QC.testProperty "Check insertion and lookup correctness w/o overwrite" (prop_insertLookupNoOv :: Integer -> Integer ->  String -> Bool)
  ]

libTests :: TestTree
libTests = testGroup "Grav2ty Library Tests" [ relGraph ]

main :: IO ()
main = defaultMain libTests
