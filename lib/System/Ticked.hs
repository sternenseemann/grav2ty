module System.Ticked
  ( runTicked
  , tickGenerator
  , tickRunner
  , Ticked (..)
  ) where

import System.Clock
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TBQueue
import Control.Monad (when, forever, unless)
import Control.Monad.STM
import Data.Maybe

newtype Ticked = Ticked { unTicked :: TBQueue () }

tickGenerator :: Int -> Ticked -> Integer -> IO ()
tickGenerator d (Ticked queue) tick = do
  atomically $ unGetTBQueue queue ()
  threadDelay d
  tickGenerator d (Ticked queue) (tick + 1)

tickRunner :: Ticked -> IO () -> IO ()
tickRunner (Ticked q) action = forever $ do
  _ <- atomically $ do
    ts' <- readTBQueue q
    writeTBQueue q ts'
    pure ts'
  action
  _ <- atomically $ readTBQueue q
  pure ()

runTicked :: Int -> IO () -> IO ()
runTicked delay action = do
  tickQueue <- Ticked <$> newTBQueueIO 1

  tr <- async $ tickRunner tickQueue action
  linkOnly (const True) tr

  tg <- async $ tickGenerator delay tickQueue 0
  linkOnly (const True) tg

  link2Only (const True) tg tr

  wait tg
  wait tr
-- we use linkOnly (const True) to also stop if one of the threads gets cancelled