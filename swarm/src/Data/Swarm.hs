{-# LANGUAGE Safe #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.Swarm
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Parallel IO working
--
-- TODO: Handle exceptions inside jobs.
----------------------------------------------------------------------------
module Data.Swarm
  (
  -- * Order presevering.
    swarm, swarm'
  -- * Unordered
  , unorderedSwarm, unorderedSwarm'
  -- * Sequential
  , seqSwarm
  ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad ((>=>))
import Control.Concurrent.STM.TBMChan
import Control.DeepSeq
import Data.Foldable
import Data.List
import Data.Ord
import Data.Traversable

return' :: NFData a => a -> IO a
return' = evaluate . force

-- | ALias for 'traverse'. Sequential version of 'swarm'.
seqSwarm :: NFData b => (a -> IO b) -> [a] -> IO [b]
seqSwarm f = traverse (f >=> return')

type InputBus a = TBMChan (IO a)
type OutputBus a = TVar [a]
data Bus a = Bus !(InputBus a) !(OutputBus a)

data SP b = SP {-UNPACK-} !Int !b

instance NFData b => NFData (SP b) where
  rnf (SP _ x) = rnf x

fst' :: SP b -> Int
fst' (SP i _) = i

snd' :: SP b -> b
snd' (SP _ x) = x

commitWork :: NFData a => TVar [a] -> a -> IO ()
commitWork results x = x `deepseq` atomically act
  where act = modifyTVar results (x : )

worker :: NFData a => Bus a -> IO ()
worker bus@(Bus chan results) = do
  x <- atomically $ readTBMChan chan
  case x of
    Nothing     -> return ()
    Just action -> action >>= commitWork results >> worker bus

-- | Like 'traverse', but IO work is spawned to multiple workers.
swarm :: NFData b => (a -> IO b) -> [a] -> IO [b]
swarm f as = do n <- getNumCapabilities; swarm' n f as

-- | 'swarm' with swarm (worker pool) size.
swarm' :: NFData b => Int -> (a -> IO b) -> [a] -> IO [b]
swarm' n f as = do
  let as' = zip [0 :: Int ..] as
  let f' (idx, a) = SP idx `fmap` f a
  bs <- unorderedSwarm' n f' as'
  return $ map snd' $ sortBy (comparing fst') bs                          

-- | Like 'swarm', but the order of elements is not preserved.
unorderedSwarm :: NFData b => (a -> IO b) -> [a] -> IO [b]
unorderedSwarm f as = do n <- getNumCapabilities; swarm' n f as

-- | 'unorderedSwarm' with swarm size.
unorderedSwarm' :: NFData b => Int -> (a -> IO b) -> [a] -> IO [b]
unorderedSwarm' n f as = do
  let l = length as
  chan <- newTBMChanIO (n * 10)
  ress <- newTVarIO []
  -- Spawn swarm
  let bus = Bus chan ress
  forM_ [1..n] $ \_ -> forkIO $ worker bus >> return ()
  -- Submit work
  traverse_ (\a -> atomically . writeTBMChan chan . f $ a) as
  atomically $ closeTBMChan chan
  -- Wait for it to be completed
  atomically $ do
    bs <- readTVar ress
    check (length bs == l)
    return bs
