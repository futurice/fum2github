----------------------------------------------------------------------------
-- |
-- Module      :  Data.Swarm
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Parallel IO working
----------------------------------------------------------------------------
module Data.Swarm (seqSwarm, swarm, swarm') where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan
import Control.DeepSeq
import Data.Foldable
import Data.List
import Data.Ord
import Data.Traversable

-- | ALis for 'traverse'. Sequential version of 'swarm'.
seqSwarm :: NFData b => (a -> IO b) -> [a] -> IO [b]
seqSwarm f = traverse (fmap force . f)

type Index = Int

commitWork :: NFData a => TVar [(Index, a)] -> Index -> a -> IO ()
commitWork results idx x = x `deepseq` atomically (modifyTVar results ((idx, x) : ))

worker :: NFData a => TMChan (Index, IO a) -> TVar [(Index, a)] -> IO ()
worker chan results = do
  x <- atomically $ readTMChan chan
  case x of
    Nothing            -> return ()
    Just (idx, action) -> action >>= commitWork results idx >> worker chan results

-- | Like 'traverse', but IO work is spawned to multiple workers.
swarm :: NFData b => (a -> IO b) -> [a] -> IO [b]
swarm f as = do n <- getNumCapabilities; swarm' n f as

swarm' :: NFData b => Int -> (a -> IO b) -> [a] -> IO [b]
swarm' n f as = do
  let l = length as
  chan <- newTMChanIO
  ress <- newTVarIO []
  -- Spawn swarm
  forM_ [1..n] $ \_ -> forkIO $ worker chan ress >> return ()
  -- Submit work
  atomically $ traverse_ (\(idx, a) -> writeTMChan chan (idx, (f a))) (zip [0..] as) >> closeTMChan chan
  -- Wait for it to be completed
  atomically $ do
    bs <- readTVar ress
    check (length bs == l)
    return $ map snd $ sortBy (comparing fst) bs
