{-# LANGUAGE Safe #-}
----------------------------------------------------------------------------
-- |
-- Module      :  System.IO.Fxtra
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Additional stuff to deal with file IO
----------------------------------------------------------------------------
module System.IO.Fxtra (
    withFile
  , returnOnIOError
  , module System.IO
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import System.IO hiding (withFile)

-- | Generalised 'withFile'
withFile :: (MonadIO m, MonadMask m) => FilePath -> IOMode -> (Handle -> m r) -> m r
withFile name mode = bracket (liftIO $ openFile name mode) (liftIO . hClose)

-- | Helper to dismiss 'IOError'
--
-- > Just <$> readFile "data.txt" `catch` returnOnIOError Nothing
returnOnIOError :: Monad m => a -> IOError -> m a
returnOnIOError x _ = return x
