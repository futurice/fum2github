----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Catch.Fxtra
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
 ----------------------------------------------------------------------------
module Control.Monad.Catch.Fxtra (
    hoistEither
  , module Control.Monad.Catch
  ) where

import Control.Monad.Catch

-- | Handy in ad-hoc scripts.
hoistEither :: (MonadThrow m, Exception e) => Either e a -> m a
hoistEither (Right x)  = return x
hoistEither (Left e)   = throwM e
