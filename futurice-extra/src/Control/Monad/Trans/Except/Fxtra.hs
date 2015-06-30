{-# LANGUAGE FlexibleContexts #-}
module Control.Monad.Trans.Except.Fxtra (
    exceptT
  , module Control.Monad.Trans.Except
  ) where

import Control.Monad.Trans.Except

-- | Inject 'Either' value into 'ExceptT' monad.
exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT (Right x)  = return x
exceptT (Left e)   = throwE e
