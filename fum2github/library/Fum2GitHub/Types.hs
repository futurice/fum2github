----------------------------------------------------------------------------
-- |
-- Module      :  Fum2GitHub.Types
-- Copyright   :  (C) 2015 Futurice
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Common types used in Fum2Github
 ----------------------------------------------------------------------------
module Fum2GitHub.Types (
    URL(..),
) where

import           Control.Applicative
import qualified Data.Aeson as Aeson

-- | Type for URLs. We don't verify they are valid.
newtype URL = URL { getURL :: String } deriving (Eq, Show)

instance Aeson.FromJSON URL where
  parseJSON v = URL <$> Aeson.parseJSON v
