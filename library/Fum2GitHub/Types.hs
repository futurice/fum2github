module Fum2GitHub.Types (
    URL(..),
) where

import           Control.Applicative
import qualified Data.Aeson as Aeson
import qualified Data.Text as T


newtype URL = URL { getURL :: String } deriving (Eq, Show)

instance Aeson.FromJSON URL where
  parseJSON v = URL <$> Aeson.parseJSON v
