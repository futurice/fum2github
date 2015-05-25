module Fum2GitHub.Util (
    URL(URL, getURL),
) where

import           Control.Applicative
import qualified Data.Aeson as Aeson
import qualified Data.Text as T


newtype URL = URL { getURL :: String } deriving (Eq, Show)

instance Aeson.FromJSON URL where
  parseJSON = Aeson.withText "String" $ pure . URL . T.unpack
