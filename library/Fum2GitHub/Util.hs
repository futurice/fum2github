module Fum2GitHub.Util (
    URL(URL, getURL),
) where


newtype URL = URL { getURL :: String } deriving (Eq, Show)
