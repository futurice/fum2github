module Main (main) where

import Data.Aeson as A
import Data.Serialize as S
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Control.Monad.Caching.ResponseMeta

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

decodeEncodeSerialize :: ResponseMeta -> Property
decodeEncodeSerialize sres = Right sres === (S.decode . S.encode) sres

decodeEncodeAeson :: ResponseMeta -> Property
decodeEncodeAeson sres = Right sres === (A.eitherDecode . A.encode) sres

qcProps :: TestTree
qcProps = testGroup "By Quickcheck"
  [ QC.testProperty "Serialize: decode . encode . (:: ResponseMeta) = id" decodeEncodeSerialize
  , QC.testProperty "Aeson: decode . encode . (:: ResponseMeta) = id" decodeEncodeAeson
  ]
