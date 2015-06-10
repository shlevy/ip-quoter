{-# LANGUAGE QuasiQuotes #-}
import Test.Tasty
import Test.Tasty.HUnit
import Network.Socket
import Network.IP.Quoter

main = defaultMain tests

tests :: TestTree
tests = testCase "INADDR_ANY" $ [ip|0.0.0.0|] @=? iNADDR_ANY
