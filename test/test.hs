{-# LANGUAGE QuasiQuotes #-}
import Test.Tasty
import Test.Tasty.HUnit
import Network.Socket
import Network.IP.Quoter
import System.Endian

main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [
  testCase "localhost" $ [ip|127.0.0.1|] @=? (toBE32 2130706433),
  testCase "v6localhost" $ [ip|::1|] @=? (0, 0, 0, 1)
  ]
