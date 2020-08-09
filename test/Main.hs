module Main where

import Test.Tasty
import Test.Tasty.LeanCheck

import qualified Tests.RegAlloc as RegAlloc

main :: IO ()
main = defaultMain $ testGroup ""
  [ LeanCheckTests 0x40000 `localOption` RegAlloc.test ]
