module Main where

import Test.Tasty

import qualified Tests.RegAlloc as RegAlloc

main :: IO ()
main = defaultMain $ testGroup "" [RegAlloc.test]
