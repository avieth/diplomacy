module Main where

import AdjudicationTests (tests)
import Test.HUnit.Text (runTestTTAndExit)

main :: IO ()
main = runTestTTAndExit tests
