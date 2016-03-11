module Main where

import Test.HUnit

main
  = runTestTT $ TestList
      [ TestLabel "send" sendT
      ]

sendT :: Test
sendT = TestCase $ do
  assertFailure "unimplemented"
