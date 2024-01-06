module Dragons.TestMain where

import AITests
import FanoronaTests
import Testing

allTests :: Test
allTests = TestGroup "All Tests"
  [ fanoronaTests
  , aiTests
  ]

testMain :: IO ()
testMain = runTests allTests
