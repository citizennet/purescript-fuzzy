module Test.Main where

import Prelude

import Data.Fuzzy (match')
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

{--main = runTest do--}
  {--suite "match'" do--}
     {--test "matches full string" do--}
        {--let expected = FuzzBall { substr: ""--}
                                {--, result: [ Right "f"--}
                                          {--, Right "o"--}
                                          {--, Right "o"--}
                                          {--, Right "b"--}
        {--equal "'foobar' should match 'foobar'" $ match' "foobar" "foobar"--}
