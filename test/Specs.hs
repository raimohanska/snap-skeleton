module Specs where
import RegexEscapeSpec
import FunctionalSpec

import Test.Hspec
import Test.HUnit

main = do 
  hspec (regexEscapeSpecs)
  runTestTT functionalTests
