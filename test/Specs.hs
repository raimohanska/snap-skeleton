module Specs where
import RegexEscapeSpec
import TemplateMatchSpec
import XmlMatchSpec
import FunctionalSpec

import Test.Hspec
import Test.HUnit

main = do 
  hspec (regexEscapeSpecs ++ templateMatchSpecs ++ xmlMatchSpecs)
  runTestTT functionalTests
