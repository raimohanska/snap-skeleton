import RegexEscapeSpec
import FunctionalSpec
import System.Exit(exitFailure)

import Test.Hspec
import Test.HUnit

main = do failOnError =<< runTestTT functionalTests
          hspecX regexEscapeSpecs

failOnError :: Counts -> IO ()
failOnError (Counts _ _ 0 0) = return ()
failOnError _                = exitFailure

