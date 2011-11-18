module Util.XmlMatch where

import Util.TemplateMatch(templateExtract)
import Text.Regex.XMLSchema.String(match, sed)
import Data.List(find)

-- Returns list of extracted variables or empty list if no match
xmlExtract :: String -> String -> [(String, String)]
xmlExtract template input = templateExtract (clean template) (clean input)

-- Returns response and list of extracted variables
xmlExtractWithConfig :: [(String, String)] -> String -> Maybe (String, [(String, String)])
xmlExtractWithConfig conf input = firstMatch
  where firstMatch = find (\(_, extractedVars) -> not $ null $ extractedVars) matches
        matches = map (\(template, reply) -> (reply, xmlExtract template input)) conf

clean :: String -> String
clean = sed (const "><") ">\\s*<" . trim
  where trim = dropWhile whitespace . reverse . dropWhile whitespace . reverse
        whitespace c = match "\\s" [c]
