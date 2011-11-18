module Util.TemplateMatch where

import Text.Regex.XMLSchema.String(match, sed, matchSubex)
import Util.RegexEscape

templateExtract :: String -> String -> [(String, String)]
templateExtract template input = matchSubex (toExtractRegex template) input
  where toExtractRegex = sed (\variable -> "(" ++ variable ++ ".*)") "\\{[^\\}]+\\}" . escapeSome "[]\\^$.|?*+()"
