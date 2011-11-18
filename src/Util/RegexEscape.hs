module Util.RegexEscape(escape, escapeSome, defaultEscapedChars) where

-- Escape a string for usage as Regex
escape :: String -> String
escape = escapeSome defaultEscapedChars

escapeSome :: [Char] -> String -> String
escapeSome escapedChars = concat . map (escapeChar escapedChars)

escapeChar escapedChars c | c `elem` escapedChars = backSlashEscape c
                          | otherwise               = [c]
  where backSlashEscape c = ['\\', c]

defaultEscapedChars = "[]\\^$.|?*+(){}"
