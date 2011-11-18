module RegexEscapeSpec where
import Test.Hspec
import Text.Regex.XMLSchema.String(match)
import Util.RegexEscape(escape)

regexEscapeSpecs = describe "Escape function" [
  it "returns original text for simple string" (escape "lol" == "lol"),
  it "escapes . " (escape "." == "\\."),
  it "escapes XML nicely" (matchSelf "<xml/>"),
  it "escapes nasty XML nicely" (matchSelf "<xml>.\\\\s\"(){}[]+</xml>")
  ]

matchSelf s = match (escape s) s
