module XmlMatchSpec where

import Test.Hspec
import Util.XmlMatch

xmlMatchSpecs = describe "Xml match" [
  it "extracts data from wildcards" (xmlExtract "<xml>{name}</xml>" " <xml>jack</xml>\t" == [("name", "jack")]),
  it "extracts more data from wildcards" (
    xmlExtract 
      "<login><username>{username}</username><password>{password}</password></login>" 
      "<login><username>u</username><password>p</password></login>" 
      == [("username", "u"), ("password", "p")]),
  it "extracts empty result for non-match" (xmlExtract "<xml>{name}</xml>" "<xml>jack</xlm>" == [])
  ]
