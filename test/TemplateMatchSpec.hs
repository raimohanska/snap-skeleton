module TemplateMatchSpec where

import Test.Hspec
import Util.TemplateMatch

templateMatchSpecs = describe "Template match" [
  it "extracts data from wildcards" (templateExtract "<xml>{name}</xml>" "<xml>jack</xml>" == [("name", "jack")])
  ]
