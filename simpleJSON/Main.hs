module Main where

import SimpleJSON
import PrettyJSON
import Prettify

import Prelude hiding ((<>))

result :: JValue
result = JObject [
  ("query", JString "awkward squad haskell"),
  ("estimatedCount", JNumber 3920),
  ("moreResults", JBool True),
  ("results", JArray [
     JObject [
      ("title", JString "Simon Peyton Jones: papers"),
      ("snippet", JString "Tackling the awkward ..."),
      ("url", JString "http://.../marktoberdorf/")
     ]])
  ]

main = putStrLn $ pretty 11. nest 4. renderJValue $ result