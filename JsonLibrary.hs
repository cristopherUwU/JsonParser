module JsonLibrary
    ( toJsonString
    ) where

import JsonObject
import JsonBuilder

jsonValue = JObject[("name", Just (JString "John")), ("lastName", Just (JString "Doe"))]
jsonValue' = JObject[("daysOfWeek", Just (JList [Just (JString "M"), Just (JString "T"), Just (JString "T")])),
                    ("name", Just (JString "John")), ("lastName", Just (JString "Doe"))]

stringValue = "{\"flag\": True, \"flag2\": true, \"number\": 5, \"Nnumber\": -5}"

toJsonString :: IO ()
toJsonString = print (parseJson stringValue)
