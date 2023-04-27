module JsonObject(JsonValue(..),NodeName) where

type NodeName = String
data JsonValue = JString String 
            | JBool Bool 
            | JObject [(String, Maybe JsonValue)]
            | JNumber Double 
            | JList [JsonValue]
            | JNil
            deriving Show