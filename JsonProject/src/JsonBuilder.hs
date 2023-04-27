module JsonBuilder(writeJson, parseJson) where

import JsonObject
import Data.List
import Data.Char (isDigit, isSpace)
import qualified Data.Maybe

reverseExample = "{\"favoriteColors\":[\"orange\",\"yellow\",\"red\"]}"
example = JObject [("name", Just (JString "Desiree")),("age", Just (JNumber 19.9)),
                    ("activities", Just (JList [Just (JString "sleep"), Just (JString "eat"), Just (JString "study")]))]

writeJson :: Maybe JsonValue -> String
writeJson (Just (JString s)) = s
writeJson (Just (JBool b)) = show b
writeJson (Just (JNumber n)) = show n
writeJson (Just (JList l)) = show (map writeJson l)
writeJson (Just (JObject o)) = "{" ++ createFromObject o ++ "}"

showNode :: (String, Maybe JsonValue) -> String
showNode (name,value) = name ++ ": " ++ writeJson value

trim :: String -> String
trim = dropWhile isSpace

createFromObject :: [(String, Maybe JsonValue)] -> String
createFromObject [] = ""
createFromObject xs = intercalate ", " (map showNode xs)

containsList :: String -> Char -> Bool
containsList xs c = isPrefixOf "[" xs && isPrefixOf "]" xs

parseJson :: String -> Maybe JsonValue
parseJson [] = Nothing
parseJson s = Just (JObject (map buildTuple (splitAcc ',' (extractObject (Just s) '{' '}') [])))

extractObject :: Maybe String -> Char -> Char -> Maybe String
extractObject Nothing _ _ = Nothing
extractObject (Just(x:xs)) i f = Just (dropWhile (\x -> (x == i) == True) (takeWhile (\x -> (x == f) == False) xs))

parseString :: String -> Maybe JsonValue
parseString "" = Nothing
parseString ('\"' : xs) = Just (JString (takeWhile (\x -> (x == '\"') == False) xs))
parseString _ = Nothing

parseBool :: String -> Maybe JsonValue
parseBool ('f' : 'a' : 'l' : 's' : 'e' : xs) = Just (JBool False)
parseBool ('t' : 'r' : 'u' : 'e' : xs) = Just (JBool True)
parseBool ('F' : 'a' : 'l' : 's' : 'e' : xs) = Just (JBool False)
parseBool ('T' : 'r' : 'u' : 'e' : xs) = Just (JBool True)
parseBool _ = Nothing

parseNumber :: String -> Maybe JsonValue
parseNumber n = if isDigit' n then Just (JNumber (read n)) else Nothing

parseList :: String -> Maybe JsonValue
parseList [] = Nothing
parseList xs = Just (JList (parseList' xs))

parseList' :: String -> [Maybe JsonValue]
parseList' [] = [Just JNil]
parseList' xs = if isOpenCharList xs then map getJValue (splitAcc ',' (extractObject (Just xs) '[' ']') []) else [Nothing]

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' ('-' : xs) = isDigit' xs
isDigit' [x] = isDigit x
isDigit' (x:xs) = isDigit x && isDigit' xs

isOpenCharList :: String -> Bool
isOpenCharList ('[' : _) = True
isOpenCharList _ = False

isOpenCharObject :: String -> Bool
isOpenCharObject ('{' : _) = True
isOpenCharObject _ = False

splitToTuples :: String -> [(String, Maybe JsonValue)]
splitToTuples s = map buildTuple (splitAcc ',' (Just s) [])


buildTuple :: String -> (String, Maybe JsonValue)
buildTuple [] = ("", Just (JString ""))
buildTuple s = let x:y:xs = splitAcc ':' (Just s) [] in (trim x, getJValue (trim y))

getJValue :: String -> Maybe JsonValue
getJValue xs
            | Data.Maybe.isJust (parseBool xs) = parseBool xs
            | Data.Maybe.isJust (parseNumber xs) = parseNumber xs
            | Data.Maybe.isJust (parseString xs) = parseString xs
            | Data.Maybe.isJust (parseList xs) = parseList xs
            | otherwise = Nothing

splitAcc :: Char -> Maybe String -> String -> [String]
splitAcc _ Nothing [] = []
splitAcc _ (Just []) [] = []
splitAcc _ (Just []) ys = [reverse ys]
splitAcc c (Just(x:xs)) ys
            | x == c = reverse ys : splitAcc c (Just xs) []
            | otherwise = splitAcc c (Just xs) (x : ys)

example' = "{\"a\":10,\"isTrue\":true,\"name\":\"John\"}"

extractNodes :: String -> [(String, String)]
extractNodes xs = map parseKeyValuePairs (splitAcc ',' (Just xs) [])

parseKeyValuePairs :: String -> (String, String)
parseKeyValuePairs s = let x:y:xs = splitAcc ':' (Just s) [] in (x, y)

listString = ["\"favoriteColors\":[\"orange\"","\"yellow\"","\"red\"]"]