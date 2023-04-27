{-# LANGUAGE LambdaCase #-}
module JsonFunctor where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isDigit, isSpace)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import JsonObject (JsonValue (..))

newtype Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f a = f <$> a

instance Applicative Parser where
  pure f = Parser (\p -> Just (f, p))
  (<*>) (Parser f) (Parser b) =
    Parser
      ( \x -> do
          (f', _) <- f x
          (b', res) <- b x
          Just (f' b', res)
      )

instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) (Parser f) (Parser b) = Parser (\x -> f x <|> b x)

example :: String
example = "{\"employee\": [{\"name\":\"sonoo\",\"salary\":56000,\"married\":true}]}"

parseBool :: Parser JsonValue
parseBool =
  Parser
    ( \case
        't' : 'r' : 'u' : 'e' : xs -> Just (JBool True, xs)
        'f' : 'a' : 'l' : 's' : 'e' : xs -> Just (JBool False, xs)
        _ -> Nothing
    )
--
separateString' :: String -> String -> Int -> Maybe (JsonValue, String)
separateString' [] res 1 = Nothing
separateString' [] res _ = Just (JString "", reverse res)
separateString' ('\"':xs) res 1 = Just (JString (reverse res), xs)
separateString' (x:xs) res counter
  | counter == 1 = separateString' xs (x:res) counter
  | otherwise = separateString' xs res (updateCounter x counter)
  where updateCounter '\"' c = c + 1
        updateCounter _ c = c

separateString :: String -> Maybe (JsonValue, String)
separateString xn = separateString' xn [] 0

parseString :: Parser JsonValue
parseString = Parser separateString

divNumber' :: String -> (Double -> Maybe JsonValue) -> Maybe (JsonValue, String)
divNumber' xs f = case span isDigit xs of
  ("", _) -> Nothing
  (num, str) -> Just (,) <*> f (read num) <*> Just str

divNumber :: String -> Maybe (JsonValue, String)
divNumber [] = Nothing
divNumber xn@(x : xs)
  | x == '-' = divNumber' xs (\num -> Just (JNumber (-1 * num)))
  | otherwise = divNumber' xn (Just . JNumber)

parseNumber :: Parser JsonValue
parseNumber = Parser divNumber

exampleBool :: Maybe (JsonValue, String)
exampleBool = parse parseBool "true hello"

trim :: String -> String
trim = filter (not . isSpace)

removePairs :: String -> Maybe String
removePairs [] = Nothing
removePairs (x : xs)
  | (x == '{' && last xs == '}') || (x == '[' && last xs == ']') = Just (init xs)
  | otherwise = Nothing
--
splitAcc' :: Char -> String -> String -> Int -> Int -> [String]
splitAcc' _ [] [] _ _ = []
splitAcc' _ [] ys _ _ = [reverse ys]
splitAcc' c (x : xs) ys bracket brace
  | x == c && x == ',' && (bracket == 0 && brace == 0) = reverse ys : splitAcc' c xs [] bracket brace
  | x == c && x == ':' && (brace == 0 && bracket == 0) = reverse ys : splitAcc' c xs [] bracket brace
  | x == '[' = splitAcc' c xs (x : ys) (bracket + 1) brace
  | x == '{' = splitAcc' c xs (x : ys) bracket (brace + 1)
  | x == ']' = splitAcc' c xs (x : ys) (bracket - 1) brace
  | x == '}' = splitAcc' c xs (x : ys) bracket (brace - 1)
  | otherwise = splitAcc' c xs (x : ys) bracket brace


splitAcc :: Char -> Maybe String -> [String]
splitAcc _ Nothing = error "Error in pairs"
splitAcc c (Just xs) = splitAcc' c xs [] 0 0

parseParser :: String -> Parser JsonValue -> Maybe JsonValue
parseParser xs p =
  let pos = parse p xs
   in case pos of
        Nothing -> Nothing
        Just (res, _) -> Just res

parseTo :: String -> Maybe JsonValue
parseTo [] = Nothing
parseTo xs@(x : xn)
  | x == '[' = parseParser xs parseList
  | x == '{' = parseParser xs parseJson
  | '\"' `elem` xs = parseParser xs parseString
  | all isDigit xn = parseParser xs parseNumber
  | otherwise = parseParser xs parseBool


createKeyValue :: String -> (String, Maybe JsonValue)
createKeyValue xs = let (x, y) = break (== ':') xs in (x, parseTo (tail y))

separateJson :: String -> Maybe (JsonValue, String)
separateJson [] = Nothing
separateJson xs = Just (JObject (map createKeyValue (splitAcc ',' (removePairs (trim xs)))), "")

parseJson :: Parser JsonValue
parseJson = Parser separateJson

extractJust :: [Maybe JsonValue] -> [JsonValue]
extractJust [] = []
extractJust (x : xs) = case x of
  Nothing -> extractJust xs
  (Just s) -> s : extractJust xs

separateList :: String -> Maybe (JsonValue, String)
separateList [] = Nothing
separateList xs = Just (JList (extractJust (map parseTo (splitAcc ',' (removePairs xs)))), "")

parseList :: Parser JsonValue
parseList = Parser separateList

getJLine :: String -> JsonValue
getJLine xs = fst (fromJust (parse (parseJson <|> parseList <|> parseString <|> parseBool <|> parseNumber) xs))
