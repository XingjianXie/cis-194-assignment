{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.Char(digitToInt)

parseNumber' :: Int -> String -> Maybe (Int, String)
parseNumber' _ ""              = Nothing
parseNumber' number (' ':rest) = Just (number, rest)
parseNumber' number (char:rest) | '0' <= char && char <= '9' = 
  let digit = digitToInt(char) in
    parseNumber' (number * 10 + digit) rest
parseNumber' _ _ = Nothing

parseNumber :: String -> Maybe (Int, String)
parseNumber (' ':_)    = Nothing
parseNumber string     = parseNumber' 0 string

parseMessageType :: String -> Maybe (MessageType, String)
parseMessageType ('I':' ':rest) = Just (Info, rest)
parseMessageType ('W':' ':rest) = Just (Warning, rest)
parseMessageType ('E':' ':rest) = let code = parseNumber rest in
  case code of
    Nothing -> Nothing
    Just (code', rest') -> Just (Error code', rest')
parseMessageType _ = Nothing

parseMessage :: String -> LogMessage
parseMessage string = let messageType = parseMessageType string in
  case messageType of
    Nothing -> Unknown string
    Just (messageType', rest) -> let number = parseNumber rest in
      case number of
        Nothing -> Unknown string
        Just (number', rest') -> LogMessage messageType' number' rest'

parseLinebreak' :: String -> String -> [String]
parseLinebreak' string ""          = [string]
parseLinebreak' string ('\n':rest) = string:parseLinebreak' "" rest
parseLinebreak' string (char:rest) = parseLinebreak' (string ++ [char]) rest

parseLinebreak :: String -> [String]
parseLinebreak ""     = []
parseLinebreak string = parseLinebreak' "" string

parse :: String -> [LogMessage]
parse string = map parseMessage (parseLinebreak string)

