{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read

parseBody :: String -> MessageType -> [String] -> LogMessage
parseBody str messageType (timeStamp:rest) = case readMaybe timeStamp :: Maybe Int of
  Nothing -> Unknown str
  Just timeStamp' -> LogMessage messageType timeStamp' (unwords rest)
parseBody str _ _ = Unknown str

parseMessage :: String -> LogMessage
parseMessage str = case words str of
  "I":rest -> parseBody str Info rest
  "W":rest -> parseBody str Warning rest
  "E":code:rest -> case readMaybe code :: Maybe Int of
    Nothing -> Unknown str
    Just code' -> parseBody str (Error code') rest
  _ -> Unknown str


parse :: String -> [LogMessage]
parse strs = map parseMessage (lines strs)
