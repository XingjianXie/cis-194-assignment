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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage@(LogMessage _ timeStamp _) messageTree = case messageTree of
  Leaf -> Node Leaf logMessage Leaf
  Node leftTree logMessage'@(LogMessage _ timeStamp' _) rightTree ->
    if timeStamp < timeStamp'
      then Node (insert logMessage leftTree) logMessage' rightTree
      else Node leftTree logMessage' (insert logMessage rightTree)

insert' :: MessageTree -> LogMessage -> MessageTree
insert' a b = insert b a

build :: [LogMessage] -> MessageTree
build list = foldl insert' Leaf list

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMessage rightTree) =
  inOrder leftTree ++ logMessage:inOrder rightTree

pattern :: LogMessage -> Bool
pattern (LogMessage (Error x) _ _) | x >= 50 = True
pattern _ = False

messageString :: LogMessage -> String
messageString (LogMessage _ _ string) = string

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = map messageString (filter pattern ((inOrder.build) list))
