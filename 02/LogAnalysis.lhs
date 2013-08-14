CIS 194 Homework 2:
===================
Brendon Roberto

Part 1: LogAnalysis Module

> {-# OPTIONS_GHC -Wall #-}
> module LogAnalysis where

> import Log

Step 1: Parse String to LogMessage

> parseMessage message = Unknown message
> parseMessage message = case (words message) of
>                            ("E":level:timestamp:rest) -> LogMessage (Error . read $ level) (read timestamp) (unwords rest)
>                            ("W":timestamp:rest) -> LogMessage Warning (read timestamp) (unwords rest)
>                            ("I":timestamp:rest) -> LogMessage Info (read timestamp) (unwords rest)
>                            message -> Unknown message

Step 2: Parse File to [LogMessage]

> parse :: String -> [LogMessage]
> parse x = let logLines = lines x
>                       in map parseMessage logLines

Step 3: Sorting LogMessages

> insert :: LogMessage -> MessageTree -> MessageTree
> insert message @ (LogMessage _ elTimestamp _) (Node left treeMessage @ (LogMessage _ treeTimestamp _) right) = if (elTimestamp > treeTimestamp) 
>                                                                                                                then Node left treeMessage (insert message right) 
>                                                                                                                else Node (insert message left) treeMessage right
> insert message Leaf = Node Leaf message Leaf
> insert message (Node left (Unknown _) _) = insert message left
> insert (Unknown _) tree = tree

> build :: [LogMessage] -> MessageTree
> build [] = Leaf
> build logs = foldr (insert) Leaf logs

> inOrder :: MessageTree -> [LogMessage]
> inOrder (Node Leaf message Leaf) = [message]
> inOrder (Node left message right) = (inOrder left) ++ [message] ++ (inOrder right)
> inOrder Leaf = []

Step 4: Postmortem

> whatWentWrong :: [LogMessage] -> [String]
> whatWentWrong [] = []
> whatWentWrong lst = map (\(LogMessage _ _ message) -> message) (filter whatWentWrongFilterHelper (inOrder . build $ lst))

> whatWentWrongFilterHelper :: LogMessage -> Bool
> whatWentWrongFilterHelper (LogMessage (Error level) _ _) = (level < 50)
> whatWentWrongFilterHelper _ = False
