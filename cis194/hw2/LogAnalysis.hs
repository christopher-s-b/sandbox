{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage s = case words s of
  -- I 5053 pci_id: con ing!
  ("I":t:msg) -> LogMessage Info (read t) (unwords msg)

  -- W 3654 e8] PGTT ASF!
  ("W":t:msg) -> LogMessage Warning (read t) (unwords msg)

  -- E 47 1034 'What a pity it
  ("E":sev:t:msg) -> LogMessage (Error $ read sev) (read t) (unwords msg)

  _ -> Unknown s


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) node = node
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node l' m'@(LogMessage _ t' _) r')
  | t <= t' = Node (insert m l') m' r'
  | t > t' = Node l' m' (insert m r')


build :: [LogMessage] -> MessageTree
build ms = foldl (flip insert) Leaf ms

inOrder :: MessageTree -> [LogMessage]
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r
inOrder Leaf = []

--fmap ((take 10) . inOrder . build) $ testParse parse 100 "error.log"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map (\(LogMessage _ _ s) -> s) $
                   filter (\(LogMessage msgType _ _) ->
                            case msgType of
                             Error sev -> sev >= 50
                             _ -> False) $
                   inOrder $ build ms
