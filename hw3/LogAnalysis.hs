{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List (sortBy)
    
parseMessage :: String -> MaybeLogMessage
parseMessage s = case words s of
  ("I":ts:tail) -> case readInt ts of
    ValidInt i -> ValidLM (LogMessage Info i (unwords tail))
    _ -> InvalidLM s
  ("W":ts:tail) -> case readInt ts of 
    ValidInt i -> ValidLM (LogMessage Warning i (unwords tail))
    _ -> InvalidLM s
  ("E":sev:ts:tail) -> case (readInt sev, readInt ts) of
    (ValidInt i, ValidInt j) -> ValidLM (LogMessage (Error i) j (unwords tail))
    _ -> InvalidLM s
  _ -> InvalidLM s
                                              
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (ValidLM m:tail) = m:validMessagesOnly tail
validMessagesOnly (_:tail) = validMessagesOnly tail

parse :: String -> [LogMessage]
parse s = validMessagesOnly (map parseMessage (lines s))

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _) = compare ts1 ts2

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages l = sortBy compareMsgs l

significantError :: LogMessage -> Bool
significantError (LogMessage (Error i) _ _)  = (i >= 50)
significantError _ = False

getString :: LogMessage -> String
getString (LogMessage _ _ st) = st

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = map getString (sortMessages (filter significantError l))

--lmContainsString :: LogMessage -> String -> Bool
--lmContainsString (LogMessage _ _ st) s =  

--messagesAbout :: String -> [LogMessage] -> [LogMessage]
