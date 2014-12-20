{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List
import Data.Char    
parseMessage :: String -> MaybeLogMessage
parseMessage s = case words s of
  ("I":ts:t) -> case readInt ts of
    ValidInt i -> ValidLM (LogMessage Info i (unwords t))
    _ -> InvalidLM s
  ("W":ts:t) -> case readInt ts of 
    ValidInt i -> ValidLM (LogMessage Warning i (unwords t))
    _ -> InvalidLM s
  ("E":sev:ts:t) -> case (readInt sev, readInt ts) of
    (ValidInt i, ValidInt j) -> ValidLM (LogMessage (Error i) j (unwords t))
    _ -> InvalidLM s
  _ -> InvalidLM s
                                              
validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (ValidLM m:t) = m:validMessagesOnly t
validMessagesOnly (_:t) = validMessagesOnly t

parse :: String -> [LogMessage]
parse s = validMessagesOnly (map parseMessage (lines s))

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _) = compare ts1 ts2

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages l = sortBy compareMsgs l

significantError :: LogMessage -> Bool
significantError (LogMessage (Error i) _ _)  = (i >= 50)
significantError _ = False

significantErrorEnhanced :: LogMessage -> String -> Bool
significantErrorEnhanced lm st = (significantError lm) || (lmContainsString lm st)

getString :: LogMessage -> String
getString (LogMessage _ _ st) = st

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = map getString (sortMessages (filter significantError l))

lmContainsString :: LogMessage -> String -> Bool
lmContainsString (LogMessage _ _ st) s = isInfixOf smin stmin
  where
    stmin = map toLower st
    smin = map toLower s
    
messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout s lms = filter (`lmContainsString` s) lms

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced st lms =
  map getString (sortMessages (filter (`significantErrorEnhanced` st) lms))
