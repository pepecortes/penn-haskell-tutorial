{-# LANGUAGE TemplateHaskell #-}

module HW03Test where

import LogAnalysis
import Log
import Test.QuickCheck
import Test.QuickCheck.All

prop_parseMessage1 = parseMessage "E 2 562 help help" == ValidLM (LogMessage (Error 2) 562 "help help")
prop_parseMessage2 = parseMessage "I 29 la la la" == ValidLM (LogMessage Info 29 "la la la")
prop_parseMessage3 = parseMessage "This is not in the right format" == InvalidLM "This is not in the right format"

prop_validMessagesOnly = validMessagesOnly [ValidLM (LogMessage (Error 2) 562 "help help"),
                                            ValidLM (LogMessage Info 29 "la la la"),
                                            InvalidLM "This is not in the right format"] ==
                         [(LogMessage (Error 2) 562 "help help"), (LogMessage Info 29 "la la la")]

prop_compareMsgs1 = compareMsgs
                    (LogMessage Warning 153 "Not a speck of light is showing, so the danger must be growing...")
                    (LogMessage Info 208 "the Weighted Companion Cube cannot talk") == LT                        
prop_compareMsgs2 = compareMsgs
                    (LogMessage (Error 101) 2001 "My God! It’s full of stars!")
                    (LogMessage Info 2001 "Daisy, Daisy, give me your answer do.") == EQ

main = $(quickCheckAll)
