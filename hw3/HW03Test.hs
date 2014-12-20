{-# LANGUAGE TemplateHaskell #-}

module HW03Test where

import LogAnalysis
import Log
import Test.QuickCheck
import Test.QuickCheck.All

lm1 = (LogMessage Warning 153 "Not a speck of light is showing, so the danger must be growing...")
lm2 = (LogMessage Info 208 "the Weighted Companion Cube cannot talk")
lm3 = (LogMessage (Error 101) 2001 "My God! It’s full of cube of stars!")
lm4 = (LogMessage Info 2001 "Daisy, Daisy, give me your answer do.")
lms = [lm1,lm2,lm3,lm4]
    
prop_parseMessage1 = parseMessage "E 2 562 help help" == ValidLM (LogMessage (Error 2) 562 "help help")
prop_parseMessage2 = parseMessage "I 29 la la la" == ValidLM (LogMessage Info 29 "la la la")
prop_parseMessage3 = parseMessage "This is not in the right format" == InvalidLM "This is not in the right format"

prop_validMessagesOnly = validMessagesOnly [ValidLM (LogMessage (Error 2) 562 "help help"),
                                            ValidLM (LogMessage Info 29 "la la la"),
                                            InvalidLM "This is not in the right format"] ==
                         [(LogMessage (Error 2) 562 "help help"), (LogMessage Info 29 "la la la")]

prop_compareMsgs1 = compareMsgs lm1 lm2 == LT                        
prop_compareMsgs2 = compareMsgs lm3 lm4 == EQ

prop_lmContainsString1 = lmContainsString lm3 "FULL"
prop_lmContainsString2 = lmContainsString lm3 "fullos" == False

prop_messagesAbout = messagesAbout "cube" lms == [lm2,lm3]

prop_significantErrorEnhanced1 = significantErrorEnhanced lm3 "nada"
prop_significantErrorEnhanced2 = significantErrorEnhanced lm2 "cube"
prop_significantErrorEnhanced3 = significantErrorEnhanced lm1 "cube" == False


main = $(quickCheckAll)
