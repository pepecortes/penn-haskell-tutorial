{-# LANGUAGE TemplateHaskell #-}

module HW02Test where

import Words
import HW02
import Data.List
import Test.QuickCheck
import Test.QuickCheck.All

prop_formableBy1 = formableBy "fun" ['x','n','i','f','u','e','l'] == True
prop_formableBy2 = formableBy "haskell" ['k','l','e','h','a','l','s'] == True
prop_formableBy3 = formableBy "haskell" ['k','l','e','h','a','y','s'] == False

prop_wordsFrom = wordsFrom ['h','e','l','l','o'] == ["eh","el","ell","he","hell","hello", "helo", "ho","hoe","hole","lo","oe","oh","ole" ]

prop_wordsFitsTemplate1 = wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" == True
prop_wordsFitsTemplate2 = wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" == False
prop_wordsFitsTemplate3 = wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" == False
prop_wordsFitsTemplate4 = wordFitsTemplate "let" ['x','x'] "let" == True

prop_wordsFittingTemplate = wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] == ["acre","bare","carb","care","carl","earl"]

prop_scrabbleValueWord1 = scrabbleValueWord "care" == 6
prop_scrabbleValueWord2 = scrabbleValueWord "quiz" == 22

prop_bestWords1 = bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']) == ["carb"]
prop_bestWords2 = bestWords ["cat", "rat", "bat"] == ["bat","cat"]
prop_bestWords3 = bestWords [] == []


main = $(quickCheckAll)
