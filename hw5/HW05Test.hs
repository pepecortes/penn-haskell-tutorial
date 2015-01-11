{-# LANGUAGE TemplateHaskell #-}

module HW05Test where

import HW05
import Ring
import Parser
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.All

prop_intParsingWorks =
  (parse "3" == Just (3 :: Integer, "")) &&
  (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
  (addId == (0 :: Integer))

prop_mod5_a = add (MkMod 3) (MkMod 4) == (MkMod 2)
prop_mod5_b = add (MkMod 1) (MkMod 4) == (MkMod 0)
prop_mod5_c = mul (MkMod 3) (MkMod 4) == (MkMod 2)
prop_mod5_d = mul (MkMod 3) (MkMod 1) == (MkMod 3)
prop_mod5_e = addInv (MkMod 3) == MkMod (-3)
prop_mod5_f = (parse "MkMod 3 + MkMod 5" :: Maybe (Mod5, String)) == Just (MkMod 3," + MkMod 5")

prop_mat_a = add (MkMat (1,2,3,4)) (MkMat (5,6,7,8)) == MkMat (6,8,10,12)
prop_mat_b = (parse "MkMat (1,2,3,4) + MkMat (5,6,7,8)" :: Maybe (Mat2x2,String)) == Just (MkMat (1,2,3,4)," + MkMat (5,6,7,8)")

prop_bool_a = add False (add True False) == add (add False True) False
prop_bool_b = add True addId == True
prop_bool_c = add True $ addInv True == False
prop_bool_d = mul False (mul True False) == mul (mul False True) False
prop_bool_e = mul True $ mulId == True
prop_bool_f = mul True (add True False) == add (mul True True) (mul True False)


main = $(quickCheckAll)
