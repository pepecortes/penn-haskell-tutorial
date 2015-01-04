module HW05 where

import Ring
import Parser
import Data.Maybe (listToMaybe)
import Data.List (stripPrefix)

data Mod5 = MkMod Integer
          deriving (Show, Eq)

data Mat2x2 = MkMat (Integer, Integer, Integer, Integer) -- (x11,x12,x21,x22)
              deriving (Show, Eq)

instance Ring Mod5 where
  addId = MkMod 0
  addInv (MkMod x) = MkMod (negate x)
  mulId = MkMod 1
  add (MkMod x) (MkMod y) = MkMod $ mod (x + y) 5
  mul (MkMod x) (MkMod y) = MkMod $ mod (x * y) 5
  
instance Parsable Mod5 where
  parse str
    | Just rest <- (stripPrefix "MkMod" str) =
           case (parse rest) :: Maybe (Integer, String) of
                  Just (i, remaining) -> Just (MkMod i, remaining)
                  Nothing -> Nothing   
    | otherwise = Nothing


instance Ring Mat2x2 where
  addId = MkMat (1,1,1,1)
  addInv (MkMat (a,b,c,d)) = MkMat ((negate a), (negate b), (negate c), (negate d))
  mulId = MkMat (1,0,0,1)
  add (MkMat (a,b,c,d)) (MkMat (w,x,y,z)) = MkMat ((a+w), (b+x), (c+y), (d+z))
  mul (MkMat (a,b,c,d)) (MkMat (w,x,y,z)) = MkMat (a*w+b*y,a*x+b*w,c*w+d*y,c*x+d*z)

instance Parsable Mat2x2 where
  parse str
    | Just rest <- (stripPrefix "MkMat" str) =
           case (reads rest :: [(I4,String)]) of
                [((a,b,c,d),remaining)] -> Just (MkMat (a,b,c,d), remaining)
                [] -> Nothing
    | otherwise = Nothing

type I4 = (Integer, Integer, Integer, Integer)

instance Ring Bool where -- see http://en.wikipedia.org/wiki/Boolean_ring
  addId = True
  addInv x = True /= x
--  mulId = MkMod 1
  add x y = x /= (y /= (x && y)) -- /= is XOR !
  mul x y = x && y
