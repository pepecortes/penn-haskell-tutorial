guardtest :: Int -> Int
guardtest i
  | mod2 <- mod i 2,
    mod3 <- mod i 3
                   = mod2 + mod3

help :: Int -> Maybe Int
help i
  | i>0 = Just i
  | otherwise = Nothing
          

x :: Int
x = 3
