example_b :: Int -> Maybe String
example_b i | i >= 50 = Just "it works"
            | (i > 10) && (i<50) = Just "almost!"
example_b _ = Nothing

data Koko a = Pepe a a 
