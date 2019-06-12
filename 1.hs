main = putStrLn $ show output

multiple3or5 :: (Integral n) => n -> Bool
multiple3or5 n = n `mod` 3 == 0 || n `mod` 5 == 0

output = sum $ filter multiple3or5 [1..999]
