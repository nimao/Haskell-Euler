main = print $ sum . takeWhile (<4000000) . filter even $ fibs

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


