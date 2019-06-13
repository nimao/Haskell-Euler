import PrimeFactors

divisors = [1..20]

main = print $ product $ zipWith (^) divisors (map max_divisors divisors)

max_divisors :: Integer -> Integer
max_divisors divisor = maximum $ map (count_divisors divisor) $ map prime_factors divisors
    where count_divisors n l = toInteger $ length $ filter (==n) l
