import PrimeFactors --3.hs
import Control.Arrow (first, second)

main = print $ map product $ take 1 $ filter has_legit_pair $ map prime_factors palindromes

palindromes :: [Integer]
palindromes = map reflect [997, 996..]
    where reflect n = 1000*n + 100*(n `mod` 10) + 10*((n `quot` 10) `mod` 10) + (n `quot` 100) 

has_legit_pair :: [Integer] -> Bool
has_legit_pair factors = or $ map legit_partition (partitions factors)

legit_partition :: ([Integer], [Integer]) -> Bool
legit_partition (x, y) = product x < 1000 && product y < 1000

partitions :: [Integer] -> [([Integer], [Integer])]
partitions [] = [([],[])]
partitions (x:xs) = map (second (x:)) p ++ map (first (x:)) p
    where p = partitions xs
