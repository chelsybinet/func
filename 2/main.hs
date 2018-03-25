import System.IO  
import Control.Monad
import Data.List
import Data.Ord

readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

removeLast :: [a] -> [a]
removeLast xs = reverse $ drop 1 $ reverse xs 

solve :: String -> Int
solve xs = count (map readMaybe $ map removeLast $ lines $ xs) 0

count :: [Maybe Int] -> Int -> Int
count [Nothing] s = if (s) == 150 then 1 else 0
count [Just n]  s =
    if s == 150
    then 1
    else if (s+n) == 150
         then 1
         else 0
count ((Nothing):xs) s = 
    if s > 150
    then 0
    else if s == 150
         then 1
         else count xs s
count ((Just n):xs) s =
    if s > 150
    then 0
    else if s == 150
         then 1
         else (count xs (s+n)) + (count xs s)

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solve contents

