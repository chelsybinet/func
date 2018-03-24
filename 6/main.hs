import System.IO  
import Control.Monad
import Data.List
import Data.Ord

solve :: String -> String
solve xs = map getPlusFrequent $ reverse $ drop 1 $ reverse $ transposeString $ lines xs

getPlusFrequent :: Ord a => [a] -> a
getPlusFrequent xs = head . maximumBy (comparing length) . group . sort $ xs

transposeString ([]:_) = []
transposeString x = (map head x) : transposeString (map tail x)

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStrLn $ show $ solve contents

