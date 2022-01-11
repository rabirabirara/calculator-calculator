
{- NOTES
Solving password levels
-}

module Password where

import Util

check :: Int -> String -> Bool
check i s = 
    let is = (map toInt) . show $ i
        matches = zip is s
     in all isMatch matches

isMatch :: (Int, Char) -> Bool
isMatch (i, c) = 
    case i of
      1 -> c `elem` "ABCabc"
      2 -> c `elem` "DEFdef"
      3 -> c `elem` "GHIghi"
      4 -> c `elem` "JKLjkl"
      5 -> c `elem` "MNOmno"
      6 -> c `elem` "PQRpqr"
      7 -> c `elem` "STUstu"
      8 -> c `elem` "VWXvwx"
      9 -> c `elem` "YZyz"



