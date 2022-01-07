module Move (Move (..), doMoves, move) where

import Data.Char
import Data.Maybe (catMaybes)
import Data.Text (Text (..), pack, unpack, replace)    -- for string functions

data Move = Add Int | Sub Int | Mul Int | Div Int | Exp Int | Flip | Sum | Back | Conc String | Trans String String deriving Show

-- To define a function, put the type signature first, then the function patterns.
-- Some moves produce an invalid value, and we want to filter them out right here.  No integer division with remainders!
move :: Int -> Move -> Maybe (Move, Int)
move i (Add n)     = Just (Add n, i + n)
move i (Sub n)     = Just (Sub n, i - n)
move i (Mul n)     = Just (Mul n, i * n)
move i (Div n)     = if rem i n /= 0 
                        then Nothing 
                        else Just (Div n, div i n)
move i (Exp n)     = Just (Exp n, i ^ n)
move i  Sum        = Just (Sum, sumDigits i)
move i  Flip       = Just (Flip, negate i)
move i  Back       = Just (Back, delete i)
move i (Conc sn)   = Just (Conc sn, conc i sn)
move i (Trans a b) = Just (Trans a b, transform i a b)

doMoves :: Int -> [Move] -> [(Move, Int)]
doMoves i mlst = catMaybes $ map (move i) mlst

-- read :: String -> a, show :: a -> String
delete :: Int -> Int
delete i = case deleteLast (show i) of
             []  -> 0
             lst -> read lst
-- delete i = let s = show i in if (length s) <= 1 then 0 else read $ deleteLast s

deleteLast :: String -> String
deleteLast [] = []      -- unreachable
deleteLast [n] = []
deleteLast (h:t) = h : (deleteLast t)

conc :: Int -> String -> Int
conc i sn = read $ (show i) ++ sn
     
-- ord: Char -> Int, chr: Int -> Char ; these are by ascii code though
transform :: Int -> String -> String -> Int
transform i a b =
    let ti = pack . show $ i
        ta = pack a
        tb = pack b
     in read $ unpack $ replace ta tb ti

toChr :: Int -> Char
toChr i = chr $ i + 48

toInt :: Char -> Int
toInt c = ord c - 48

sumDigits :: Int -> Int
sumDigits i = sum $ map toInt $ show i

