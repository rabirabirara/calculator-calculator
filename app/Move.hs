module Move (Move (..), doMoves, move) where

import Data.Char
import Data.Maybe (catMaybes)
import Data.Text (Text (..), pack, unpack, replace)    -- for string functions
-- import Debug.Trace

data Move = Add Int | Sub Int | Mul Int | Div Int | Exp Int | Flip | Sum | Rev | Back | Conc String | Trans String String deriving Show

-- To define a function, put the type signature first, then the function patterns.
-- Some moves produce an invalid value, and we want to filter them out right here.  No integer division with remainders!
move :: Int -> Move -> Maybe (Move, Int)
move i (Add n)     = Just (Add n, i + n)
move i (Sub n)     = Just (Sub n, i - n)
move i (Mul n)     = Just (Mul n, i * n)
-- Don't divide if it produces remainder.
move i (Div n)     = if rem i n /= 0 
                        then Nothing 
                        else Just (Div n, div i n)
move i (Exp n)     = Just (Exp n, i ^ n)
move i  Sum        = Just (Sum, sumDigits i)
move i  Flip       = Just (Flip, negate i)
move i  Rev        = Just (Rev, revInt i)
-- Don't backspace if at fixed point.
move i  Back       = if i == 0
                        then Nothing
                        else Just (Back, delete i)
move i (Conc sn)   = Just (Conc sn, conc i sn)
move i (Trans a b) = Just (Trans a b, transform i a b)

doMoves :: Int -> [Move] -> [(Move, Int)]
doMoves i mlst = catMaybes $ map (move i) mlst

-- read :: String -> a, show :: a -> String
delete :: Int -> Int
delete i = if i < 10 && i > -10 
              then 0 
              else read $ deleteLast (show i)

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

revInt :: Int -> Int
revInt i = 
    let negative = i < 0
        si = if negative then show $ negate i else show i
        ri = (read . reverse) si
     in if negative then negate ri else ri
