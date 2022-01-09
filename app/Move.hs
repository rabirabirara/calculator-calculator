module Move (Move (..), Dir (..), doMoves, move) where

import Data.Char
import Data.Maybe (catMaybes)
import Data.List (isInfixOf)
import Data.Text (Text (..), pack, unpack, replace)    -- for string functions
-- import Debug.Trace

data Move = Add Int | Sub Int | Mul Int | Div Int | Exp Int | Flip | Sum | Rev | Back | Mirror | Concat String | Shift Dir | Trans String String deriving Show
data Dir = L | R deriving Show

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
move i  Mirror     = Just (Mirror, mirror i)
move i (Concat sn) = Just (Concat sn, conc i sn)
move i (Shift dir) = Just (Shift dir, shift i dir)
-- wastes of Trans moves are invalid - though this does mean 
-- the solver won't find solves of less depth! can fix this with IDDFS
move i (Trans a b) = 
    let si = show i 
     in if a `isInfixOf` si     -- if the number actually contains the digits
           then Just (Trans a b, transform si a b)
           else Nothing

-- gets rid of Nothing values (invalid moves)
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
transform :: String -> String -> String -> Int
transform si a b = 
    let ti = pack si
        ta = pack a
        tb = pack b
     in read $ unpack $ replace ta tb ti

toChr :: Int -> Char
toChr i = chr $ i + 48

toInt :: Char -> Int
toInt c = ord c - 48

-- if number is negative, result is negative
sumDigits :: Int -> Int
sumDigits i = if i < 0
                 then negate $ sum $ map toInt $ tail $ show i
                 else sum $ map toInt $ show i

revInt :: Int -> Int
revInt i = if i < 0
              then negate $ (read . reverse) $ tail $ show i
              else (read . reverse) $ show i

-- false is left, true is right
shift :: Int -> Dir -> Int
shift i dir = case dir of
                L -> shiftLeft i
                R -> shiftRight i

shiftLeft :: Int -> Int
shiftLeft i = if i < 0
                 then let si = tail $ show i
                       in negate $ read $ (tail si) ++ ([head si])
                 else let si = show i
                       in read $ (tail si) ++ ([head si])

shiftRight :: Int -> Int
shiftRight i = if i < 0
                  then let si = tail $ show i
                        in negate $ read $ (last si) : (init si)
                  else let si = show i
                        in read $ (last si) : (init si)

mirror :: Int -> Int
mirror i = read $ si ++ rsi
    where si = show i
          rsi = if i < 0
                   then reverse $ tail si
                   else reverse si
              
