module Move where--(Move (..), Dir (..), doMoves, move, isMemCon) where

import Util
import Data.Maybe (mapMaybe)
import Data.List (isInfixOf, sort, subsequences, inits, tails)
import Data.Text (Text (..), pack, unpack, replace)    -- for string functions
import Debug.Trace

data Move = 
    Add Int 
  | Sub Int 
  | Mul Int 
  | Div Int
  | Exp Int
  | Flip 
  | Sum 
  | Rev 
  | Round Int
  | Back 
  | Delete Int
  | Change String 
  | Mirror 
  | Concat String 
  | Insert String Int
  | Filter Char
  | Store 
  | MemCon String
  | Inv10 
  | Shift Dir 
  | Sort Dir
  | Trans String String deriving Show

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
move i  Back       = if i == 0 then Nothing else Just (Back, del i)
-- move i (Delete _)  = Nothing
-- not supposed to be used - it's for display
-- move _ (Change _)  = Nothing
move i  Mirror     = Just (Mirror, mirror i)
-- for some reason, concatenating a negative number breaks the game.  see lvl. 148
move i (Concat sn) = if head sn == '-' then Nothing else Just (Concat sn, conc i sn)
-- move _ (Insert _ _)  = Nothing
move i (Filter cn) = if cn == '-' then Nothing else Just (Filter cn, filterDigits i cn) 
-- move i  Store      = Nothing
move i (MemCon sn) = if head sn == '-' then Nothing else Just (MemCon sn, conc i sn)
move i  Inv10      = Just (Inv10, inv10 i)
move i (Shift dir) = Just (Shift dir, shift i dir)
move i (Sort dir)  = Just (Sort dir, sortDigits i dir)
-- wastes of Trans moves are invalid - though this does mean 
-- the solver won't find solves of less depth! can fix this with IDDFS
move i (Trans a b) = 
    let si = show i 
     in if a `isInfixOf` si     -- if the number actually contains the digits
           then Just (Trans a b, transform si a b)
           else Nothing
move _ _ = Nothing

-- buttons that perform multiple moves are handled here.
multiMove :: Int -> Move -> Maybe [(Move, Int)]
multiMove i (Delete _)    = if i == 0 then Nothing else Just (produceDeletes i)
multiMove i (Insert sn _) = if head sn == '-' then Nothing else Just (produceInserts i sn)
multiMove i (Round _)     = if i < 11 && i > -11 then Nothing else Just (produceRounds i)
multiMove _ _ = Nothing

-- gets rid of Nothing values (invalid moves)
-- do single moves, then do multimoves
doMoves :: Int -> [Move] -> [(Move, Int)]
doMoves i mlst = (mapMaybe (move i) mlst) ++ (concat $ mapMaybe (multiMove i) mlst)

-- read :: String -> a, show :: a -> String
del :: Int -> Int
del i = if i < 10 && i > -10 
              then 0 
              else read $ deleteLast (show i)

deleteLast :: String -> String
deleteLast [] = []      -- unreachable
deleteLast [n] = []
deleteLast (h:t) = h : (deleteLast t)


-- Back and Delete are different buttons, so I will continue to treat them differently.
-- if you can delete the negative sign: TODO
produceDeletes :: Int -> [(Move, Int)]
produceDeletes i =
    if i < 10 && i > -10
       then [(Delete 0, 0)]
       else let negative = i < 0
                inorm = if negative then negate i else i
                si = show i
                len = length si - 1
                fsis = (filter (\s -> length s == len)) . subsequences $ si
                fis = map (\s -> if negative then negate . read $ s else read s) fsis
             in map (\(idx, fi) -> (Delete idx, fi)) (zip [0..len] fis)

            

-- ? sn cannot be a negative number.  the game will just break!
conc :: Int -> String -> Int
conc i sn = read $ (show i) ++ sn


produceInserts :: Int -> String -> [(Move, Int)]
produceInserts ip sn =
    let ng = ip < 0
        i = if ng then negate ip else ip
        si = show i
        len = length si
        (fs,ls,ns) = (inits si, tails si, reverse [0..len])
        halves = if sn == "0" then zip3 (tail fs) (tail ls) (tail ns) else zip3 fs ls ns
     in map (\(f,l,n) -> 
         (Insert sn n, (if ng then negate else id) . read $ ins sn f l)
         ) halves

-- given a first and second half, attach s between them: former ++ s ++ latter
-- * if ins never uses more than a digit, improve efficiency by changing s to a char and consing it latter instead.
ins :: String -> String -> String -> String
ins s former latter = former ++ s ++ latter


-- yes, length . show is the simple way out.  not like it costs much.
produceRounds :: Int -> [(Move, Int)]
produceRounds i = map (\k -> (Round k, roundToPlace i k)) [1..(length . show $ i)-1]


-- it works pretty well.
roundToPlace :: Int -> Int -> Int
roundToPlace n k = f n k * 10^k
    where k10 = 10.0 ^^ k
          f :: Int -> Int -> Int
          f n k = round (fromIntegral n / 10^^k)
     

-- ord: Char -> Int, chr: Int -> Char ; these are by ascii code though
transform :: String -> String -> String -> Int
transform si a b = 
    let ti = pack si
        ta = pack a
        tb = pack b
     in read $ unpack $ replace ta tb ti


-- if number is negative, result is negative
sumDigits :: Int -> Int
sumDigits i = if i < 0
                 then negate . sum . (map toInt) . tail . show $ i
                 else sum . (map toInt) . show $ i

-- reverse an integer's digits
revInt :: Int -> Int
revInt i = if i < 0
              then negate . read . reverse . tail . show $ i
              else read . reverse . show $ i

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
              

isMemCon :: Move -> Bool
isMemCon (MemCon _) = True
isMemCon _ = False


inv10 :: Int -> Int
inv10 i =
    let si = show i
     in read $ map invertChar10 si

invertChar10 :: Char -> Char
invertChar10 c = 
    case c of
      '-' -> '-'
      '0' -> '0'
      d -> toChr $ 10 - toInt d

sortDigits :: Int -> Dir -> Int
sortDigits i R = read . sort . show $ i
sortDigits i L =
    if i < 0
       then negate . read . reverse . sort . show . negate $ i
       else read . reverse . sort . show $ i

-- remove all occurences of cn from i
-- ! If all the digits = cn, then simply return 0!
filterDigits :: Int -> Char -> Int
filterDigits i cn = 
    let fs = filter (\c -> c /= cn) . show $ i
     in if null fs
           then 0
           else read fs
