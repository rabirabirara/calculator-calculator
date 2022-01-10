module Calc (Calc (..), Change (..), Storage (..), Prop (..), solve) where

import Move
import Data.Maybe (isJust)
import Data.Array (listArray)
import Debug.Trace

-- record syntax - for product types with named type constructors
data Calc = Calc 
    { start   :: Int
    , goal    :: Int
    , depth   :: Int
    , moves   :: [Move]
    , changes :: [Change]   -- for buttons that alter moves
    , storage :: Maybe Storage  -- TODO: Maybe and List are both monads, replace maybe with the list monad in future.
    , portals :: [Portal]
    } deriving Show

-- portals are indices going from right to left, starting from 0.
data Prop = PropPortal Portal

type Portal = (Int, Int)

type Storage = Maybe Int


-- Even if it's points-free, you still have to include everything in the signature.
successor :: Calc -> (Int -> [(Move, Int)])
successor c = ((flip doMoves) (moves c))

test :: Calc -> (Int -> Bool)
test c = (== (goal c))

invalid :: Int -> Bool
invalid i = i > 999999 || i < -99999

data Change = Inc Int deriving Show

changeToMove :: Change -> Move
changeToMove (Inc i) = Change ("[+]" ++ show i)

-- for each change in (changes c), produce a new Calc along with the Change that got there.
produceChanges :: Calc -> [(Change, Calc)]
produceChanges c = map (\ch -> (ch, change c ch)) (changes c)

-- applies a change to a calc
change :: Calc -> Change -> Calc
change c ch =
    let newMoves = 
            case ch of
              Inc i -> map (incMove i) (moves c)
        newStorage = (changeMem ch) (storage c)
     in Calc { start   = start c
             , goal    = goal c
             , depth   = depth c
             , moves   = newMoves
             , changes = changes c
             -- fmap Just (mx) will wrap mx in second Just or leave as Nothing
             , storage = newStorage
             }

incMove :: Int -> Move -> Move
incMove i mv = trace "HERE" $
    case mv of
      Add num -> Add (num + i)
      Sub num -> Sub (num + i)
      Mul num -> Mul (num + i)
      Div num -> Div (num + i)
      Exp num -> Exp (num + i)
      Concat snum -> Concat (show $ i + read snum)
      MemCon snum -> MemCon (show $ i + read snum)
      -- TODO: implement inc of Trans
      mv -> mv

-- so with this new storage feature, let's just say there's only one storage button. you can store the current total with no cost. we produce two sets of moves - those after storage and those without storage.
-- write an int to memory AND update its buttons.
writeMem :: Int -> Calc -> Calc
writeMem i c = Calc { start = start c
                    , goal = goal c
                    , depth = depth c
                    , moves = writeMemToMoves (moves c) (Just i)
                    , changes = changes c
                    , storage = Just (Just i)
                    }

-- update a movelist with memory by erasing the old memconcat buttons and adding the new one.
writeMemToMoves :: [Move] -> Storage -> [Move]
writeMemToMoves mlst mem =
    case mem of
      Nothing -> mlst
      Just i -> MemCon (show i) : filter (\m -> not $ isMemCon m) mlst

-- in case a change button and a memcon button are both present.
changeMem :: Change -> Maybe Storage -> Maybe Storage
changeMem (Inc i) (Just (Just mem)) = Just (Just (i + mem))
changeMem _ Nothing = Nothing


-- convert array of digits (Char, specifically) into an Int
readDigitArray :: Array Int Char -> Int
readDigitArray a = undefined


-- using Arrays?? in Haskell? preposterous. also, Array has no data constructors; it's just a type. but you do specify the inner types.
doPortal :: Array Int Char -> Portal -> Array Int Char
doPortal a (fromHere, toHere) =
    let leavingDigit = a! fromHere


-- update number based on portals
doPortals :: Int -> Calc -> Int
doPortals i c = 
    let portals = portals c
        si = reverse $ show i   -- we index from right to left
        lsi = length si
        arrI = listArray (0, lsi-1) si
     in foldl doPortal arrI (portals c)


-- In the future, we would write a doProperties function or something that used the Prop datatype, but we don't really need to so let's not.


-- TODO: Write an interactive solve that can produce all solutions, instead of just one.
-- TODO: Write an iterative-deepening depth-first-search as well, so we simply find all solutions given a certain depth.  Have it terminate at the shortest depth.
-- Interestingly, the problem space just lends itself to multiple solutions, making it actually quite quick to find solutions to problems.
solve :: Calc -> [Move]
solve c = reverse $ solve_help c [] (start c) (depth c) False

-- take problem, take past moves, take current state, take moves left, return future moves
-- this is a depth limited search - basically fix the first move and then explore the rest recursively.  
-- move order matters - if you put the only good move at the end of the movelist, the search will try all bad moves first.
-- what kind of heuristic would work here? can't be distance to number, because Concat, Back, Trans are all wildly changing.  maybe INVERSE distance to number?  kind of insane.
-- if stored, then don't append moves created through storage.
solve_help :: Calc -> [Move] -> Int -> Int -> Bool -> [Move]
solve_help c path i 0 stored = if test c i then path else []
solve_help c path i d stored =
    -- if test c i then path else   -- use this for exploration, not for solving.
    if invalid i 
       then [] 
       else 
       let nextStates = successor c i
           newCalcs = produceChanges c
           pathsPostStorage = 
               if isJust (storage c) && not stored 
                  then solve_help (writeMem i c) (Store : path) i (d-1) True 
                  else []
        in findShortestPath $ pathsPostStorage
        :  (map (\(nextMove, nextNum) -> solve_help c (nextMove : path) nextNum (d-1) False) nextStates)
        ++ (map (\(change, newCalc) -> solve_help newCalc ((changeToMove change) : path) i (d-1) False) newCalcs)

findShortestPath :: [[Move]] -> [Move]
findShortestPath = findShortestPathHelp (maxBound :: Int) []

findShortestPathHelp :: Int -> [Move] -> [[Move]] -> [Move]
findShortestPathHelp _ minpath [] = minpath
findShortestPathHelp len minpath ([]:t) = findShortestPathHelp len minpath t
findShortestPathHelp minlen minpath (h:t) = 
    let newlen = length h
     in if newlen < minlen
           then findShortestPathHelp newlen h t
           else findShortestPathHelp minlen minpath t

