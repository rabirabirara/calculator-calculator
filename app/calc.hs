
-- A Calc is an instance of a problem, with one start, one goal, one depth, and one set of buttons/properties.
-- For multiple goals, just create multiple Calcs.
-- For multiple depths,

module Calc where --(Calc (..), Change (..), Storage (..), Portal (..), solve, iddfs, iddfsAll) where

import Util
import Move
import Data.Maybe (isJust, fromMaybe)
import Debug.Trace

-- record syntax - for product types with named type constructors
data Calc = Calc 
    { start   :: Int
    , goal    :: Int
    , depth   :: Int
    , moves   :: [Move]
    , changes :: [Change]   -- for buttons that alter moves
    , storage :: Maybe Storage  -- TODO: Maybe and List are both monads, replace maybe with the list monad in future.  Multiple stores is definitely feasible.
    , portal  :: Maybe Portal   -- in the future, who knows? maybe we control the portals instead of having them be automatic.
    } deriving Show

-- portals are indices going from right to left, starting from 0.
-- data Prop = PropPortal Portal

type Portal = (Int, Int)

type Storage = Maybe Int


-- Even if it's points-free, you still have to include everything in the signature.
-- get results of applying moves, as well as factoring in portals.
successor :: Int -> Calc -> [(Move, Int)]
successor i c = let nextStates = doMoves i (moves c)
                 in fromMaybe nextStates (applyPortals nextStates c)    -- if portals not applied, just give nextstates; else apply portals

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
             , portal  = portal c
             }

incMove :: Int -> Move -> Move
incMove i mv =
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
                    , portal = portal c
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



-- incremental update: remove an item at index i from lst and return it and the new list
popAt :: Int -> [a] -> ([a], a)
popAt i lst = popAtHelp i lst []

popAtHelp :: Int -> [a] -> [a] -> ([a], a)
popAtHelp _ [] newlst = undefined   -- out of bounds error
popAtHelp 0 (h:t) newlst = ((reverse newlst) ++ t, h)
popAtHelp i (h:t) newlst = popAtHelp (i-1) t (h : newlst)


willPortal :: Int -> Int -> Bool
willPortal i fromHere = i >= pow10 || i <= (negate pow10)
    where pow10 = 10 ^ fromHere

-- do a single portal, potentially recurse. also, this will still work even if there are digits beyond fromHere, because we are removing the digits from the list.
doPortal :: Int -> Portal -> Int
doPortal i (fromHere, toHere) =
    let (rsi, dc) = popAt fromHere (reverse $ show i)   -- * reverse si because we index from right to left
        (ri, d) = (read $ reverse rsi, toInt dc)
        addThis = d * (10 ^ toHere)
        newi = ri + addThis
     in if willPortal newi fromHere  -- if the number has enough digits to portal
           then doPortal newi (fromHere, toHere)
           else newi

-- update number based on portal, if the number will portal
doPortals :: Int -> Calc -> Portal -> Int
doPortals i c (from, to) = 
    if willPortal i from 
       then doPortal i (from, to)
       else i

-- take successor states and maybe apply portals with your calc
applyPortals :: [(Move, Int)] -> Calc -> Maybe [(Move, Int)]
applyPortals nexts c =
    case portal c of
      Just (from, to) -> Just $ map (\(move, i) -> (move, doPortals i c (from, to))) nexts
      Nothing -> Nothing

-- In the future, we would write a doProperties function or something that used the Prop datatype, but we don't really need to so let's not.

-- findAll :: Calc -> [[Move]]
-- findAll finds all paths given a calc, of all depths.


-- apply iddfs to every calc in a list
iddfsAll :: [Calc] -> [[[Move]]]
iddfsAll cs = map iddfs cs

-- Given a calc, run iterative-deepening depth-first-search.
-- Rather than finding all paths, it simply finds ONE path from each depth of moves.
iddfs :: Calc -> [[Move]]
iddfs c = map (\d -> reverse $ solve_help c [] (start c) d False) [1..(depth c)]


-- TODO: Write an interactive solve that can produce all solutions, instead of just one.
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
    -- if test c i then path else   -- premature cutoff.  the game accepts premature solves, so this works.  This isn't needed since we do IDDFS and thus our search is optimal.
    if invalid i 
       then [] 
       else 
       let nextStates = successor i c
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

