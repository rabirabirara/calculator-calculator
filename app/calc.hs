-- TODO: Big flaw where start is larger than goal and we have Div move. in which case we will integer divide and never produce the right answer.  Need to write constraint with Move Div where you cannot produce a remainder.
module Calc (Calc (..), solve) where

import Move
-- import Debug.Trace

-- record syntax - for product types with named type constructors
data Calc = Calc 
    { start :: Int
    , goal  :: Int
    , depth :: Int
    , moves :: [Move]
    } deriving Show

-- Even if it's points-free, you still have to include everything in the signature.
successor :: Calc -> (Int -> [(Move, Int)])
successor c = ((flip doMoves) (moves c))

test :: Calc -> (Int -> Bool)
test c = (== (goal c))

invalid :: Int -> Bool
invalid i = i > 999999 || i < -99999

-- TODO: Write an interactive solve that can produce all solutions, instead of just one.
-- TODO: Write an iterative-deepening depth-first-search as well, so we simply find all solutions given a certain depth.  Have it terminate at the shortest depth.
-- Interestingly, the problem space just lends itself to multiple solutions, making it actually quite quick to find solutions to problems.
solve :: Calc -> [Move]
solve c = reverse $ solve_help c [] (start c) (depth c)

-- take problem, take past moves, take current state, take moves left, return future moves
-- this is a depth limited search - basically fix the first move and then explore the rest recursively.  move order matters somewhat, then - if you put the right move at the end of the moves list, this search will try all the wrong moves first.
-- what kind of heuristic would work here? can't be distance to number, because Concat, Back, Trans are all wildly changing.  maybe INVERSE distance to number?  kind of insane.
solve_help :: Calc -> [Move] -> Int -> Int -> [Move]
solve_help c path i 0 = 
    if test c i then path else []
solve_help c path i d =
    -- if test c i then path else   -- use this for exploration, not for solving.
    if invalid i 
       then [] 
       else let nextStates = successor c i
             in findShortestPath $ map (\(nextMove, nextNum) -> solve_help c (nextMove : path) nextNum (d-1)) nextStates

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

