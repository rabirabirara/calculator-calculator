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
    }

-- Even if it's points-free, you still have to include everything in the signature.
successor :: Calc -> (Int -> [(Move, Int)])
successor c = ((flip doMoves) (moves c))

test :: Calc -> (Int -> Bool)
test c = (== (goal c))

solve :: Calc -> [Move]
solve c = reverse $ solve_help c [] (start c) (depth c)

-- take problem, take past moves, take current state, take moves left, return future moves
-- this is a depth first search - basically fix the first move and then explore the rest recursively.  move order matters somewhat, then - if you put the right move at the end of the moves list, this search will try all the wrong moves first.
-- what kind of heuristic would work here? can't be distance to number, because Conc, Back, Trans are all wildly changing.  maybe INVERSE distance to number?  kind of insane.
solve_help :: Calc -> [Move] -> Int -> Int -> [Move]
solve_help c path i 0 = 
    if test c i then path else []
solve_help c path i d =
    -- if test c i then path else   -- use this for exploration, not for solving.
    let nextStates = successor c i
     in findShortestSolution (maxBound :: Int) [] $ map (\(nextMove, nextNum) -> solve_help c (nextMove : path) nextNum (d-1)) nextStates

findShortestSolution :: Int -> [Move] -> [[Move]] -> [Move]
findShortestSolution _ minpath [] = minpath
findShortestSolution len minpath ([]:t) = findShortestSolution len minpath t
findShortestSolution minlen minpath (h:t) = 
    let newlen = length h
     in if newlen < minlen
           then findShortestSolution newlen h t
           else findShortestSolution minlen minpath t

