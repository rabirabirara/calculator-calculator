-- The module can choose functions/types to export by placing them and their constructors in the module statement.
-- TODO: Implement the password solving feature in levels of multiple 20.
module Main (main) where

-- We also choose what to import with lists in parentheses.
import Util
import Move
import Calc
import Data.Char (isSpace)
import Data.Maybe (mapMaybe, isJust, fromMaybe, listToMaybe)
import Data.List (find)
import Data.List.Split (splitOn)
import Debug.Trace


-- removes whitespace from a string, inside and outside
removeWhitespace :: String -> String
removeWhitespace = (filter (not . isSpace))


-- read and show are for String -> a and a -> String
-- so for case statements haskell considers indentation of _, so don't space it out of line with the other cases.
parseMove :: String -> Move
parseMove [] = undefined
parseMove (mc:num) = 
    case mc of
      'a' -> Add (read num) 
      's' -> Sub (read num) 
      'm' -> Mul (read num) 
      'd' -> Div (read num) 
      'e' -> Exp (read num) 
      'f' -> Flip
      '+' -> Sum
      'r' -> Rev
      'R' -> Round 0
      'b' -> Back
      'B' -> Delete 0
      '^' -> Change num
      '|' -> Mirror
      'c' -> Concat num
      'C' -> Insert num 0
      '/' -> Filter (head num)
      '#' -> Store
      '@' -> MemCon num
      'i' -> Inv10
      'h' -> case (head num) of
               'l' -> Shift L
               'r' -> Shift R
               _   -> undefined
      '=' -> case (head num) of
               'l' -> Sort L
               'r' -> Sort R
               _   -> undefined
      't' -> let args = splitOn ">" num 
              in Trans (head args) (head $ tail args)
      _   -> undefined

parseChange :: String -> Maybe Change
parseChange [] = Nothing
parseChange (mc:num) = 
    case mc of
      '^' -> Just (Inc (read num))
      _   -> Nothing

parseStore :: String -> Maybe Storage
parseStore (mc:num) =
    case mc of
      '@' -> Just (Just (read num))
      '#' -> Just Nothing
      _   -> Nothing


-- don't add Change moves to the normal Move list
-- the Change type constructors in Move are for display only
parseMoves :: String -> ([Move], [Change], Maybe Storage)
parseMoves mstr = 
    let buttons = (splitOn ",") . removeWhitespace $ mstr
        moves = map parseMove buttons
        changes = mapMaybe parseChange buttons
        -- remember, we only expect to have one Store button.  if there are more... TODO
        storage = fromMaybe Nothing $ find isJust $ map parseStore buttons
     in (moves, changes, storage)


parsePortal :: String -> Maybe Portal
parsePortal [] = Nothing
parsePortal (pc:num) =
    case pc of
      'p' -> let args = splitOn ">" num
              in Just (((read :: String -> Int) $ head args), ((read :: String -> Int) $ head $ tail args))
      _   -> Nothing


parseProperties :: String -> [Portal]
parseProperties pstr =
    let props = splitOn "," pstr
        portals = mapMaybe parsePortal props
        -- only one portal. if more: TODO  we could easily support a list of them, of course, though I doubt it would work in gameplay.
        -- refer to this portal parsing implementation when supporting several store buttons.
     in portals

readInt :: IO Int
readInt = readLn

parseGoals :: String -> [Int]
parseGoals s = (map read) . (splitOn ",") . removeWhitespace $ s

-- = vs. <-: https://stackoverflow.com/a/28625714/13553596
-- also, use trace to print debug statements (not print)
-- also, use putStrLn to print Strings, or you'll get double quotes (from show)
-- also, use getLine and getChar to read Strings and Chars, so you don't need quotes.
main :: IO ()
main = do
    putStrLn "Enter starting number:"
    start <- readInt
    putStrLn "Enter goals:"
    gs <- getLine
    putStrLn "Enter depth (number of moves):"
    depth <- readInt
    putStrLn "Enter buttons.  See README for button specification."
    ms <- getLine
    putStrLn "Enter calculator properties (e.g. portals)."
    ps <- getLine
    let (moves, changes, storage) = parseMoves ms
        goals = parseGoals gs
        portals  = parseProperties ps
        problems = map (\g ->
            Calc { start   = start
                 , goal    = g
                 , depth   = depth
                 , moves   = moves
                 , changes = changes
                 , storage = storage
                 , portal  = listToMaybe portals    -- again, just the one portal.
                 }
                       ) goals
        solves = iddfsAll problems
     in printAllSolves goals solves
    putStrLn "\nDo you want to continue? (Y/n)"
    continue <- getChar
    if continue == 'y' || continue == '\n'
       then main
       else return ()     
       -- 'pure' is a Monad function that wraps its argument in the proper monad. aka 'return'. use to return from IO ()


-- takes a list of goals, a list of solves for a set of depths, pretty prints them
printAllSolves :: [Int] -> [[[Move]]] -> IO ()
printAllSolves gs pss = mapM_ printIddfsSolve (zip gs pss)

-- pretty print iddfs result
printIddfsSolve :: (Int, [[Move]]) -> IO ()
printIddfsSolve (g, ps) = do
    putStrLn $ (show g) ++ " :: "
    mapM_ printSolve (zip [1..(length ps)] ps)

-- pretty print a solution at depth d
printSolve :: (Int, [Move]) -> IO ()
printSolve (d,p) = putStrLn $ (show d) ++ ": " ++ (show p)

-- debugging: https://www.reddit.com/r/haskell/comments/oqb1g/using_print_statements_in_code_for_debugging/
-- also, use the ghci debugger.  with cabal, the command would be: `cabal repl --repl-options=-prof`. then :? when in ghci to look around.

{- NOTES 
So, we know that the order that the moves are given in matters slightly.  We also know that we need a heuristic for choosing the order of moves in doMoves.
How do we solve these two problems?  That is, how can we evaluate how useful a move is?
Perhaps in move simplicity?  For example, the entropy of flipping the sign of a number is just 1 bit.  You flip it once, and flipping it twice is useless.
Backspace has a little more entropy, but less than concat because it can't occur twice.
Frankly looks like a hard problem, but who knows, maybe some emergent behavior will arise.  Machine learning application??

-Honestly seems like a heuristic for this is as hard as solving the Collatz conjecture.

-Actually, there may yet be a heuristic.  For example, fail-fast is possible - moves such as Mirror are bound to fail-fast, and Trans is sometimes impossible.


--Interesting math problem: given a Calc, i.e. a set of buttons (no properties) and a starting number, what are all reachable numbers within the given moves?  With no move limit?  (i.e. find the entire range of the calc from the start)  With any start n?
As I see it this is only possible through computer search, the same way we can't tell why all numbers reach 1 per the Collatz conjecture.

-}
