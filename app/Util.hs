module Util where

import Data.Char (ord, chr)

toChr :: Int -> Char
toChr i = chr $ i + 48

toInt :: Char -> Int
toInt c = ord c - 48

