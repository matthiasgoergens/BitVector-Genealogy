module Main where

import Data.Array.Unboxed
import BitVector

a :: Array Int Int
a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])

b :: Array Int Bool
b = array (1,100) $ zip [1..100] $ repeat False

type AVertice = Array Int Bool
type AVertices = Array Int AVertice

-- (Array Int (Int,Int))



main = do return ()
          g <- annotMake n
          print . elems $ b
