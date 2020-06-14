{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'nonDivisibleSubset' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER k
--  2. INTEGER_ARRAY s
--
group :: [(a,a)] -> [(a,a)] -> [[a]]
group [] _ = [[]]
group [(a,b)] _ = [[a,b]]
group [(a,b),(a2,b2)] arr
    | (a,b2) `elem` arr && (
nonDivisibleSubset :: Int -> [Int] -> Int
nonDivisibleSubset k s = Data.List.length $ maximumBy (\a b -> compare $ (Data.List.length a) (Data.List.length b)) [ x | x<- s,y<-s,((x+y) `mod` k )/=0]

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    sTemp <- getLine

    let s = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip sTemp

    let result = nonDivisibleSubset k s

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
