module Main where

import Data.List (sort)
import System.Random (randomRs, newStdGen)
import System.Environment (getArgs)

-- input orders: lower bound, upper bound, list length
main :: IO ()
main = do
  args <- getArgs
  gen <- newStdGen
  let lb = read (args !! 0)
  let ub = read (args !! 1)
  let len = read (args !! 2)
  let numberList = take len $ randomRs (lb, ub) gen
  let sLow = take 2 numberList
  let sHigh = take 3 numberList
  putStrLn $ show $ determine3Largest $ findLists sLow sHigh numberList
  let sortedNumberList = sort numberList
  putStrLn $ show $ (take 2 sortedNumberList :: [Int]) ++ drop (length sortedNumberList - 3) sortedNumberList

smallNumbers :: [Int] -> [Int]
smallNumbers = take 2 . sort

largeNumbers :: [Int] -> [Int]
largeNumbers = drop 1 . sort

findLists :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
findLists sns lns [] = (sns, lns)
findLists sns lns (x:xs)
  | x < 0     = findLists (smallNumbers (x:sns)) lns xs
  | x > 0     = findLists sns (largeNumbers (x:lns)) xs
  | otherwise = findLists sns lns xs

determine3Largest :: ([Int], [Int]) -> [Int]
determine3Largest (sns, lns) =
  if product sns > product (take 2 lns) then
    sns ++ (drop 2 $ lns) :: [Int]
  else
    lns
