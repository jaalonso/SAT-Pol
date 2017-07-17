{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module ReadingF where

import Data.List
import Data.Char (isSpace)
import Data.Foldable (sum, product)
import System.Environment

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Utils (toSet)

import PolAux
import Tool (tool)
import ToolS (toolS)

import qualified Data.Set as S

-------------------------------------------------------------------------------

main1 f = do
  s <- readFile f
  print $ nub $ (foldr (\x acc -> (((clean . varFold . words) x):acc)) []) $ lines $ s

-------------------------------------------------------------------------------

-- main2 "/Users/danielrodriguezchavarria/Desktop/300/ReadingFiles/fil3.txt"

main2 f = do
  s <- readFile f
  print $ tool $ nub $ (foldr (\x acc -> (((clean . varFold . words) x):acc)) []) $ lines $ s 

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

main3 f = do
  s <- readFile f
  print $ (foldr (\x acc -> (S.insert ((clean . varFold . words) x) acc)) S.empty) $ lines $ s

-------------------------------------------------------------------------------

-- main2 "/Users/danielrodriguezchavarria/Desktop/300/ReadingFiles/fil3.txt"

main4 f = do
  s <- readFile f
  print $ toolS $ (foldr (\x acc -> (S.insert ((clean . varFold . words) x) acc)) S.empty) $ lines $ s 

-------------------------------------------------------------------------------
  
-- Si es FND usar:

-- divide :: [String] -> [[String]]
-- divide [] = [[]]
-- divide ((x:xs):xss) | x == '-'  = map (xs:) yss ++ yss
--                     | otherwise = map ((x:xs):) yss
--   where yss = divide xss

-------------------------------------------------------------------------------

-- Si es FNC usar:

varFold :: [String] -> Vect F2 (Lex String)
varFold (x:xs) | x == "c" || x == "p" = 1
varFold xs = foldl' (\acc x -> disj (var' x) acc) zerov xs

-------------------------------------------------------------------------------

var' "0"      = zerov
var' ('-':xs) = 1 + var ('x':xs)
var' x        = var ('x':x)

-------------------------------------------------------------------------------

disj :: Num a => a -> a -> a
disj x y = x + y + x*y

-------------------------------------------------------------------------------
