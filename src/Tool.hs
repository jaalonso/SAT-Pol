-- | This module aims to provide an efficient tool to solve the SAT
-- problem. Both the algorithm and the implementation have been carried out by
-- the researchers from the department of Computer Science and Artificial
-- Intelligence at the University of Seville.

module Tool
    ( tool
      , varsList
    ) where

import Data.List(nub,iterate,partition, foldl', union)

import PolAux
import Examples

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Utils (toSet)

import qualified Data.Set as Set

-------------------------------------------------------------------------------

-- | varsList xs return the set of variables which occurs in any polynomial of
-- the list xs. For example, the trivial case:
-- 
-- >>> let xs = (take 10 (map (var . (\n -> 'x':n) . show) [1..])) :: [LexPoly F2 String]
-- >>> xs
-- [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10]
-- >>> varsList xs
-- [x1,x2,x3,x4,x5,x6,x7,x8,x9,x10]
-- >>> varsList [x1*x2+x2,x1+x1*x3,x1*x4*x2]
-- [x1,x2,x3,x4]
varsList :: (Foldable t
            , Num k
            , MonomialConstructor m
            , Ord (m v)
            , Ord k) =>
            t (Vect k (m v)) -> [Vect k (m v)]
varsList xs = nub (concatMap vars xs)

varsList2 :: (Foldable t
            , Num k
            , MonomialConstructor m
            , Ord (m v)
            , Ord k) =>
            t (Vect k (m v)) -> [Vect k (m v)]
varsList2 xs = nub $ foldl' (\acc x -> (vars x)++acc) [] xs

-------------------------------------------------------------------------------

-- | The function (deltaRule p x y) performs the independence rule described in
-- the paper [?]. It's important to note that p is the variable from wich we
-- derive and the one we would drop. For example:
-- 
-- >>> deltaRule (x1:: LexPoly F2 String) (1:: LexPoly F2 String) (1:: LexPoly F2 String)
-- 1
-- >>> deltaRule (x1:: LexPoly F2 String) (1:: LexPoly F2 String) (0:: LexPoly F2 String)
-- 0
-- >>> deltaRule (x1:: LexPoly F2 String) (x1:: LexPoly F2 String) (x1:: LexPoly F2 String)
-- 1
deltaRule :: (Eq k, Eq u, Num k, Ord (m u), Algebra k (m u),
              MonomialConstructor m, Show (m u), Show u) =>
             Vect k (m u) -> Vect k (m u) -> Vect k (m u) -> Vect k (m u)
deltaRule p a1 a2 = clean (1 + (1+a1*a2)*(1+a1*da2 + a2*da1 + da1*da2))
  where da1 = deriv a1 p
        da2 = deriv a2 p

-------------------------------------------------------------------------------

-- | deltaRuleList1Step apply deltaRule from p between every polynomial in the
-- first list and store the results in the accumulator (second list). For
-- example:
-- 
-- >>> deltaRuleList1Step (x1:: LexPoly F2 String) ([x1]:: [LexPoly F2 String]) ([1]::[LexPoly F2 String]) 
-- [1,1]
-- >>> deltaRuleList1Step (x1:: LexPoly F2 String) ([x1,x1*x2,x1*x3]:: [LexPoly F2 String]) ([]::[LexPoly F2 String]) 
-- [x3,x2x3,x2,x3,x2,1]
deltaRuleList1Step ::
  (Eq k
  , Eq u
  , Num k, Ord k
  , Ord (m u)
  , Algebra k (m u)
  , MonomialConstructor m
  , Show (m u), Show u) =>
  Vect k (m u) -> [Vect k (m u)] -> [Vect k (m u)] -> [Vect k (m u)]
deltaRuleList1Step _  [] xs    = xs
deltaRuleList1Step p (x:xs) ys =
 deltaRuleList1Step p xs (foldl' (\acc y -> ((deltaRule p x y):acc)) ys (x:xs))

-------------------------------------------------------------------------------
                                   
-- |toolAux check if in any step of the algorithm a zero is obtained. In this
-- case, the original set of formulas was unsatisfiable and the tool answer
-- would be "False". Otherwise, the set of polynomials is divided in two subsets,
-- one contains those polynomials in which occurs the variable p, while the
-- other store the rest.

-- We should think if there exists any way to use the lazy power in the search
-- of zeros.


toolAux :: (Eq k
           , Ord k
           , Eq u
           , Show (m u)
           , Show u
           , MonomialConstructor m
           , Algebra k (m u)
           , Ord (m u)
           , Num k) =>
           [Vect k (m u)] -> [Vect k (m u)] -> Bool
toolAux [] xs     = if (elem 0 xs) then False else True
toolAux (p:ps) xs = if (elem 0 ys) then False else (toolAux ps ys)
  where (xs1,xs2) = partition (\x -> elem p (vars x)) xs
        ys = nub (deltaRuleList1Step p xs1 xs2)

-------------------------------------------------------------------------------

-- |tool decides if the set of formulas that produced the set of polynomials
-- were satisfiables. The function input is a list of polynomials because the
-- transformation from formula to polynomial is handled by ReadingF.hs module.

tool :: (Eq u
        , Show (m u)
        , Show u
        , MonomialConstructor m
        , Algebra k (m u)
        , Ord (m u)
        , Ord k
        , Num k) =>
        [Vect k (m u)] -> Bool
tool xs = toolAux (varsList xs) xs

-------------------------------------------------------------------------------



