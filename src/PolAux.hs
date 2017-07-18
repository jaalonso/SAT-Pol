{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module PolAux
    ( clean,
      deriv,      
    ) where

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Utils (toSet)

import Examples

-------------------------------------------------------------------------------

-- | bind2 performs the clean function.
bind2 :: (Eq k
         , Num k
         , Ord a
         , Show a
         , Algebra k a
         , MonomialConstructor m) =>
         Vect k (m v)  -> (v -> Vect k a) -> Vect k a
v `bind2` f = linear (\m -> product [ f x | (x,i) <- mindices m]) v

-------------------------------------------------------------------------------

-- | clean aims to select the simplest representative of a polynomial in the
-- quotient group F2[x_1,...,x_N]/(x_1+x_1^2,...,x_N+x_N^2). The main idea is
-- to replace every ocurrence of x_i^M with x_i thus we obtain an identical
-- polynomial without exponents greater than 1.
--
-- In the library HaskellForMaths exists a function that performs the
-- same (%%) so we can check the results. For example,
--
-- >>> clean (x1^3:: LexPoly F2 String)
-- x1
-- >>> (x1^3:: LexPoly F2 String) %% [x1^2+x1]
-- x1
-- >>> clean (x1^3*x2^6+x3^2*x4+x1+1 :: LexPoly F2 String)
-- x1x2+x1+x3x4+1
-- >>> let pol = x1^3*x2^6+x3^2*x4+x1+1 :: LexPoly F2 String
-- >>> let list = [x1^2+x1,x2^2+x2,x3^2+x3,x4^2+x4] 
-- >>> pol %% list
-- x1x2+x1+x3x4+1
clean :: (Eq k
         , Num k
         , Eq (m u)
         , Show u
         , Ord (m u)
         , Show (m u)
         , Algebra k (m u)
         , MonomialConstructor m) =>
         Vect k (m u) -> Vect k (m u)
clean f = f `bind2` var

-------------------------------------------------------------------------------

-- | bind3 performs the deriv function.
bind3 :: (Eq v
         , Eq k
         , Num k
         , Ord (m2 v)
         , Show (m2 v)
         , Algebra k (m2 v)
         , MonomialConstructor m
         , MonomialConstructor m1
         , MonomialConstructor m2) =>
         m1 v -> Vect b (m v) -> Vect k (m2 v)
bind3 m v 
  | varDif `elem` mIndices =
      product [var x ^ i | (x,i) <- mIndices, x /= fst varDif]
  | otherwise =  0
  where mIndices = mindices m
        varDif = head (mindices (lm v))

-- | deriv calculates the derivative of the polynomial f with respect to the
-- variable v. It's important to note that we use the fact that we are in the
-- quotient group described above. For example:
--
-- >>> let v = x1:: LexPoly F2 String
-- >>> deriv v v
-- 1
-- >>> deriv (1+x1+x2+x1*x2:: LexPoly F2 String) v
-- x2+1
-- >>> deriv (x1*x2+x1+x3*x4+1:: LexPoly F2 String) v
-- x2+1
deriv :: (Eq k
         , Eq u
         , Num k
         , Ord (m u)
         , Show (m u)
         , Algebra k (m u)
         , MonomialConstructor m) =>
         Vect k (m u) -> Vect k (m u) -> Vect k (m u)
deriv f v = linear (`bind3` v) f
