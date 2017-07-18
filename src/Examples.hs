-- |This module contains several examples in order to verify the good
-- functioning of the other modules.
module Examples where

import Data.List(nub,iterate,partition)

import Math.CommutativeAlgebra.Polynomial
import Math.Core.Field (F2)
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.Algebras.TensorProduct
import Math.Core.Utils (toSet)


import qualified Data.Set as S

-------------------------------------------------------------------------

[x1,x2,x3,x4,x11] =
  map var ["x1","x2","x3","x4","x11"] :: [LexPoly F2 String]

example :: [LexPoly F2 String]
example = [1,x1*x2+x1+x2,x1*x2+x1+1,x1*x2+1,x1*x2+x2+1]

exampleSet :: S.Set (LexPoly F2 String)
exampleSet = S.fromList [1,x1*x2+x1+x2,x1*x2+x1+1,x1*x2+1,x1*x2+x2+1]

-- monx :: Lex String
-- --polx :: Vect F2 (Lex String)
-- monx = Lex (M 1 [("x1",1)])

-- ejMonomio = Lex (M 1 [("x1",1),("y2",2)])
-- ejMonomio2 = Lex (M 1 [("x1",2),("y2",2)])

-- ej1 :: LexPoly F2 String
-- --ej1 :: Vect F2 (Lex String)
-- ej1 = 3*x1*y2+x1^2+y2+z0

ej2 :: Vect F2 (Lex String)
-- --ej2 :: LexPoly F2 String
ej2 = x4*x2^4+x1+x1^34*x2^12

ej1 :: LexPoly F2 String
ej1 = x1*x2+x2*x3+x2+x4*x11+x11*x1*x2

-- ej3 :: LexPoly F2 String
ej3 :: LexPoly F2 String
ej3 = ej1^1001

-- | Ejemplo de grado
-- 
-- >>> deg ej3
-- 3003

-- ej4 :: LexPoly F2 String
-- ej4 = (ej1^1000) %% [x1^2+x1,y2^2+y2,z0^2+z0]

-------------------------------------------------------------------------

-- form2 [x,y]    = Conjunction (x) (Negation y)
-- form2 [x,y,z]  = Conjunction (x) (Negation y)
-- form2 (x:y:xs) = Disjunction (Conjunction (x) (Negation y)) (form2 (y:xs))

-- ejemploGrande :: Int -> [Expr]
-- ejemploGrande n = take n (map (\x -> (Variable (Var x))) (iterate (\x->x++"'") "p"))

-- > tool (ejemploGrande 100)
-- True
-- (0.01 secs, 1,236,880 bytes)
-- λ> tool (ejemploGrande 200)
-- True
-- (0.04 secs, 2,488,984 bytes)
-- λ> tool (ejemploGrande 300)
-- True
-- (0.15 secs, 4,302,304 bytes)
-- λ> tool (ejemploGrande 400)
-- True
-- (0.39 secs, 6,670,592 bytes)
-- λ> tool (ejemploGrande 500)
-- True
-- (0.86 secs, 9,603,336 bytes)
-- λ> tool (ejemploGrande 1000)
-- True
-- (8.91 secs, 33,546,520 bytes)

--ejemploGrande2 n = map form2 (map ejemploGrande [2..n])

--prueba = foldl (\y x -> ([form2Pol x] ++ y)) (ejemploGrande 2) [ej2] --([] :: [Vect F2 (Lex String)])



-- λ> (head . reverse . ejemploGrande2) 15
-- ((p ∧ ¬p') ∨ ((p' ∧ ¬p'') ∨ ((p'' ∧ ¬p''') ∨ ((p''' ∧ ¬p'''') ∨ ((p'''' ∧ ¬p''''') ∨ ((p''''' ∧ ¬p'''''') ∨ ((p'''''' ∧ ¬p''''''') ∨ ((p''''''' ∧ ¬p'''''''') ∨ ((p'''''''' ∧ ¬p''''''''') ∨ ((p''''''''' ∧ ¬p'''''''''') ∨ ((p'''''''''' ∧ ¬p''''''''''') ∨ ((p''''''''''' ∧ ¬p'''''''''''') ∨ (p'''''''''''' ∧ ¬p''''''''''''')))))))))))))
-- (0.01 secs, 1,018,192 bytes)
-- λ> (vars . form2Pol . head . reverse . ejemploGrande2) 15
-- [p,p',p'',p''',p'''',p''''',p'''''',p''''''',p'''''''',p''''''''',p'''''''''',p''''''''''',p'''''''''''',p''''''''''''']
-- (2.81 secs, 813,249,936 bytes)
