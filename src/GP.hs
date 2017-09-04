{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GP where

import GenProg
import Data.Generics
import Control.Monad (liftM, liftM2)
import Control.Monad.Random
import GenProg.GenExpr.Data
import Text.Printf (printf)

data E = Plus E E
       | Minus E E
       | Times E E
       | Div E E
       | Const Int
       deriving (Typeable,Data,Eq)

instance Show E where
 show (Plus l r) = printf "(%s + %s)" (show l) (show r)
 show (Minus l r) = printf "(%s + %s)" (show l) (show r)
 show (Times l r) = printf "%s * %s" (show l) (show r)
 show (Div l r) = printf "%s / %s" (show l) (show r)
 show (Const c) = show c


eval :: E -> Maybe Int
eval (Const c)     = Just c
eval (Plus e1 e2)  = liftM2 (+) (eval e1) (eval e2)
eval (Minus e1 e2) = liftM2 (-) (eval e1) (eval e2)
eval (Times e1 e2) = liftM2 (*) (eval e1) (eval e2)
eval (Div e1 e2) | ok        = liftM2 div x1 x2
                 | otherwise = Nothing
  where (x1,x2) = (eval e1,eval e2)
        ok = x2 /= Just 0 && liftM2 mod x1 x2 == Just 0

instance GenProg (Rand StdGen) E where
  terminal    = Const `liftM` getRandomR (1, 9)
  nonterminal = do
    r <- getRandomR (0,3)
    [liftM2 Plus terminal terminal,
     liftM2 Minus terminal terminal,
     liftM2 Times terminal terminal,
     liftM2 Div terminal terminal] !! r


myFitness :: Int -> E -> Double
myFitness n e = error + size
  where
    error = realToFrac $ maybe maxBound (abs . (n -)) (eval e)
    size = realToFrac (nodes e) / 50
