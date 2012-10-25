{- Copyright 2012 Dustin DeWeese
   This file is part of pegc.

    pegc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    pegc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pegc.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module PegC.Graph where

import PegC.Tokenize
import PegC.Value
import Data.List
import Control.Monad
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Arrow (first)
import Data.Array
import Debug.Trace

arity (W "+") = (2,1)
arity (W "-") = (2,1)
arity (W "*") = (2,1)
arity (W "div") = (2,1)
arity (W "dup") = (1,2)
arity (W "pop") = (1,0)
arity (W "swap") = (2,2)
arity (W "\\/") = (2,1)
arity (W "!") = (1,1)
arity (W ".") = (2,1)
arity (W "popr") = (1,2)
arity (W "pushr") = (2,1)
arity (W "quot") = (1,1)
arity (R _) = (1,1)
arity _ = (0,1)

composeArity (ix, ox) (iy, oy) = (ix + max 0 (-h), oy + max 0 h) 
  where h = ox - iy
arityM = foldl' (\s -> composeArity s . arity) (0,0)

push = do (a, s, c) <- get
          put (a, c:s, c+1)
          return c
pushI x = do (a, s, c) <- get
             put (a, x:s, c)
             return x
pop = do (a, x:s, c) <- get
         put (a, s, c)
         return x
swap = do (a, x:y:s, c) <- get
          put (a, y:x:s, c)
          return [y,x]
dup = do (a, x:s, c) <- get
         put (a, x:x:s, c)
         return [x,x]

regCnt (L xs) = fst (arityM xs) + sum (map regCnt xs) + 1
regCnt (W "swap") = 0
regCnt (W "dup") = 0
regCnt (W "pop") = 0
regCnt (W "popr") = 1
regCnt x = snd (arity x)

tellA x = modify (\(a, s, c) -> (a // x, s, c))
withA f = do
  (a, s, c) <- get
  return $ f `runReader` a
getRef x = do
  (a, _, _) <- get
  return (a!x)

ast (L xs) = do
    i <- replicateM ic push
    mapM_ ast xs
    o <- replicateM oc pop
    r <- push
    tellA [(r, (0, Q (AST i o), []))]
    return [r]
  where (ic, oc) = arityM xs
ast (W "swap") = swap
ast (W "dup") = dup
ast (W "pop") = (:[]) `liftM` pop
ast (W "quot") = do
  x <- pop
  r <- push
  tellA [(r, (0, Q (AST [] [x]), []))]
  return [r]
ast (W "popr") = do
  r <- pop
  (_,Q (AST i (x:o)),_) <- getRef r
  y <- push
  tellA [(y, (0, Q (AST i o),[]))]
  pushI x
  return [x,y]
ast (W ".") = do
  y <- pop
  x <- pop
  (_, Q astX, _) <- getRef x
  (_, Q astY, _) <- getRef y
  astZ <- composeAst astX astY
  z <- push
  tellA [(z, (0, Q astZ, []))]
  return [z]
ast x = do
    i <- replicateM ic pop
    o <- replicateM oc push
    tellA [(r, (n, x, i)) | (r,n) <- zip o [0..]]
    return o
  where (ic, oc) = arity x

composeAst :: (MonadState (Array Int (Int, Value, [Int]), [Int], Int) m) => AST -> AST -> m AST
composeAst (AST ix ox) (AST iy oy) = do
  tellA . map (\(o, i) -> (i, (0, R o, []))) $ zip ox iy
  return (AST (drop (length ox) iy ++ ix) (oy ++ drop (length iy) ox))

buildAst :: [Value] -> Array Int (Int, Value, [Int])
buildAst xs = a
  where (a, s, c) = flip execState (array (0, regCnt (L xs) - 1) [], [], 0) . ast . L $ xs

exec (0, W "+", i) = do
  [I x, I y] <- mapM force i
  return . I $ x + y
exec (0, R x, []) = force x
exec (0, x, []) = return x

force x = exec . (!x) =<< ask  

eval a = mapM force o `runReader` a
  where (0, c) = bounds a
        (0, Q (AST [] o), []) = a!c

run xs = (eval . buildAst) `liftM` tok xs

tok x = case tokenize x of
  Left _ -> error "failed to tokenize"
  Right x -> return x
{-
sets ss = S.fromList [ x `S.intersection` y | (x:xs) <- init . tails . S.toList $ ss, y <- xs]
allSets ss | ss' `S.isSubsetOf` ss = ss
           | otherwise = ss `S.union` allSets ss'
  where ss' = sets ss
-}

seperate [s] = [s]
seperate ss = filter (\x -> not $ any (S.isProperSubsetOf x) ss') ss'
  where ss' = concat [[x `S.difference` y, x `S.intersection` y, y `S.difference` x] | (x:xs) <- init . tails $ ss, y <- xs]

deps a x = deps' [x] S.empty
  where deps' [] s = s
        deps' (x:xs) s = deps' (c ++ xs) (x `S.insert` s)
          where (_,_,c) = a!x
                
{-
deps a x = deps' [x] S.empty []
  where deps' [] s [] = [s]
        deps' [] s (y:ys) = s : deps' [y] S.empty ys
        deps' (x:xs) s ys = case w of
                                 W "dup" -> deps' xs s (x:ys) -- defer
                                 _ -> deps' (c ++ xs) (x `S.insert` s) ys
          where (_,w,c) = a!x
-}

outDeps a = map (deps a) s
  where (_, Q (AST _ s), _) = a ! snd (bounds a)

units = seperate . outDeps

{-
varName x = "var" ++ show x

gen (W "+") _ b = do [x, y] <- b
                     (m, c, i) <- get
                     tell [varName c ++ " = " ++ varName x ++ " + " ++ varName y]
                     put (m, c+1, i)
                     return c
gen (I x) _ _ = do (m, c, i) <- get
                   tell [varName c ++ " = " ++ show x]
                   put (m, c+1, i)
                   return c
gen (R n) _ b = do (m, _, _) <- get
                   case M.lookup n m of
                     Nothing -> do [x] <- b
                                   modify (\(m, c, i) -> (M.insert n x m, c, i))
                                   return x
                     Just x -> return x
gen (In 0) x _ = do (m, c, i) <- get
                    put (m, c+1, (x,c):i)
                    return c
gen (W w) x b = do cs <- b
                   (m, c, i) <- get 
                   tell [varName c ++ " = " ++ w ++ "(" ++ commas (map varName cs) ++ ")"]
                   put (m, c+1, i)
                   return c

commas = concat . intersperse ", "

generate src = do
  AST a r <- comp' src
  flip mapM a $ \x -> do
    ((o, (_, n, i)), s) <- runWriterT . flip runStateT (M.empty, 0, []) $ mapASTM gen x
    return (i, r, o, n, s)

genFunc n x = (map (\(i, f) -> func (n ++ "_" ++ show i) f) . zip [0..]) `liftM` generate x

outName n = "out" ++ show n
inName n = "in" ++ show n

proto name r = "void " ++ name ++ "( " ++ commas ["int in" ++ show x | x <- [0..r-1]] ++ " )"
func name (i, r, o, n, s) = proto name r ++ "\n{\n" ++
                            declare ++
                            copyIn ++
                            concatMap (("  "++).(++ ";\n")) s ++
                            copyOut ++
                            "}\n"
  where declare | n - length i > 0 = "  int " ++ commas [varName x | x <- [0..n-1]] ++ ";\n"
                | otherwise = ""
        copyIn = concatMap (\(x, y) -> "  " ++ varName y ++ " = in" ++ show x ++ ";\n") i
        copyOut = "  return " ++ varName o ++ ";\n"
-}