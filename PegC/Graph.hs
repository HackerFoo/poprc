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
import Data.IntMap (IntMap, (!))
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
arity (W "id") = (1,1)
arity (R _) = (1,1)
arity _ = (0,1)

composeArity (ix, ox) (iy, oy) = (ix + max 0 (-h), oy + max 0 h) 
  where h = ox - iy
arityM = foldl' (\s -> composeArity s . arity) (0,0)

push = do (a, s, c) <- get
          put (a, c:s, c+1)
          return c
newRefs n = do (a, s, c) <- get
               put (a, s, c+n)
               return c
pushI x = do (a, s, c) <- get
             put (a, x:s, c)
             return x  
pop = do
  (a, x:s, c) <- get
  put (a, s, c)
  return x

regCntM xs = fst (arityM xs) + sum (map regCnt xs)
regCnt (W "swap") = 0
regCnt (W "dup") = 0
regCnt (W "pop") = 0
--regCnt (W "\\/") = 0
regCnt (W "popr") = 1
regCnt x = snd (arity x)


tellA x = modify (\(a, s, c) -> (a `M.union` M.fromList x, s, c))
withA f = do
  (a, s, c) <- get
  return $ f `runReader` a
getRef x = do
  (a, _, _) <- get
  return (a!x)
  
-- initial pass
swizzle (W "swap") = do
  (a, x:y:s, c) <- get
  put (a, y:x:s, c)
swizzle (W "dup") = do
  (a, x:s, c) <- get
  put (a, x:x:s, c)
swizzle (W "pop") = pop >> return ()
swizzle (W "quot") = do
  x <- pop
  r <- push
  tellA [(r, (0, Q $ AST (M.fromList [(0, (0, R x, []))]) [] [0], [x]))]
swizzle (W "popr") = do  
  x <- pop
  (0, Q a, []) <- getRef x
  rt <- push
  let (h, t) = splitAst (rt+1) a
  newRefs $ M.size h - 1
  tellA $ (rt, (0, Q t, [])) : M.toList h
  push
  return ()
swizzle (L xs) = do
  o <- push
  tellA [(o, (0, Q (buildAst xs), []))]
swizzle x = do
    i <- replicateM ic pop
    o <- replicateM oc push
    tellA [(r, (n, x, i)) | (r,n) <- zip o [0..]]
  where (ic, oc) = arity x

transplant o m = M.fromList . zip [o..] . map f . M.elems $ m
  where nm = M.fromList $ zip (M.keys m) [o..]
        f (0, R r, []) = (0, W "id", [r])
        f (n, w, cs) = (n, w, map (nm!) cs)

splitAst x (AST a i (o:os)) = (transplant x h, AST t i os)
  where od = deps a [o]
        (h, t) = M.partitionWithKey (\k _ -> k `S.member` od) a

-- when composing, ast is spliced as soon as depencies are met
-- ast represented as graphs missing dependencies + registers of outputs + registers of refs + input count

{-
ast (L xs) = do
    i <- replicateM ic push
    mapM_ ast xs
    o <- replicateM oc pop
    r <- push
    tellA [(r, (0, Q (AST i o), []))]
    return [r]
  where (ic, oc) = arityM xs
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
composeAst :: (MonadState (IntMap Int (Int, Value, [Int]), [Int], Int) m) => AST -> AST -> m AST
composeAst (AST ix ox) (AST iy oy) = do
  tellA . map (\(o, i) -> (i, (0, R o, []))) $ zip ox iy
  return (AST (drop (length ox) iy ++ ix) (oy ++ drop (length iy) ox))
-}
buildAst :: [Value] -> AST --([Int], [Int], IntMap (Int, Value, [Int]))
buildAst xs = AST a ir or
  where ((ir,or), (a, s, c)) = flip runState (M.empty, [], -i) $ do
          ir <- replicateM i push
          mapM_ swizzle xs
          or <- replicateM o pop
          return (ir, or)
        (i, o) = arityM xs

testAst = liftM buildAst . tok

exec (0, W "+", i) = do
  [I x, I y] <- mapM force i
  return . I $ x + y
exec (0, R x, []) = force x
exec (0, x, []) = return x

force x = exec . (!x) =<< ask  

eval (AST a i o) = mapM force o `runReader` a

run xs = (eval . buildAst) `liftM` tok xs

tok x = case tokenize x of
  Left _ -> error "failed to tokenize"
  Right x -> return x

seperate [s] = [s]
seperate ss = nub . sort . filter (\x -> not $ any (S.isProperSubsetOf x) ss') $ ss'
  where ss' = concat [[x `S.difference` y, x `S.intersection` y, y `S.difference` x] | (x:xs) <- init . tails $ ss, y <- xs]

deps a x = deps' x S.empty
  where deps' [] s = s
        deps' (x:xs) s = deps' (dep1 a x ++ xs) (x `S.insert` s)

depsM a x = deps' x S.empty
  where deps' [] s = return s
        deps' (x:xs) s = do xs' <- dep1 a x
                            deps' (xs' ++ xs) (x `S.insert` s)

dep1M a x | x < 0 = return []
          | w == W "\\/" = return [c!!0] `mplus` return [c!!1]
          | otherwise = return c
  where (_,w,c) = a!x
                
dep1 a x | x < 0 = []
         | w == W "\\/" = []
         | otherwise = c
  where (_,w,c) = a!x
{-
outDeps a = mapM (deps a . (:[])) $ s
  where (_, Q (AST _ s), _) = a ! snd (bounds a)

units a = reg
  where pr = [ (x, S.fromList (concat y) `S.difference` x)
             | od <- outDeps a,
               x <- (map (S.filter (>= 0)) . seperate) od,
               let y = dep1' a `map` S.toList x ]
        o = S.fromList (concatMap (S.toList . snd) pr ++ s)
        (_, Q (AST _ s), _) = a ! snd (bounds a)
        reg = [ (S.toAscList r, S.toAscList $ p `S.difference` o, S.toAscList $ p `S.intersection` o) | (p, r) <- pr ]
-}
varName x | x < 0 = "in" ++ show (negate x - 1)
          | otherwise = "reg" ++ show x

gen (d, (_, W "+", [x, y])) = varName d ++ " = " ++ varName x ++ " + " ++ varName y
gen (d, (_, I x, _)) = varName d ++ " = " ++ show x
gen (d, (_, Q _, _)) = varName d ++ " = <<AST>>"
gen (d, (x, W "in", _)) = varName d ++ " = in" ++ show x
gen (d, (x, W w, cs)) = varName d ++ " = " ++ w ++ "_" ++ show x ++ "(" ++ commas (map varName cs) ++ ")"

commas = concat . intersperse ", "
{-
generate name src = do
  a <- buildAst `liftM` tok src
  return [func (name ++ "_" ++ show c) a u | (c,u) <- zip [0..] $ units a]
-}
proto name i o = "int " ++ name ++ "( " ++ commas (["int " ++ varName x | x <- i] ++ 
                                                    ["int *" ++ varName x | x <- o]) ++ " )"
func name a (i, r, o) =
  proto name i (tail o) ++ "\n" ++
  "{\n" ++
  "  int " ++ commas [varName x | x <- r ++ [head o]] ++ ";\n" ++
  concatMap (\x -> "  "++ gen (x, (a!x)) ++ ";\n") (r ++ [head o]) ++
  concatMap (\x -> "  *"++ gen (x, (a!x)) ++ ";\n") (tail o) ++
  "  return " ++ varName (head o) ++ ";\n" ++
  "}\n"
