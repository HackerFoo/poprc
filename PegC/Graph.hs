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
import Control.Monad.Trans.List
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
{-
regCntM xs = fst (arityM xs) + sum (map regCnt xs)
regCnt (W "swap") = 0
regCnt (W "dup") = 0
regCnt (W "pop") = 0
--regCnt (W "\\/") = 0
regCnt (W "popr") = 1
regCnt x = snd (arity x)
-}

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
  tellA [(r, (0, Q $ AST (M.fromList [(0, (0, R x, []))]) [] [[0]], [x]))]
swizzle (W "popr") = do  
  x <- pop
  (0, Q a, _) <- getRef x
  rt <- push
  let (h, r, o, t) = splitAst (rt+1) a
  newRefs $ M.size h
  tellA $ (rt, (0, Q t, r)) : M.toList h
  pushI o
  return ()
swizzle (W ".") = do
  (0, Q y, yd) <- getRef =<< pop
  (0, Q x, xd) <- getRef =<< pop
  z <- push
  tellA [(z, (0, Q $ x `composeAst` y, xd ++ yd))]
swizzle (L xs) = do
  o <- push
  tellA [(o, (0, Q (buildAst xs), []))]
swizzle x = do
    i <- replicateM ic pop
    o <- replicateM oc push
    tellA [(r, (n, x, i)) | (r,n) <- zip o [0..]]
  where (ic, oc) = arity x

-- messy, needs clean up
splitAst x (AST a i [o:os]) = (h', map (nm!) $ S.toList rd, nm!o, AST t' i [os])
  where t = M.mapWithKey (\k x -> case k `M.lookup` nm of -- add refs
                             Just r -> (0, R r, [])
                             Nothing -> x) a
        t' = M.filterWithKey (\k _ -> k `S.member` td) $ t -- filter tail
        td = deps t os
        hd = deps a [o]
        rd = td `S.intersection` hd
        h = M.filterWithKey (\k (_, w, _) -> k `S.member` hd && not (isRef w)) $ a -- filter head
        h' = M.fromList . zip [x..] . map f . M.elems $ h -- transpant
        nm = M.fromList $ zip (M.keys h) [x..] ++ -- map from inside to outside node IDs
                          [(x, y) | (x, (_, R y, _)) <- M.toList a]
        --f (0, R r, []) = (0, W "id", [r])
        f (n, w, cs) = (n, w, map (nm!) cs)

-- messy, but works
composeAst :: AST -> AST -> AST
composeAst (AST ax ix [ox]) (AST ay iy [oy]) = AST axy (ix ++ iy') [oy' ++ drop (length iy) ox]
  where mIx = if M.null ax then 0 else fst (M.findMax ax) + 1
        axy = M.union ax . M.fromList .
              map (\(k, (n, w, ns)) -> (k+mIx, (n, updateRef w, map update ns))) .
              M.toList $ ay
        update x | x < 0 = iom!x
                 | otherwise = x+mIx
        -- maintain references in subs
        updateRef (Q (AST a i o)) = Q $ AST (M.map f a) i o
          where f (n, R x, d) = (n, R $ update x, d)
                f x = x
        updateRef x = x
        iom = M.fromList $ zip iy (iy' ++ ox)
        iy' = reverse $ take (length iy - length ox) [minIn, minIn-1..]
        minIn = if null ix then (-1) else minimum (ix ++ iy) - 1
        oy' = map (+mIx) oy
        
buildAst :: [Value] -> AST
buildAst xs = AST a [(-i)..(-1)] or
  where (or, (a, s, c)) = flip runState (M.empty, [], -i) . runListT $ do
          replicateM i push
          mapM_ swizzle xs
          replicateM o pop
        (i, o) = arityM xs

testAst = liftM buildAst . tok

exec (0, W "+", i) = do
  [I x, I y] <- mapM force i
  return . I $ x + y
exec (0, W "id", [x]) = force x
exec (0, R x, []) = force x
exec (0, x, _) = return x

force x = exec . (!x) =<< ask  

eval (AST a i [o]) = mapM force o `runReader` a

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
        deps' (x:xs) s = do xs' <- dep1M a x
                            deps' (xs' ++ xs) (x `S.insert` s)

dep1M a x | x < 0 = return []
          | w == W "\\/" = return [c!!0] `mplus` return [c!!1]
          | otherwise = return c
  where (_,w,c) = a!x
                
dep1 a x | x < 0 = []
         | w == W "\\/" = []
         | otherwise = c
  where (_,w,c) = a!x

outDeps (AST a i [o]) = mapM (depsM a . (:[])) $ o

units ast@(AST a i [o]) = reg
  where pr = [ (x, S.fromList (concat y) `S.difference` x)
             | od <- outDeps ast,
               x <- (map (S.filter (>= 0)) . seperate) od,
               let y = dep1 a `map` S.toList x ]
        o' = S.fromList (concatMap (S.toList . snd) pr ++ o)
        reg = [ (S.toAscList r, S.toAscList $ p `S.difference` o', S.toAscList $ p `S.intersection` o') | (p, r) <- pr ]

varName x | x < 0 = "in" ++ show (negate x - 1)
          | otherwise = "reg" ++ show x

gen (d, (_, W "+", [x, y])) = varName d ++ " = " ++ varName x ++ " + " ++ varName y
gen (d, (_, I x, _)) = varName d ++ " = " ++ show x
gen (d, (_, Q _, _)) = varName d ++ " = <<AST>>"
gen (d, (x, W "in", _)) = varName d ++ " = in" ++ show x
gen (d, (x, W w, cs)) = varName d ++ " = " ++ w ++ "_" ++ show x ++ "(" ++ commas (map varName cs) ++ ")"

commas = concat . intersperse ", "

generate name src = do
  a <- buildAst `liftM` tok src
  return [func (name ++ "_" ++ show c) a u | (c,u) <- zip [0..] $ units a]

proto name i o = "int " ++ name ++ "( " ++ commas (["int " ++ varName x | x <- i] ++ 
                                                    ["int *" ++ varName x | x <- o]) ++ " )"
func name (AST a _ _) (i, r, o) =
  proto name i (tail o) ++ "\n" ++
  "{\n" ++
  "  int " ++ commas [varName x | x <- r ++ [head o]] ++ ";\n" ++
  concatMap (\x -> "  "++ gen (x, (a!x)) ++ ";\n") (r ++ [head o]) ++
  concatMap (\x -> "  *"++ gen (x, (a!x)) ++ ";\n") (tail o) ++
  "  return " ++ varName (head o) ++ ";\n" ++
  "}\n"
