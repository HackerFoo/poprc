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
import Data.Monoid

-- return the arity of a word
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

-- calculate the arity of the composition of two words
composeArity (ix, ox) (iy, oy) = (ix + max 0 (-h), oy + max 0 h) 
  where h = ox - iy

arityM = foldl' (\s -> composeArity s . arity) (0,0)

-- push a new index on the stack
push = do s <- get
          (a,c) <- lift get
          put (c:s)
          lift $ put (a, c+1)
          return c

-- reserve 'n' new indices, returning the first
newRefs n = do (a, c) <- lift get
               lift $ put (a, c+n)
               return c

-- push the index 'x' on the stack
pushI x = do s <- get
             put (x:s)
             return x

-- pop an index off the stack
pop = do
  (x:s) <- get
  put s
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

-- add a new mapping
tellA x = lift $ modify (\(a, c) -> (a `M.union` M.fromList x, c))

{-
getRef x = do
  (a, _) <- lift get
  case a!x of
    (n, W "\\/", [a, b]) -> getRef a `mplus` getRef b
    (n, W ".", [ar, br]) -> do
      Q a <- getRef ar
      Q b <- getRef br
      return . Q $ a `composeAst` b
    (n, W "quot", [a]) ->
      return . Q $ AST (M.fromList [(0, (0, R a, []))]) [] [0]
    (n, x, d) -> return x
-}
multi m = do
  s <- get        
  (a, c) <- lift get
  return . flip evalStateT (a, c) . runListT . flip evalState s $ m

-- initial pass; eliminate swap, dup, quotes
swizzle (W "swap") = do
  (x:y:s) <- get
  put (y:x:s)
swizzle (W "dup") = do
  (x:s) <- get
  put (x:x:s)
swizzle (W "pop") = pop >> return ()
{-
swizzle (W "popr") = do
  x <- pop
  l <- multi $ do
    Q a <- getRef x
    [rt] <- newRefs 1
    let (h, r, o, t) = splitAst (rt+1) a
    newRefs $ M.size h
    tellA $ (rt, (0, Q t, r)) : M.toList h
    return (rt, o)
  c <- 
  return ()
swizzle (W "quot") = do
  x <- pop
  r <- push
  tellA [(r, (0, Q $ AST (M.fromList [(0, (0, R x, []))]) [] [[0]], [x]))]
swizzle (W ".") = do
  (0, Q y, yd) <- getRef =<< pop
  (0, Q x, xd) <- getRef =<< pop
  z <- push
  tellA [(z, (0, Q $ x `composeAst` y, xd ++ yd))]
swizzle (W "\\/") = do
  y <- pop
  x <- pop
  pushI x `mplus` pushI y
  return ()
-}
swizzle (L xs) = do
  o <- push
  tellA [(o, (0, Q (buildAst xs), []))]
swizzle x = do
    i <- replicateM ic pop
    o <- replicateM oc push
    tellA [(r, (n, x, i)) | (r,n) <- zip o [0..]]
  where (ic, oc) = arity x

-- seperate nested ASTs
seperateAst n xs = seperateAst' 0 [(n, xs)]
  where seperateAst' c [] = []
        seperateAst' c ((t, AST x e i o):xs) = (t, AST x' e i o) : seperateAst' c' (ss ++ xs)
          where ((c', ss), x') = M.mapAccum f (c, []) x
                f (c, ss) (n, (Q x), d) = ((c+1, ((sn c, x):ss)), (n, P $ sn c, d))
                f (c, ss) x = ((c, ss), x)
                sn c = n ++ "_sub" ++ show c
  
{-
-- messy, needs clean up
splitAst x (AST a i (o:os)) = (h', map (nm!) $ S.toList rd, nm!o, AST t' i os)
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
composeAst (AST ax ix ox) (AST ay iy oy) = AST axy (ix ++ iy') (oy' ++ drop (length iy) ox)
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
-}

-- build an AST from tokenized input
buildAst :: [Value] -> AST
buildAst xs = AST a [] [(-i)..(-1)] or
  where (or, (a, _)) = flip runState (M.empty, -i) . flip evalStateT [] $ do
          replicateM i push
          mapM_ swizzle xs
          replicateM o pop
        (i, o) = arityM xs

testAst = liftM buildAst . tok

exec (0, W "+", i) = do
  [I x, I y] <- mapM force i
  return . I $ x + y
exec (0, W "id", [x]) = force x
exec (0, W "\\/", [x, y]) = force x `mplus` force y
exec (0, R x, []) = force x
exec (0, x, _) = return x

force x = exec . (!x) =<< ask

choose = foldr (mplus . return) mzero

eval (AST a e i o) = flip runReader a . runListT $ mapM force o

run xs = (eval . buildAst) `liftM` tok xs

tok x = case tokenize x of
  Left _ -> error "failed to tokenize"
  Right x -> return x

-- seperate partially dependent sets of indices
seperate [s] = [s]
seperate ss = nub . sort . filter (\x -> not $ any (S.isProperSubsetOf x) ss') $ ss'
  where ss' = concat [[x `S.difference` y, x `S.intersection` y, y `S.difference` x] | (x:xs) <- init . tails $ ss, y <- xs]

-- calculate dependencies in an AST
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

-- dependencies of the returned values
outDeps (AST a e i o) = mapM (depsM a . (:[])) $ o

-- group independent groups of indicies of an AST into inputs, registers, and outputs
units ast@(AST a e i o) = reg
  where pr = [ (x, S.fromList (concat y) `S.difference` x)
             | od <- outDeps ast,
               x <- (map (S.filter (>= 0)) . seperate) od,
               let y = dep1 a `map` S.toList x ]
        o' = S.fromList (concatMap (S.toList . snd) pr ++ o)
        reg = [ (S.toAscList r, S.toAscList $ p `S.difference` o', S.toAscList $ p `S.intersection` o') | (p, r) <- pr ]

varName x | x < 0 = "in" ++ show (negate x - 1)
          | otherwise = "reg" ++ show x

-- generate code from a word
gen (d, (_, W "+", [x, y])) = varName d ++ " = add_int(" ++ varName x ++ ", " ++ varName y ++ ")"
gen (d, (_, I x, _)) = varName d ++ " = var_int(" ++ show x ++ ")"
gen (d, (_, P n, _)) = varName d ++ " = &" ++ n 
gen (d, (_, Q _, _)) = varName d ++ " = <<AST>>"
gen (d, (x, W "id", [r])) = varName d ++ " = " ++ varName r
gen (d, (x, W w, cs)) = varName d ++ " = " ++ w ++ "_" ++ show x ++ "(" ++ commas (map varName cs) ++ ")"

commas = concat . intersperse ", "

generate name src = do
  a <- buildAst `liftM` tok src
  return [func (n ++ "_" ++ show c) a' u |
          (n, a') <- seperateAst name a,
          (c,u) <- zip [0..] $ units a']

proto name i o = "cell_t *" ++ name ++ "( " ++ commas (["int " ++ varName x | x <- i] ++ 
                                                   ["int *" ++ varName x | x <- o]) ++ " )"
func name (AST a _ _ _) (i, r, o) =
  proto name i (tail o) ++ "\n" ++
  "{\n" ++
  "  int " ++ commas [varName x | x <- r ++ [head o]] ++ ";\n" ++
  concatMap (\x -> "  "++ gen (x, (a!x)) ++ ";\n") (r ++ [head o]) ++
  concatMap (\x -> "  *"++ gen (x, (a!x)) ++ ";\n") (tail o) ++
  "  return " ++ varName (head o) ++ ";\n" ++
  "}\n"

testGen = mapM_ putStrLn <=< generate "test"
