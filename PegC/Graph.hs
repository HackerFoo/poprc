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
         put $ trace ("B (s,c) = " ++ show (s,c)) (a, s, c)
         return x
swap = do (a, x:y:s, c) <- get
          put (a, y:x:s, c)
          return [y,x]
dup = do (a, x:s, c) <- get
         put (a, x:x:s, c)
         return [x,x]

regCnt (L xs) = ic + sum (map regCnt xs) + 1
  where (ic, oc) = arityM xs
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
  {-
ast (W ".") = do
  x <- pop
  y <- pop
  (_, Q astX, _) <- getRef x
  (_, Q astY, _) <- getRef y
  z <- push
  tellA [(z, (0, composeAst astX astY, []))]
  return [z]
-}
ast x = do
    i <- replicateM ic pop
    o <- replicateM oc push
    tellA [(r, (n, x, i)) | (r,n) <- zip o [0..]]
    return o
  where (ic, oc) = arity x
{-
composeAst (AST ix ox) (AST iy oy) =
  where 
-}
buildAst xs = a
  where (a, s, c) = flip execState (array (0, regCnt (L xs) - 1) [], [], 0) . ast . L $ xs

exec (0, W "+", i) = do
  [I x, I y] <- mapM force i
  return . I $ x + y
exec (0, x, []) = return x

force x = exec . (!x) =<< ask  

eval a = mapM force o `runReader` a
  where (0, c) = bounds a
        (0, Q (AST [] o), []) = a!c

run xs = (eval . buildAst) `liftM` tok xs

{-
mapAST f (Node x i ns) = f x i $ mapAST f `map` ns
mapASTM f (Node x i ns) = f x i $ mapM (mapASTM f) ns
-}
{-
swizzle
  :: (MonadPlus m, MonadState (M.IntMap Value) m) =>
     AST -> m AST
swizzle (AST xs rq) = do xs' <- mapM (mapASTM f) xs
                         return $ AST xs' rq
  where f (W "dup") _ m = liftM head m
        f (W "id") n m = do xs <- m
                            guard (n < length xs)
                            return (xs !! n)
        f (W "swap") n m = do xs <- m
                              guard (n < length xs)
                              return (reverse xs !! n)
        f (W "popr") 0 m = do [q] <- m
                              Q (AST (x:xs) 0) <- mapASTM exec q
                              return (moveIn (-1) x)
        f (W "popr") 1 m = do [q] <- m
                              Q (AST (x:xs) 0) <- mapASTM exec q
                              return $ Node (Q $ AST xs 0) 0 []
        f (W "quot") _ m = do [x] <- m
                              return $ Node (Q $ AST [moveIn 1 x] 0) 0 []
        f (Q ast) i m = do ast' <- swizzle ast
                           Node (Q ast') i `liftM` m
        f v i m = Node v i `liftM` m
        
moveIn y (Node (In l) n c) = Node (In $ l + y) n (map (moveIn y) c)
moveIn y (Node x n c) = Node x n (map (moveIn y) c)
-}                
tok x = case tokenize x of
  Left _ -> error "failed to tokenize"
  Right x -> return x
{-
eval (AST xs 0) = mapM (mapASTM exec) xs
eval (AST _ _) = error "not enough arguments"

composeAST (AST x rx) (AST y ry) = AST (map (mapAST (f 0)) y ++ drop ry x) (rx + max 0 (ry - length x))
  where f l (In l') n [] | l == l' = if n < length x
                                        then x!!n
                                        else Node (In l) (n - length x) []
        f l (Q (AST ns 0)) n [] = Node (Q (AST (map (mapAST (f (l+1))) ns) 0)) n [] 
        f _ v i xs = Node v i xs

exec (W "+") _ m = do [I y, I x] <- m
                      return . I $ x + y
exec (W "-") _ m = do [I y, I x] <- m 
                      return . I $ x - y
exec (W "*") _ m = do [I y, I x] <- m 
                      return . I $ x * y
exec (W "div") _ m = do [I y, I x] <- m
                        return . I $ x `div` y
exec (W ".") _ m = do [Q y, Q x] <- m
                      return . Q $ composeAST x y
exec (W "dup") _ m = do [x] <- m
                        return x
exec (W "id") n m = do xs <- m
                       guard $ n < length xs
                       return $ xs !! n
exec (W "swap") n m = do xs <- m
                         guard $ n < length xs
                         return $ reverse xs !! n
exec (R x) _ y = do m <- get
                    case M.lookup x m of
                      Nothing -> do [z] <- y
                                    case z of
                                      In _ -> return z
                                      _ -> do
                                        modify (M.insert x z)
                                        return z
                      Just z -> return z
exec (W _) _ _ = error "unknown word"
exec x _ _ = return x

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

comp x = do (t, _) <- ast 0 `liftM` tok x
            a <- swizzle t
            return a
comp' = flip evalStateT M.empty . comp
run = flip evalStateT M.empty . (eval <=< comp)

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