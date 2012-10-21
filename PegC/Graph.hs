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

{-# LANGUAGE NoMonomorphismRestriction #-}
module PegC.Graph where

import PegC.Tokenize
import PegC.Value
import Data.List
import Control.Monad
import qualified Data.IntMap as M
import Control.Monad.State
import Control.Monad.Writer
import Control.Arrow (first)
import Debug.Trace

arity (W "+") = (2,1)
arity (W "-") = (2,1)
arity (W "*") = (2,1)
arity (W "div") = (2,1)
arity (W "dup") = (1,2)
arity (W "drop") = (1,0)
arity (W "swap") = (2,2)
arity (W "\\/") = (2,1)
arity (W "!") = (1,1)
arity (W ".") = (2,1)
arity (W "popr") = (1,2)
--arity (W "pushr") = (2,1)
arity (W "quot") = (1,1)
arity (R _) = (1,1)
arity _ = (0,1)

ast rf xs = foldl' f (AST [] 0, rf) xs
  where f (AST s rq, rf) x = (AST (on ++ s') (max rq $ rq + i - a), rf')
          where (i, o) = arity x
                (x', rf') = case x of
                  (L xs) -> first Q $ ast rf xs
                  (W "dup") -> (x, rf+1)
                  _ -> (x, rf)
                on = if x /= W "dup"
                       then [Node x' n c | n <- [0..o-1]]
                       else [Node (R rf) n [Node x n c] | n <- [0..o-1]]
                (c, s') = splitAt i $ s ++ [Node (W "in") n [] | n <- [rq..rq+i-a-1]]
                a = length s

{-
req xs = foldl' req' (-1) xs + 1
  where --req' n (Node (W "in") x ns) = foldl' req' (max x n) ns
        req' n (Node _ _ ns) = foldl' req' n ns

refCount xs = foldl' rc (-1) xs + 1
  where rc n (Node (R x) _ ns) = foldl' rc (max x n) ns
        --rc n (Node (Q xs) _ ns) = foldl' rc (max (refCount xs) n) ns
        rc n (Node _ _ ns) = foldl' rc n ns

refAdd n = map (mapAST f)
  where f (R r) i ns = Node (R (r+n)) i ns
        f (Q (AST xs rf rq)) i ns = Node (Q $ AST (refAdd n xs) rf rq) i ns
        f v i ns = Node v i ns
-}
mapAST f (Node x i ns) = f x i $ mapAST f `map` ns
mapASTM f (Node x i ns) = f x i $ mapM (mapASTM f) ns

swizzle (AST xs rq) = do xs' <- mapM (mapASTM f) xs
                         return $ AST xs' rq
  where f (W "dup") _ m = fmap head m
        f (W "id") n m = do xs <- m
                            guard (n < length xs)
                            return (xs !! n)
        f (W "swap") n m = do xs <- m
                              guard (n < length xs)
                              return (reverse xs !! n)
        f (W "popr") 0 m = do [q] <- m
                              Q (AST (x:xs) 0) <- eval (probe "q = " q)
                              --guard $ req [x] == 0
                              return x
        f (W "popr") 1 m = do [q] <- m
                              Q (AST (x:xs) 0) <- eval q
                              --guard $ req [x] == 0
                              return $ Node (Q $ AST xs 0) 0 []
        f (W "quot") _ m = do [x] <- m
                              return $ Node (Q $ AST [x] 0) 0 []
        f v i m = Node v i `fmap` m
                
tok x = case tokenize x of
  Left _ -> mzero
  Right x -> return x

eval = mapASTM exec

composeAST (AST x rx) (AST y ry) = AST (map (mapAST f) y ++ drop ry x) (rx + max 0 (ry - length x))
  where f (W "in") n [] = x!!n
        f v i xs = Node v i xs

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
                                    modify (M.insert x z)
                                    return z
                      Just z -> return z
exec (W _) _ _ = mzero
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
gen (W "in") x _ = do (m, c, i) <- get
                      put (m, c+1, (x,c):i)
                      return c

comp x = do (t, _) <- ast 0 `fmap` tok x
            AST a r <- swizzle t
            return (a, r)
comp' = flip evalStateT M.empty . comp
run = flip evalStateT M.empty . (mapM eval . fst <=< comp)

commas = concat . intersperse ", "

generate src = do
  (a, r) <- comp' src
  ((o, (_, n, i)), s) <- runWriterT . flip runStateT (M.empty, 0, []) $ mapM (mapASTM gen) a
  return (i, r, o, n, s)

genFunc n x = func n `fmap` generate x

outName n = "out" ++ show n
inName n = "in" ++ show n

proto name r o = "void " ++ name ++ "( " ++ commas (["int in" ++ show x | x <- [0..r-1]] ++
                                                    ["int *out" ++ show x | x <- [0..length o - 1]]) ++ " )"
func name (i, r, o, n, s) = proto name r o ++ "\n{\n" ++
                            declare ++
                            copyIn ++
                            concatMap (("  "++).(++ ";\n")) s ++
                            copyOut ++
                            "}\n"
  where declare | n - length i > 0 = "  int " ++ commas [varName x | x <- [0..n-1]] ++ ";\n"
                | otherwise = ""
        copyIn = concatMap (\(x, y) -> "  " ++ varName y ++ " = in" ++ show x ++ ";\n") i
        copyOut = concatMap (\(x, y) -> "  *out" ++ show y ++ " = " ++ varName x ++ ";\n") (zip o [0..])
