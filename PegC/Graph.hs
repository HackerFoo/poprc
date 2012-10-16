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
arity _ = (0,1)

ast = fst . foldl' f ([], 0)
  where f (s, rc) x = (on ++ s', if i > a then rc + i - a else rc) 
          where (i, o) = arity x
                on = [Node x n c | n <- [0..o-1]]
                (c, s') = splitAt i $ s ++ [Node (A "in") n [] | n <- [rc..rc+i-a-1]]
                a = length s

req xs = foldl' req' (-1) xs + 1
  where req' n (Node (A "in") x ns) = foldl' req' (max x n) ns
        req' n (Node _ _ ns) = foldl' req' n ns

mapAST f (Node x i ns) = f x i $ mapAST f `map` ns
mapASTM f (Node x i ns) = f x i =<< mapM (mapASTM f) ns
                
swizzle = mapASTM f
  where f (W "dup") _ [x] = return x
        f (W "id") n xs = guard (n < length xs) >> return (xs !! n)
        f (W "swap") n xs = guard (n < length xs) >> return (reverse xs !! n)
        f (W "popr") 0 [q] = do Q (x:xs) <- eval q
                                guard $ req [x] == 0
                                return x
        f (W "popr") 1 [q] = do Q (x:xs) <- eval q
                                guard $ req [x] == 0
                                return $ Node (Q xs) 0 []
        f (L l) i xs = return $ Node (Q (ast l)) i xs
        f (W "quot") _ [x] = return $ Node (Q [x]) 0 []
        f v i xs = return $ Node v i xs
                
tok x = case tokenize x of
  Left _ -> mzero
  Right x -> return x

eval = mapASTM exec

composeAST x y = map (mapAST f) y ++ drop (req y) x
  where f (A "in") n [] = x!!n
        f v i xs = Node v i xs

exec (W "+") _ [I y, I x] = return . I $ x + y
exec (W "-") _ [I y, I x] = return . I $ x - y
exec (W "*") _ [I y, I x] = return . I $ x * y
exec (W "div") _ [I y, I x] = return . I $ x `div` y
exec (W ".") _ [Q y, Q x] = return . Q $ composeAST x y
exec (W "dup") _ [x] = return x
exec (W "id") n xs | n < length xs = return $ xs !! n
exec (W "swap") n xs | n < length xs = return $ reverse xs !! n
exec (W _) _ _ = mzero
exec x _ _ = return x

comp = mapM swizzle . ast <=< tok
run = mapM eval <=< comp
