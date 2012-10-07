{- Copyright 2012 Dustin DeWeese
   This file is part of peg.

    peg is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    peg is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with peg.  If not, see <http://www.gnu.org/licenses/>.
-}

module PegC.Graph where

import PegC.Tokenize
import PegC.Value
import Control.Monad.State
import Data.List
import Debug.Trace
{-
arity = foldl' composeArity (0,0)

composeArity (i0, o0) (i1, o1) | d >= 0 = (i0, o1 + d)
                               | otherwise = (i0 - d, o1)
  where d = o0 - i1
-}
type ValInfo = (Value, [(Int, Int)])
type AST = (Int, Int, [(Int, Int)], [ValInfo])

-- dependencies
ast :: [Value] -> AST
ast = foldl' (\x v -> op v (arity v) x) (0, 0, [], [])
      -- c --> output count
      -- r --> remaining values required
      -- s --> stack holding locations of outputs
      -- g --> the graph
  where op v (i,o) (c, r, s, g) = (c+1, r + r', s'', g')
          where (a, s') = splitAt i s
                r' = i - length a
                s'' = [(0, x) | x <- [0..o-1]] ++ map (\(x, y) -> (x+1, y)) s'
                g' = (v, a ++ [(c, x) | x <- [r..r+r'-1]]) : g

composeAst :: AST -> AST -> AST
composeAst (cf, rf, sf, f) (cg, rg, sg, g) =
  (cf + cg + 1,
   rf + rg',
   sg ++ map (\(x, y) -> (x+cg+1, y)) (drop rg sf),
   g ++ (W "id#", sf ++ [(cf+1,x) | x <- [0..rg'-1]]) : f)        
  where rg' = max 0 (rg - length sf)

evalA :: AST -> Maybe [Value]
evalA (c, r, s, g) | r /= 0 = Nothing
                   | otherwise = Just $ getArgs s vs
  where vs = foldr (\(x, ds) s -> exec x (getArgs ds s) : s) [] g

eval :: [Value] -> Maybe [Value]
eval = evalA . ast

-- remove dropped branches
prune :: AST -> AST
prune (c, r, s, g) = (length g', r, s', g')
  where ((_, s'):g') = reverse . snd $ foldl shift ([0], []) ((W "", s):g)
        shift (s, g) (x, ds) | 0 `elem` s = (map fst ds ++ (filter (>=0) . map (subtract 1) $ s), (x, ds) : g)
                             | otherwise = (filter (>= 0) . map (subtract 1) $ s, update g)
        update = zipWith shiftDep [0..] :: [ValInfo] -> [ValInfo]
          where shiftDep n (x, ds) = (x, map (\(d, y) -> if d > n then (d-1, y) else (d, y)) ds) :: ValInfo

exec (W "+") [I y, I x] = [I $ x + y]
exec (W "-") [I y, I x] = [I $ x - y]
exec (W "dup") [x] = [x, x]
exec (W "drop") [x] = []
exec (W "swap") [x, y] = [y, x]
exec (W "id#") x = x
exec x [] = [x]

getArgs ds s = map (\(x,y) -> s!!x!!y) ds

arity x@(W "+") = (2,1)
arity x@(W "-") = (2,1)
arity x@(W "dup") = (1,2)
arity x@(W "drop") = (1,0)
arity x@(W "swap") = (2,2)
arity x@(W "\\/") = (2,1)
arity x@(W "!") = (1,1)
arity x@(W ".") = (2,1)
arity _ = (0,1)

-- $ forces arg to calculate arity