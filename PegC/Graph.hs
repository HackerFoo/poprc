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

module PegC.Graph where

import PegC.Tokenize
import PegC.Value
import Control.Monad.State
import Data.List
import Debug.Trace
import Control.Applicative
import Control.Arrow (first, second)
{-
arity = foldl' composeArity (0,0)

composeArity (i0, o0) (i1, o1) | d >= 0 = (i0, o1 + d)
                               | otherwise = (i0 - d, o1)
  where d = o0 - i1
-}

-- dependencies
ast :: [Value] -> AST
ast = foldl' (\x v -> op (q v) (arity v) x) (0, 0, [], [])
      -- c --> output count
      -- r --> remaining values required
      -- s --> stack holding locations of outputs
      -- g --> the graph
  where op v (i,o) (c, r, s, g) = (c+1, r + r', s'', g')
          where (a, s') = splitAt i s
                r' = i - length a
                s'' = [(0, x) | x <- [0..o-1]] ++ map (\(x, y) -> (x+1, y)) s'
                g' = (v, a ++ [(c, x) | x <- [r..r+r'-1]]) : g
        q (L x) = Q (ast x)
        q x = x

composeAst :: AST -> AST -> AST
composeAst (cf, rf, sf, f) (cg, rg, sg, g)
  | rg == 0 = (cf + cg, rf, sg ++ map (\(x, y) -> (x+cg, y)) sf, g ++ f)
  | otherwise = (cf + cg + 1,
                 rf + rg',
                 sg ++ map (\(x, y) -> (x+cg+1, y)) (drop rg sf),
                 g ++ (W "id", sf ++ [(cf+1,x) | x <- [0..rg'-1]]) : f)        
  where rg' = max 0 (rg - length sf)

evalA :: AST -> Maybe [Value]
evalA (c, r, s, g) | r /= 0 = Nothing
                   | otherwise = getArgs s <$> foldM (\s (x, ds) -> (:s) <$> exec x (getArgs ds s)) [] (reverse g)

generate n = generate' n . absRef
generate' :: String -> AST -> (String, String)
generate' n (c, r, s, g) = (sig, "{\n" ++ dec ++ body ++ tail ++ "}\n")
  where sig = "void " ++ n ++ "(" ++ comma (["int " ++ varString (0, x) | x <- [0..r-1]] ++
                                            ["int *out" ++ show n | n <- [0..length s-1]]) ++ ")"
        intVs = filter (\(x,_) -> x > 0) . nub . sort . (s++) . concatMap snd $ g
        dec = "  int " ++ comma (map varString intVs) ++ ";\n"
        body = concatMap (\((w, rs), n) -> "  " ++ gen w rs n ++ ";\n") $ zip (reverse g) [1..]
        tail = concat ["  " ++ copyOut x n ++ ";\n" | (x, n) <- zip s [0..]]

proto (h, b) = h ++ ";\n"
func (h, b) = h ++ "\n" ++ b

shiftFst n (x, y) = (n-x-1, y)

-- make references absolute (instead of relative)
absRef (c, r, s, g) = (c, r, map (shiftFst (c+1)) s, reverse g')
  where g' = map (\(n, (w, xs)) -> (w, map (shiftFst n) xs)) . zip [1..] . reverse $ g 

varString (0, y) = "in" ++ show y
varString (x, y) = "var" ++ show x ++ "_" ++ show y
copyOut x n = "*out" ++ show n ++ " = " ++ varString x

reachable :: [Int] -> [[Int]] -> ([Int], [Bool])
reachable = mapAccumL f
  where f s d | 0 `elem` s = (dec s ++ d, True)
              | otherwise = (dec s, False)
        dec = map (subtract 1) . filter (> 0)

-- selectively remove items from the AST
collapse g s = reverse $ foldl (\g' (b, x) -> if b then x:g' else moveDep (-1) g') [] (zip s g)

-- remove dropped branches
prune (c, r, s, g) = (length g', r, s', g')
  where (_, bs) = reachable (map fst s) (map (map fst . snd) g)
        (_,s'):g' = collapse ((W "", s):g) (True:bs)

swizzle (c, r, s, g) = (length g'', r, s', g'')
  where g' = map (\((w, ds):g) -> (w, map (deepGet g) ds)) . init . tails $ (W "",s) : g
        ((_,s'):g'') = collapse g' (map f g')
        f (W "dup", _) = False
        f (W "swap", _) = False
        f (W "id", _) = False
        f _ = True

deepGet g (x,y) = case h of
  (W "swap", ds) -> cont (reverse ds !! y)
  (W "id", ds) -> cont (ds !! y)
  (W "dup", [d]) -> cont d
  _ -> (x, y)
  where h:g' = drop x g
        cont = first ((+1).(+x)) . deepGet g'

evalS a = do 
  (h, t) <- split a
  eh <- evalA h
  return (eh, t)
  
outDep (c, r, s, g) = transpose $ map (\x -> snd . reachable [fst x] $ map (map fst . snd) g) s

comp = swizzle . ast . tok

--fragments

-- splits the AST, separating the first indivisible calculation from the rest of the AST
-- TODO: will need to check if reach is outside graph to determine number of required args
split (c, r, s, g)
  | and bs && r > 0 = Nothing
  | otherwise = Just ((length gh, 0, sh', gh), (length gt, r, st', gt))
  where (_, bs) = reachable [fst $ head s] (map (map fst . snd) g)
        sh = filter ((bs !!) . fst) s
        st = filter (not . flip elem sh) s
        (_, sh'):gh = collapse ((W "", sh) : g) (True : bs)
        (_, st'):gt = collapse ((W "", st) : g) (True : map not bs)

astGraph (_,_,_,g) = g

{-
eval :: [Value] -> Maybe [Value]
eval = evalA . ast
-}
{-
prune :: AST -> AST
prune (c, r, s, g) = (length g', r, s', g')
  where ((_, s'):g') = reverse . snd $ foldl shift ([0], []) ((W "", s):g)

shift (s, g) (x, ds) | 0 `elem` s = (map fst ds ++ (filter (>=0) . map (subtract 1) $ s), (x, ds) : g)
                     | otherwise = (filter (>= 0) . map (subtract 1) $ s, moveDep (-1) g)
-}
moveDep o = zipWith f [0..] :: [ValInfo] -> [ValInfo]
  where f n (x, ds) = (x, map (\(d, y) -> if d > n then (d+o, y) else (d, y)) ds) :: ValInfo

exec (W "+") [I y, I x] = return [I $ x + y]
exec (W "-") [I y, I x] = return [I $ x - y]
exec (W "dup") [x] = return [x, x]
exec (W "drop") [x] = return []
exec (W "swap") [x, y] = return [y, x]
exec (W "id") x = return x
exec (W "!") [A "True"] = return [A "True"]
exec (W "!") [_] = mzero
exec (W "popr") [Q ast] = do
  (h, t) <- evalS ast
  return [head h, Q t]
exec x [] = return [x]

comma = concat . intersperse ", "

gen_op op [y, x] o = varString (o, 0) ++ " = " ++ varString x ++ " " ++ op ++ " " ++ varString y

gen (W "+") i o = gen_op "+" i o
gen (W "-") i o = gen_op "-" i o
gen (W "*") i o = gen_op "*" i o
gen (W "/") i o = gen_op "/" i o
gen (I x) [] o = varString (o, 0) ++ " = " ++ show x
gen (W w) i o = w ++ "(" ++ comma (map varString i ++ map (\x -> "&" ++ varString (o, x)) [0..oc-1]) ++ ")"
  where (ic, oc) = arity (W w)

getArgs ds s = map (\(x,y) -> s!!x!!y) ds

tok s = case tokenize s of
  Right x -> x
  Left _ -> []

arity (W "+") = (2,1)
arity (W "-") = (2,1)
arity (W "*") = (2,1)
arity (W "/") = (2,1)
arity (W "dup") = (1,2)
arity (W "drop") = (1,0)
arity (W "swap") = (2,2)
arity (W "\\/") = (2,1)
arity (W "!") = (1,1)
arity (W ".") = (2,1)
arity (W "popr") = (1,2)
arity (W "pushr") = (2,1)
arity _ = (0,1)

-- $ forces arg to calculate arity