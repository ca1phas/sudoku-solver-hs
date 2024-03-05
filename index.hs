import Data.List

-- DECLARATIONS
type Grid = [Group]

type Group = [Int]

type Domain = [Int]

-- DEFINITIONS
boxsize :: Int
boxsize = 3

blank :: Int
blank = 0

maxDigit :: Int
maxDigit = boxsize ^ 2

domain0 :: Domain
domain0 = [1 .. maxDigit]

-- EXAMPLE GRIDS
-- Solvable only using the basic rules
easy' :: Grid
easy' =
  [ [2, 4, 9, 5, 7, 1, 6, 3, 8],
    [8, 6, 1, 4, 3, 2, 9, 7, 5],
    [5, 7, 3, 9, 8, 6, 1, 4, 2],
    [7, 2, 5, 6, 9, 8, 4, 1, 3],
    [6, 9, 8, 1, 4, 3, 2, 5, 7],
    [3, 1, 4, 7, 2, 5, 8, 6, 9],
    [9, 3, 7, 8, 1, 4, 5, 2, 6],
    [1, 5, 2, 3, 6, 9, 7, 8, 4],
    [4, 8, 6, 2, 5, 7, 3, 9, 1]
  ]

-- 249|571|638
-- 861|432|975
-- 573|986|142
-- 725|698|413
-- 698|143|257
-- 314|725|869
-- 937|814|526
-- 152|369|784
-- 486|257|391

-- 285|763|914
-- 467|291|358
-- 913|584|726
-- 549|617|832
-- 738|942|165
-- 126|835|497
-- 691|428|573
-- 374|156|289
-- 852|379|641

-- 285|467|913|549|738|126|691|374|852
-- 763|291|584|617|942|835|428|156|379
-- 914|358|726|832|165|497|573|289|641

-- 285467913|549738126|691374852
-- 763291584|617942835|428156379
-- 914358726|832165497|573289641

easy :: Grid
easy =
  [ [2, 0, 0, 0, 0, 1, 0, 3, 8],
    [0, 0, 0, 0, 0, 0, 0, 0, 5],
    [0, 7, 0, 0, 0, 6, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 1, 3],
    [0, 9, 8, 1, 0, 0, 2, 5, 7],
    [3, 1, 0, 0, 0, 0, 8, 0, 0],
    [9, 0, 0, 8, 0, 0, 0, 2, 0],
    [0, 5, 0, 0, 6, 9, 7, 8, 4],
    [4, 0, 0, 2, 5, 0, 0, 0, 0]
  ]

-- First gentle example from sudoku.org.uk
gentle :: Grid
gentle =
  [ [0, 1, 0, 4, 2, 0, 0, 0, 5],
    [0, 0, 2, 0, 7, 1, 0, 3, 9],
    [0, 0, 0, 0, 0, 0, 0, 4, 0],
    [2, 0, 7, 1, 0, 0, 0, 0, 6],
    [0, 0, 0, 0, 4, 0, 0, 0, 0],
    [6, 0, 0, 0, 0, 7, 4, 0, 3],
    [0, 7, 0, 0, 0, 0, 0, 0, 0],
    [1, 2, 0, 7, 3, 0, 5, 0, 0],
    [3, 0, 0, 0, 8, 2, 0, 7, 0]
  ]

-- First diabolical example
diabolical :: Grid
diabolical =
  [ [0, 9, 0, 7, 0, 0, 8, 6, 0],
    [0, 3, 1, 0, 0, 5, 0, 2, 0],
    [8, 0, 6, 0, 0, 0, 0, 0, 0],
    [0, 0, 7, 0, 5, 0, 0, 0, 6],
    [0, 0, 0, 3, 0, 7, 0, 0, 0],
    [5, 0, 0, 0, 1, 0, 7, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 0, 9],
    [0, 2, 0, 6, 0, 0, 3, 5, 0],
    [0, 5, 4, 0, 0, 8, 0, 7, 0]
  ]

-- First "unsolvable" (requires backtracking) example
unsolvable :: Grid
unsolvable =
  [ [1, 0, 0, 9, 0, 7, 0, 0, 3],
    [0, 8, 0, 0, 0, 0, 0, 7, 0],
    [0, 0, 9, 0, 0, 0, 6, 0, 0],
    [0, 0, 7, 2, 0, 9, 4, 0, 0],
    [4, 1, 0, 0, 0, 0, 0, 9, 5],
    [0, 0, 8, 5, 0, 4, 3, 0, 0],
    [0, 0, 3, 0, 0, 0, 7, 0, 0],
    [0, 5, 0, 0, 0, 0, 0, 4, 0],
    [2, 0, 0, 8, 0, 6, 0, 0, 9]
  ]

-- Minimal sized grid (17 values) with a unique solution
minimal :: Grid
minimal =
  [ [0, 9, 8, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 7, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 5, 0, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 2, 0, 0, 0, 0, 9],
    [0, 0, 0, 9, 0, 6, 0, 8, 2],
    [0, 0, 0, 0, 0, 0, 0, 3, 0],
    [5, 0, 1, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 4, 0, 0, 0, 2, 0]
  ]

-- Empty grid
new :: Grid
new = replicate maxDigit (replicate maxDigit blank)

-- Rows, Columns, Boxes
rows :: a -> a
rows = id

cols :: [[a]] -> [[a]]
cols = transpose

boxs :: [[a]] -> [[a]]
boxs = concat . cols . map (concatBy boxsize) . cols . map (chop boxsize)
  where
    chop _ [] = []
    chop n xs = take n xs : chop n (drop n xs)
    concatBy n [] = []
    concatBy n xss = concat (take n xss) : concatBy n (drop n xss)

-- VALIDITY CHECKING
valid' :: [Int] -> Bool
valid' [] = True
valid' (x : xs) = (x == blank || x `notElem` xs) && valid' xs

valid :: Grid -> Bool
-- valid g = all valid' (rows g) && all valid' (cols g) && all valid' (boxs g)
valid g = all valid' (rows g) && all valid' (cols g)

complete' :: [Int] -> Bool
complete' [] = True
complete' (x : xs) = x /= blank && x <= maxDigit && x `notElem` xs && complete' xs

complete :: Grid -> Bool
-- complete g = all complete' rs && all complete' cs && all complete' bs
complete g = all complete' rs && all complete' cs
  where
    rs = rows g
    cs = cols g

-- bs = boxs g

-- A BASIC SOLVER
-- Very large search space, will not terminate in practice

-- PRUNING
-- Large search space, will take a long time to terminate in practice
domain' :: Group -> [Domain]
domain' = map (\c -> if c == blank then domain0 else [c])

domains :: Grid -> [[Domain]]
domains = map domain'

dsslen :: [[Domain]] -> Int
dsslen = length . concat . concat

prune'' :: Domain -> [Domain] -> [Domain]
prune'' _ [] = []
prune'' vs (xs : ds) = xs' : prune'' vs' ds
  where
    xs' = if length xs == 1 then xs else filter (`notElem` vs) xs
    vs' = if length xs' == 1 then head xs' : vs else vs

prune' :: Int -> [[Domain]] -> [[Domain]]
prune' n dss =
  let dss' = cols . boxs $ f (boxs $ f (cols $ f (rows dss)))
      n' = dsslen dss'
   in if n == n' then dss' else prune' n' dss'
  where
    f = map (\ds -> prune'' [head d | d <- ds, length d == 1] ds)

prune :: [[Domain]] -> [[Domain]]
prune dss = prune' (dsslen dss) dss

solve :: Grid -> Grid
solve = map concat . prune . domains

-- REPEATED PRUNING
-- Large search space (except easy sudoku), will take a long time terminate in practice

-- PROPERTIES OF MATRICES

-- EFFICIENT SOLVER
