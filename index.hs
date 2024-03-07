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

maxv :: Int
maxv = boxsize ^ 2

domain0 :: Domain
domain0 = [1 .. maxv]

single :: [a] -> Bool
single [_] = True
single _ = False

-- EXAMPLE GRIDS
-- Solvable only using the basic rules
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

-- Rows, Columns, Boxes
rows :: a -> a
rows = id

cols :: [[a]] -> [[a]]
cols = transpose

boxs :: [[a]] -> [[a]]
boxs = concat . cols . map gather . cols . map chop
  where
    chop [] = []
    chop xs = take boxsize xs : chop (drop boxsize xs)
    gather [] = []
    gather xss = concat (take boxsize xss) : gather (drop boxsize xss)

-- Get domains
domains :: Grid -> [[Domain]]
domains = map (map (\c -> if c == blank then domain0 else [c]))

-- Get domains size
dsslen :: [[Domain]] -> Int
dsslen = length . concat . concat

-- Prune domains
prune'' :: Domain -> [Domain] -> [Domain]
prune'' _ [] = []
prune'' vs (d : ds) = if null d' then [] else d' : prune'' vs' ds
  where
    d' = if single d then d else filter (`notElem` vs) d
    vs' = if single d' then head d' : vs else vs

prune' :: Int -> [[Domain]] -> [[Domain]]
prune' n dss = if n == n' then dss' else prune' n' dss'
  where
    pruneBy f = f . map (\ds -> prune'' [head d | d <- ds, single d] ds) . f
    dss' = pruneBy boxs . pruneBy cols $ pruneBy rows dss
    n' = dsslen dss'

prune :: [[Domain]] -> [[Domain]]
prune dss = prune' (dsslen dss) dss

-- Select cell with the minimum remaining values
selectCell'' :: Int -> [Domain] -> [(Int, Int)]
selectCell'' m ds = [(j, n) | (j, d) <- zip [0 ..] ds, let n = length d, n > 1, n < m]

selectCell' :: Int -> (Int, Int) -> Int -> [[Domain]] -> (Int, Int)
selectCell' _ c _ [] = c
selectCell' m c i (ds : dss) =
  if null cs
    then selectCell' m c (i + 1) dss
    else let (j, n) = head cs in selectCell' n (i, j) (i + 1) dss
  where
    cs = selectCell'' m ds

selectCell :: [[Domain]] -> (Int, Int)
selectCell = selectCell' maxvn1 (0, 0) 0

maxvn1 :: Int
maxvn1 = maxv + 1

-- Select value with the least constraining degree
cdeg :: Int -> [Domain] -> Int
cdeg v ds = length [1 | d <- ds, v `elem` d]

sortValues :: [[Domain]] -> (Int, Int) -> [Int]
sortValues dss (i, j) =
  map snd (sortOn fst [(cdeg v ds, v) | v <- vs])
  where
    vs = dss !! i !! j
    ds =
      [ d
        | (i', ds) <- zip [0 ..] dss,
          (j', d) <- zip [0 ..] ds,
          if i == i' then j /= j' else j == j'
      ]

-- Check if grid complete
complete :: [[Domain]] -> Bool
complete = all (all single)

-- Check if grid valid
valid' :: Domain -> [Domain] -> Bool
valid' vs [] = null vs
valid' vs ([v] : ds) = valid' (filter (/= v) vs) ds

valid :: [[Domain]] -> Bool
valid g = all (all (valid' domain0)) [rows g, cols g, boxs g]

-- Solve grid
update :: [[Domain]] -> (Int, Int) -> Domain -> [[Domain]]
update dss (i, j) d =
  let (ldss, ds : rdss) = splitAt i dss
      (lds, _ : rds) = splitAt j ds
   in ldss ++ ((lds ++ (d : rds)) : rdss)

solve' :: [[Domain]] -> [Grid]
solve' dss
  | null dss = []
  | complete dss = [map concat dss | valid dss]
  | otherwise =
      let c = selectCell dss
       in [s | v <- sortValues dss c, s <- solve' (prune (update dss c [v]))]

solve :: Grid -> [Grid]
solve = solve' . prune . domains
