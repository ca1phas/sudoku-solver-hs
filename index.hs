import Data.List

-- DECLARATIONS
type Value = Char

type Row a = [a]

type Matrix a = [Row a]

type Grid = Matrix Value

-- DEFINITIONS
boxsize :: Int
boxsize = 3

values :: String
values = ['1' .. '9']

empty :: Char -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

-- EXAMPLE GRIDS
-- Solvable only using the basic rules
easy :: Grid
easy =
  [ "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ]

-- First gentle example from sudoku.org.uk
gentle :: Grid
gentle =
  [ ".1.42...5",
    "..2.71.39",
    ".......4.",
    "2.71....6",
    "....4....",
    "6....74.3",
    ".7.......",
    "12.73.5..",
    "3...82.7."
  ]

-- First diabolical example
diabolical :: Grid
diabolical =
  [ ".9.7..86.",
    ".31..5.2.",
    "8.6......",
    "..7.5...6",
    "...3.7...",
    "5...1.7..",
    "......1.9",
    ".2.6..35.",
    ".54..8.7."
  ]

-- First "unsolvable" (requires backtracking) example
unsolvable :: Grid
unsolvable =
  [ "1..9.7..3",
    ".8.....7.",
    "..9...6..",
    "..72.94..",
    "41.....95",
    "..85.43..",
    "..3...7..",
    ".5.....4.",
    "2..8.6..9"
  ]

-- Minimal sized grid (17 values) with a unique solution
minimal :: Grid
minimal =
  [ ".98......",
    "....7....",
    "....15...",
    "1........",
    "...2....9",
    "...9.6.82",
    ".......3.",
    "5.1......",
    "...4...2."
  ]

-- Empty grid
blank :: Grid
blank = replicate n (replicate n '.')
  where
    n = boxsize ^ 2

-- Rows, Columns, Boxes
rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
  where
    pack = split . map split
    split = chop boxsize
    unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

-- VALIDITY CHECKING
unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x : xs) = x `notElem` xs && unique xs

valid :: Grid -> Bool
valid g = all unique (rows g) && all unique (cols g) && all unique (boxes g)

-- A BASIC SOLVER
-- Very large search space, will not terminate in practice
basicSolve :: Grid -> [Grid]
basicSolve = filter valid . collapse . choices

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = map (map choice)
  where
    choice v = if v == '.' then ['1' .. '9'] else [v]

collapse :: Matrix [a] -> [Matrix a]
collapse = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [y : ys | y <- xs, ys <- cp xss]

-- PRUNING
-- Large search space, will take a long time to terminate in practice
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxes . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss = [xs `minus` singles | xs <- xss]
  where
    singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys = if single xs then xs else xs \\ ys

pruneSolve :: Grid -> [Grid]
pruneSolve = filter valid . collapse . prune . choices

-- REPEATED PRUNING
-- Large search space (except easy sudoku), will take a long time terminate in practice
repeatedPruneSolve :: Grid -> [Grid]
repeatedPruneSolve = filter valid . collapse . fix prune . choices

fix :: (Eq a) => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
  where
    x' = f x

-- PROPERTIES OF MATRICES
complete :: Matrix Choices -> Bool
complete = all (all single)

safe :: Matrix Choices -> Bool
safe m =
  all consistent (rows m)
    && all consistent (cols m)
    && all consistent (boxes m)

consistent :: Row Choices -> Bool
consistent row = unique [cs | cs <- row, single cs]

void :: Matrix Choices -> Bool
void = any (any null)

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

-- EFFICIENT SOLVER
solve :: Grid -> [Grid]
solve = search . prune . choices

search :: Matrix Choices -> [Grid]
search m
  | blocked m = []
  | complete m = collapse m
  | otherwise =
      [g | m' <- expand m, g <- search (prune m')]

expand :: Matrix Choices -> [Matrix Choices]
expand m =
  [rows1 ++ [row1 ++ [[c]] ++ row2] ++ rows2 | c <- cs]
  where
    (rows1, row : rows2) = span (all single) m
    (row1, cs : row2) = span single row