import Data.List

-- DEFINITIONS
boxsize :: Int
boxsize = 2

values :: [Char]
values = ['1' .. '4']

type Grid = Matrix Value

type Matrix a = [Row a]

type Value = Char

type Row a = [a]

-- EXAMPLE GRIDS
-- 2x2
test :: Grid
test =
  [ "1234",
    "3412",
    ".3.1",
    ".1.3"
  ]

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

-- TRANSFORMATION
rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols = transpose

boxs :: Matrix a -> Matrix a
boxs = map concat . concatMap cols . split . map split
  where
    split [] = []
    split row = take boxsize row : split (drop boxsize row)
    group [] = []

-- SOLVE
type Choices = [Value]

solve = prune . map choices

choices :: Row Value -> Row Choices
choices = map (\v -> if v == '.' then values else [v])

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map filterChoices . f

filterChoices :: Row Choices -> Row Choices
filterChoices css =
  map (\cs -> if length cs == 1 then cs else cs \\ singles css) css
  where
    singles = concat . filter single

single :: [Value] -> Bool
single [] = False
single (_ : xs) = null xs

complete :: Matrix Choices -> Bool
complete = all (all single)
