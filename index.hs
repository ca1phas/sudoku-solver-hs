import Data.List (transpose)

type Value = Char

type Row a = [a]

type Matrix a = [Row a]

type Grid = Matrix Value

blank :: Grid
blank = replicate 9 (replicate 9 '.')

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = transpose

boxes :: Matrix a -> [Row a]
boxes [] = []

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x : xs) = x `notElem` xs && unique xs

valid :: Grid -> Bool
valid g = all unique (rows g) && all unique (cols g) && all unique (boxes g)
