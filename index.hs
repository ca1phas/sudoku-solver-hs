import Data.List

-- Declarations
type Value = Char

type Row a = [a]

type Matrix a = [Row a]

type Grid = Matrix Value

-- Definitions
boxsize :: Int
boxsize = 3

values :: [Char]
values = ['1' .. '9']

empty :: Char -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

blank :: Grid
blank = replicate 9 (replicate 9 '.')

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

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x : xs) = x `notElem` xs && unique xs

valid :: Grid -> Bool
valid g = all unique (rows g) && all unique (cols g) && all unique (boxes g)
