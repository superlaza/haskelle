--I got 99 problems but a list ain't one

{--unsolved
24
25
38
39
--}

import System.Random
import qualified Data.List as D
import qualified Data.Char as DC
import Debug.Trace

--auxiliary functions
removeDups l = map (head) $ pack $ D.sort l

--misc

--BST stuff. As I came across the problem in LYAH I wrote my own solutions
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

--this function was originally written inline, where guards look nasty
insert EmptyTree v = (Node v EmptyTree EmptyTree)
insert (Node x left right) v = if v<x then (Node x (insert left v) right) else (Node x left (insert right v))

--his uses the fact that if the element we're looking for is smaller than the root, we only search down that subtree
isElem EmptyTree v = False
isElem (Node x left right) v
	| x==v = True
	| otherwise = (isElem left v)||(isElem right v)

--P1
myLast [x] = x
myLast (x:xs) = myLast xs

--P2
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

--P3
elementAt l 0 = head l
elementAt (x:xs) n = elementAt xs (n-1)

--P4
myLength [] = 0
myLength (x:xs) = 1+(myLength xs)

--P5
rev [] = []
rev (x:xs) = (rev xs)++[x]

--P6
isPal l = l == rev l


--P7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: (Num a)=> NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List l) = concamap (flatten) l

concamap :: (Num a)=> (NestedList a -> [a]) -> [NestedList a] -> [a]
concamap f [] = []
concamap f (x:xs) = (f x)++(concamap (f) xs) 

test = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]

--P8
compress1 [] a = a
compress1 (x:xs) a
	| (x==(head a)) = compress1 xs a
	| otherwise = compress1 xs (x:a)
	
compress2 [x] = [x]
compress2 (x:xs)
	| (x==(head a)) = a
	| otherwise = (x:a)
	where a = (compress2 xs)
	
--P9
pack [x] = [[x]]
pack (x:xs)
	| (x==(head (head a))) = (x:(head a)):(tail a)
	| otherwise = ([x]:a)
	where a = (pack xs)
	
--P10
encode l = map (\l -> (length l, head l)) (pack l)

--P11
data Encoded a = Single a | Multiple Int a deriving (Show)

encoder [] = []
encoder l = (encoder' h):(encoder (if (null t) then [] else head t)) where (h:t) = (tkwhl (==(head l)) l)

encoder' :: [a] -> (Encoded a)
encoder' [x] = (Single x)
encoder' l = (Multiple (length l) (head l))

tkwhl f [] = [[]]
tkwhl f l@(x:xs) 
	|(f x) = (x:(head a)):(tail a)
	| otherwise = [[],l]
	where a=(tkwhl f xs)

--P12
decoder l = concatMap decoder' l

decoder' (Single x) = [x]
decoder' (Multiple n x) = (replicate n x)

--P14
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--P15
repli l n = concatMap (repli' n) l

repli' 0 x = []
repli' n x = x:(repli' (n-1) x)

--P16
drop' n [] = []
drop' 0 l = l
drop' n (x:xs) = drop' (n-1) xs

takeEvery [] skip = []
takeEvery l@(x:xs) skip = x:(takeEvery (drop' l skip) skip)

take' n [] = []
take' 0 l = []
take' n (x:xs) = x:(take' (n-1) xs)

dropEvery [] skip = []
dropEvery l skip = (take' (skip-1) l)++(dropEvery (drop' skip l) skip)

--P17
--split shouldn't account for negative n because that would be meaningless
split' [] n = []
split' l 0 = [[], l]
split' l@(x:xs) n  
	| (n>=(length l))||(n<0) = [l]
	| otherwise = (x:(head a)):(tail a) where a = (split' xs (n-1))

--P18
slice [] i j = []
slice l i j
	| i==j = [(l!!(i-1))]
	| i>j = l
	| i<j = ((take' (j-i+1)).(drop' (i-1))) l
	
--P19
rotate l n 
	| (n>=0) = (\[l1, l2] -> l2++l1) (split' l (mod n (length l)))
	| (n<0) = (\[l1, l2] -> l2++l1) (split' l a)
	where a = (length l) - (mod (abs n) (length l))
	
--P20
--needs to handle bad inputs
removeAt n l = (\[l1, l2] -> (head l2, l1++(tail l2))) (split' l (n-1))

--P21
insertAt el l i = (\[l1, l2] el -> l1++[el]++l2) (split' l (i-1)) el

--P22
range i j 
	| i>j = []
	| i==j = [i]
	| i<j = i:(range (i+1) j)
	
--P23
--assuming elements must be different...

rndselect' [] n g = []
rndselect' l 0 g = [] 
rndselect' l n g = let (value, newgen) = random g :: (Int, StdGen) in (l!!(mod value (length l))):rndselect' (snd (removeAt (l!!(mod value (length l))) l)) (n-1) newgen 

rndselect l n = rndselect' l n (mkStdGen 1)

--P26
--distintness is important
combinations :: (Ord a)=>Int->[a]->[[a]]
combinations 1 l = [[x] | x<-l]
combinations n l = [x:ps | x<-l, ps<-(combinations (n-1) l), x<(head ps)]

--just for kicks
permutations :: (Ord a)=>Int->[a]->[[a]]
permutations 1 l = [[x] | x<-l]
permutations n l = [x:ps | x<-l, ps<-(permutations (n-1) l), not (elem x ps)]

--P27
removeList l rs = [x | x<-l, not (elem x rs)]

group l [x] = [c:[] | c<-(combinations x l)] 
group l (x:xs) = [c:gs | c<-(combinations x l), gs<-(group (removeList l c) xs)]

--P28
--a)
lsort :: (Ord a) => [[a]] -> [[a]]  
lsort [] = []  
lsort (x:xs) =   
    let smallerSorted = lsort [a | a <- xs, (length a) <= (length x)]  
        biggerSorted = lsort [a | a <- xs, (length a) > (length x)]  
    in  smallerSorted ++ [x] ++ biggerSorted

--b)
toTuple :: (Ord a) => [a] -> (Int, [a])
toTuple x = (length x, x)

fromTuple :: (Ord a) => (Int, [a]) -> [a]
fromTuple (n, l) = l 

--lfsort' l = map toTuple l

--lfsort' :: (Ord a) => [(Int, [a])] -> [(Int, a)]  
lfsort' [] = []  
lfsort' (x:xs) =   
    let smallerSorted = lfsort' [a | a <- xs, (fst a) <= (fst x)]  
        biggerSorted = lfsort' [a | a <- xs, (fst a) > (fst x)]  
    in  smallerSorted ++ [x] ++ biggerSorted

lfsort l = lfsort' ((\l -> (map toTuple l)) l)

--P31
--isPrime' n =  foldl (&&) True (map (\x -> ((mod (n::Int) x)/=0)) [2..((floor (sqrt (n::Int)))::Int)])
isPrime :: (Integral a) => a -> Bool
isPrime n =  foldr (&&) 
					True 
					(map modder pssibleFactors)
					where
						modder x = (mod n x) /= 0
						pssibleFactors = [2..(floor (sqrt (fromIntegral n)))]

--P32
gcf a b
	| b==0 = a
	| otherwise = gcd b $ mod a b

--P33
coprime a b = 1==gcf a b

--P34
phi 1 = 1
phi n = length ( filter ((==1) . (gcf n)) [1..(n-1)] )

--P35
factor' n ps 
	| (isPrime n) = [n]
	| otherwise  = f1:(factor' (quot n f1) ps) where f1 = (head $ filter ((==0).(mod n)) ps)

factor n = factor' n [p | p<-[2..(if (even n) then (quot n 2) else (quot (n-1) 2))], isPrime p]

--P36
factorsMult n = encode $ factor n

--P37
phi' n = product $ map (\(m,p) -> (p-1)*(p^(m-1))) $ factorsMult n

--P40
goldbach n = head [p1:[(n-p1)] | p1<-[p | p<-[2..(n-2)], isPrime p], isPrime (n-p1) ]

--P41
goldbachList i j = putStr $ foldr (++) [] [ foldr (++) [] [(show index), " = ",(show x)," + ",(show y),"\n" ]| index<-[i..j], even index, let (x:y:[]) = goldbach index]

--p95
nums = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
fullWords n = map (nums !!) $ toEng n where
	toEng 0 = []
	toEng x = (toEng $ div x 10)++[mod x 10]

--p96
isAlphaNum = (`elem` (['A'..'z']++['0'..'9']))

identifier id = if (head id) == '-' then False else identifier' id
identifier' [c] = isAlphaNum c
identifier' (c:cs)
	| isAlphaNum c = identifier' (cs)
	| c == '-' 			= if isAlphaNum (head cs) then identifier' cs else False
	
--p97
--This is with no outside help...
slicer i j [] = []
slicer i j l
	| i==j = [(l!!(i))]
	| i>j = l
	| i<j = ((take (j-i+1)).(drop (i))) l

newtype Sudoku = Sudoku {get :: [[Int]]} deriving (Eq, Show)

--utility function to show boards
showLst :: Show a => [a] -> IO ()
showLst = mapM_ (putStrLn.show)
	
--convert from string, 0's mark empty slots
--string is continuous integer with row after row on one line
convert st = Sudoku (convert' (map (fromChar) st)) where
	convert' [] = []
	convert' l = (take 9 l):(convert' (drop 9 l)) 
	fromChar x = DC.ord x - 48

--convert board to a list of columns
cols (Sudoku rs) = (D.transpose rs)

--get the square containing an index as a list
squareAt (Sudoku rs) row col = concat $ map (slicer indexCol (indexCol+2)) (slicer indexRow (indexRow+2) rs) where
	indexRow = 3*(div row 3)
	indexCol = 3*(div col 3)

--gets the indices of empty slots for a given board
empties :: Sudoku -> [(Int,Int)]
empties (Sudoku rs) = [(i,j) | i<-[0..8], j<-[0..8], (rs !! i) !! j == 0]

--list the possible entry values, based on the sudoku constraints, for a given board and index
possibles (Sudoku rs) (i,j) = [n | n<-[1..9], not (elem n (D.nub $ (rs!!i)++((cols (Sudoku rs))!!j)++(squareAt (Sudoku rs) i j) ) )]

--replaces an element in a list with the given element
replace :: a -> Int -> [a] -> [a]
replace el index list = (take index list)++(el:(drop (index+1) list))

--The second makeSudoku' makes a new board from the old by filling each empty slot with every possible entry that fits the constraints, creating multiple
--boards. The first makeSudoku makes a new board by filling each empty slot with the first possible entry that fits the constraints, but on hard puzzles
--it often leads to partial solutions with no way to move forward
{--=====================================================--}

--searches only one constant branch for solution
makeSudoku (Sudoku rs) 
	| null indOpts = (Sudoku rs)
	| otherwise	   = let {(i,j) = (fst . head) indOpts; n = head( (snd . head) indOpts)} in makeSudoku (Sudoku (replace (replace n j (rs !! i)) i rs)) where
		indOpts = mapIndOpts (Sudoku rs)

--nondeterministic search for solution
makeSudoku' (Sudoku rs) 
	| null indOpts = [(Sudoku rs)]
	| otherwise	   = let (i,j) = (fst . head) indOpts  in [Sudoku (replace (replace n j (rs !! i)) i rs) | n<-((snd . head) indOpts)] where
		indOpts = mapIndOpts (Sudoku rs)

--handles repeated monadic computation
shepard lst = shepard' 0 [lst]
shepard' (n) lst
	| any (null . empties) lst = lst >>= makeSudoku'
	| otherwise 			   = trace (show n) $ shepard' (n+1) (lst >>= makeSudoku')

--connect an index to the options that would fit the constraints at that index
-- list looks like this: [ ( index1 , [opt1, opt2,...] ),  ( index2 , [opt1, opt2,...] ), .... ( indexn , [opt1, opt2, ...] ) ]
mapIndOpts sud = dropWhile (\(i, l)->null l) $ sortOpts [(i, (possibles sud i)) | i<-(empties sud)]

--sorts the list created above by length of options
sortOpts = D.sortBy (compare')
compare' (ind1, l1) (ind2, l2)
	| length l1 < length l2 = LT
	| length l1 > length l2 = GT
	| otherwise 			= EQ

--usage: make a list of the entry puzzle when shepard is using nondeterministic makeSudoku'

solve = (get.head.(filter (null.empties).shepard))

sudokuComplete = Sudoku [[9,3,4,8,2,5,6,1,7],
			  [6,7,2,9,1,4,8,5,3],
			  [5,1,8,6,3,7,9,2,4],
			  [3,2,5,7,4,8,1,6,9],
			  [4,6,9,1,5,3,7,8,2],
			  [7,8,1,2,6,9,4,3,5],
			  [1,9,7,5,8,2,3,4,6],
			  [8,5,3,4,7,6,2,9,1],
			  [2,4,6,3,9,1,5,7,8]]
--easy puzzle			  
sudoku = Sudoku [[0,0,4,8,0,0,0,1,7],
			  [6,7,0,9,0,0,0,0,0],
			  [5,0,0,6,3,0,0,0,4],
			  [0,2,5,0,0,0,1,6,0],
			  [0,6,9,0,0,0,7,8,0],
			  [0,0,1,0,6,9,0,0,5],
			  [1,9,7,5,8,2,3,0,6],
			  [0,0,0,0,0,6,0,9,1],
			  [2,4,0,0,0,1,5,0,0]]

--hard puzzle
sudokuHard = convert "013020009040600000000003008700000534000090000824000006100700000000005070300080260"
sudokuHard2 = convert "000008000004050009892004010000005003007080600900200000010900546500020300000500000"
sudokuHardest = convert "800000000003600000070090200050007000000045700000100030001000068008500010090000400"

sudokuHSolution = Sudoku {get = 
	[[8,1,2,7,5,3,6,4,9],
	[9,4,3,6,8,2,1,7,5],
	[6,7,5,4,9,1,2,8,3],
	[1,5,4,2,3,7,8,9,6],
	[3,6,9,8,4,5,7,2,1],
	[2,8,7,1,6,9,5,3,4],
	[5,2,1,9,7,4,3,6,8],
	[4,3,8,5,2,6,9,1,7],
	[7,9,6,3,1,8,4,5,2]]}
	
res = solve sudokuHard