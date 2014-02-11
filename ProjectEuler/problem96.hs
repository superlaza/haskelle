import qualified Data.List as D
import qualified Data.Char as DC
import Debug.Trace
import System.IO
import Control.Monad

--24702, took about 9 minutes...

main :: IO ()
main = do
    withFile "sudoku.txt" ReadMode (\handle -> do  
        sudokuList <- replicateM 50 $ do
		hGetLine handle
		contents <- replicateM 9 $ hGetLine handle   
        	return $ concat contents
	--sequence $ map (showLst . solve . convert) (sudokuList)
	putStrLn $ show $ foldl (\sum sudStr -> sum + (let (x1:x2:x3:xs):ys=(solve.convert) sudStr in (x1*100+x2*10+x3))) 0 sudokuList
	) 


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