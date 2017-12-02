

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe, listToMaybe, fromJust)
import           Text.Read  (readMaybe)

allTuples width height = [ ((x1,x2),G) | x1 <- [0..width], x2 <- [0..height]]

empti = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),G),((1,0),G),((1,1),G),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),G),((2,3),G),((3,0),G),((3,1),G),((3,2),G),((3,3),G)]

splitXrow = M.fromList [((0,0),X),((0,1),G),((0,2),X),((0,3),X),((1,0),G),((1,1),G),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),G),((2,3),G),((3,0),G),((3,1),G),((3,2),G),((3,3),G)]

vertMap = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),X),((1,0),G),((1,1),G),((1,2),G),((1,3),X),((2,0),G),((2,1),G),((2,2),G),((2,3),X),((3,0),G),((3,1),G),((3,2),G),((3,3),X)]

horzMap = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),G),((1,0),G),((1,1),G),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),G),((2,3),G),((3,0),X),((3,1),X),((3,2),X),((3,3),X)]

leftUpRight = M.fromList [((0,0),X),((0,1),G),((0,2),G),((0,3),G),((1,0),G),((1,1),X),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),X),((2,3),G),((3,0),G),((3,1),G),((3,2),G),((3,3),X)]

leftDownRight = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),X),((1,0),G),((1,1),G),((1,2),X),((1,3),G),((2,0),G),((2,1),X),((2,2),G),((2,3),G),((3,0),X),((3,1),G),((3,2),G),((3,3),G)]

pXPlay = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),X),((1,0),G),((1,1),G),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),G),((2,3),G),((3,0),G),((3,1),G),((3,2),G),((3,3),G)]

colZeroFull = M.fromList [((0,0),O),((0,1),X),((0,2),O),((0,3),X),((1,0),G),((1,1),G),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),G),((2,3),G),((3,0),G),((3,1),G),((3,2),G),((3,3),G)]

playerXTurn = Board 4 4 empti

playerOTurn = Board 4 4 pXPlay

colZeroFullBoard = Board 4 4 colZeroFull

diagWinner = Board 4 4 leftUpRight

-- Data Types ---------------------------------------------------

-- L represents checking Left when looking for diagonal wins, R for checking Right
data Dir = L | R
	deriving (Eq, Ord, Enum)
 
-- Player values for our Board map, Player X, O, and G which represents an unplayed board tile
data Player = X | O | G 
	deriving (Show, Eq, Ord, Enum)

-- Our game Board which holds the number of rows, columns 
-- and a map containing coordinates as keys with Players as values for each tile
data Board = Board 
	{ boardRows :: Integer 
	, boardColumns :: Integer 
	, boardTiles :: Map (Integer, Integer) Player 
	}

-- Applying Show to Board type 
instance Show Board where
    show board@(Board rows columns _) = unlines $
        [ concat [show i | i <- [0 .. columns - 1]]
        ] ++
        [ [showTile (get row column board) | column <- [0 .. columns - 1]]
        | row <- [0 .. rows - 1]
        ]


-- Functions ----------------------------------------------------


-- Dictates what to print given the Player in the tile
showTile :: Maybe Player -> Char
showTile Nothing  = ' '
showTile (Just X) = 'X'
showTile (Just O) = 'O'
showTile (Just G) = ' '


get :: Integer -> Integer -> Board -> Maybe Player
get row column = M.lookup (row, column) . boardTiles 


-- Creates new Board given row and colum number input 
chooseSizeBoard :: Integer -> Integer -> Board
chooseSizeBoard rows columns = Board rows columns (M.fromList (allTuples columns rows))


getTiles :: Board -> Map (Integer,Integer) Player
getTiles board = (boardTiles board)


-- Return map of only tiles with given Player from given Board
filterPlayers :: Map (Integer,Integer) Player -> Player -> Map (Integer,Integer) Player
filterPlayers board player = M.filter (== player) board


getColumn :: Map (Integer,Integer) Player -> Integer -> Map (Integer,Integer) Player
getColumn board row = M.filterWithKey (\(c,r) _ -> r == row) board


getRow :: Map (Integer,Integer) Player -> Integer -> Map (Integer,Integer) Player
getRow board col = M.filterWithKey (\(c,r) _ -> c == col) board


-- Given a row from the board in the from of a list, see if you can find 4 in a row the same as given player 
checkLine :: [((Integer,Integer), Player)] -> Player -> Bool
checkLine [] p = False
checkLine (((a1,b1),p1):((a2,b2),p2):((a3,b3),p3):((a4,b4),p4):r) p 
	|  p1==p && p2==p && p3==p && p4==p = True
	| otherwise = checkLine (((a2,b2),p2):((a3,b3),p3):((a4,b4),p4):r) p 
checkLine (x:t) p = False


-- recurse over each row 
horizontal :: Board -> Player -> Integer -> Bool
horizontal _ _ 0 = False
horizontal board player totalRowNum
-- checkLine is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkLine (M.toList (getRow (boardTiles board) totalRowNum)) player = True
	| otherwise = horizontal board player (totalRowNum - 1)


-- recurse over each row ** Probably have to change the fact that im filtering out one player 
vertical :: Board -> Player -> Integer -> Bool
vertical _ _ 0 = False
vertical board player totalColNum
-- checkLine is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkLine (M.toList (getColumn (boardTiles board) totalColNum)) player = True
	| otherwise = vertical board player (totalColNum - 1)


diagonal :: Board -> Player -> Integer -> Bool
diagonal _ _ 0 = False
diagonal board player totalRowNum
-- checkDiag is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkDiag (M.toList (filterPlayers (boardTiles board) player) ) player board = True
	| otherwise = diagonal board player (totalRowNum - 1)

-- Check for edge cases, chose a direction to look diagonally and see if a diagonal line exists with the given player, and Board
checkDiag :: [((Integer,Integer), Player)] -> Player -> Board -> Bool 
checkDiag [] _ _ = False
checkDiag (((y,x),z):t) p board 
	| x == 0 && verRight (y,x) p (boardTiles board) = hasDiagonal ((y,x), z) board 3 (Just R)
	| x == ((boardColumns board)-1) && verLeft (y,x) p (boardTiles board) = hasDiagonal ((y,x), z) board 3 (Just L)
	| (verRight (y,x) p (boardTiles board)) = hasDiagonal ((y,x), z) board 3 (Just R)
	| (verLeft (y,x) p (boardTiles board)) = hasDiagonal ((y,x), z) board 3 (Just L)
	| otherwise = False


-- Recurse to see if given tile has a same player tile in the given upper diagonal direction in Board
hasDiagonal :: ((Integer, Integer), Player) -> Board -> Integer -> Maybe Dir -> Bool
hasDiagonal _ _ 0 _ = True
hasDiagonal ((x,y),z) board n dir
	| dir == (Just R) && verRight (x,y) z (boardTiles board) = hasDiagonal ((x+1, y+1), z) board (n-1) (Just R)
	| dir == (Just L) && verLeft (x,y) z (boardTiles board) = hasDiagonal ((x-1, y+1), z) board (n-1) (Just L)
	| otherwise = False


-- Check if upper right Diagonal tile has same Player as p
verRight :: (Integer, Integer) -> Player -> Map (Integer,Integer) Player -> Bool
verRight (x,y) p boardTiles
	| (M.lookup (x+1,y+1) boardTiles) == Just p = True
	| otherwise = False


-- Check if upper left Diagonal tile has same Player as p
verLeft :: (Integer, Integer) -> Player -> Map (Integer,Integer) Player -> Bool
verLeft (x,y) p boardTiles
	| (M.lookup (x-1,y+1) boardTiles) == Just p = True
	| otherwise = False


-- Check all possible options for a win given a board and a Player
check :: Board -> Player -> Bool 
check board player 
	| horizontal board player (boardColumns board) = True
	| vertical board player (boardRows board) = True
	| diagonal board player (boardColumns board) = True
	| otherwise = False

 
whoWon :: Board -> Int
whoWon board 
    | check board X = 1
    | check board O = 2
    | otherwise = 0


whosTurn :: Board -> Player
whosTurn board 
	| (pXPlays board) == (pOPlays board) = X
    | otherwise = O


pXPlays :: Board -> Int
pXPlays board = foldr (\x y -> 1 + y) 0 (M.toList (filterPlayers (boardTiles board) X))

pOPlays :: Board -> Int
pOPlays board = foldr (\x y -> 1 + y) 0 (M.toList (filterPlayers (boardTiles board) O))


getMove :: Player -> IO Integer
getMove player = 
	do
		putStr (show player)
		putStrLn ", please make your move!"
		col <- getLine
		case readMaybe col :: Maybe Integer of
			Just x -> return x
			Nothing -> putStrLn "Invalid number entered" >> getMove player

isLegalMove :: Board -> Integer -> IO Bool
isLegalMove board col = 
	do
		let valid = (boardColumns board > col) && hasFreeSpace board col
		putStrLn (moveReply valid)
		return valid

hasFreeSpace :: Board -> Integer -> Bool
hasFreeSpace board col 
	| (M.lookup (0, col) (boardTiles board)) == Just G = True
	| (M.lookup (0, col) (boardTiles board)) == Just X = False
	| (M.lookup (0, col) (boardTiles board)) == Just O = False
	| otherwise = False

moveReply :: Bool -> String 
moveReply valid 
	| valid = ""
	| otherwise = "Illegal Move! Try again... "

placeMove :: Board -> Integer -> Player -> Board 
placeMove board col player = Board (boardRows board) (boardColumns board) (M.insert (row,col) player (boardTiles board))
	where row = nextFreeRow board player col

nextFreeRow :: Board -> Player -> Integer -> Integer
nextFreeRow board player column = findFreeRow board column ((boardRows board)-1)

findFreeRow :: Board -> Integer -> Integer -> Integer  
findFreeRow _ _ 0 = 0
findFreeRow board column row 
    | (M.lookup (row, column) (boardTiles board)) == Just G = row
    | (M.lookup (row, column) (boardTiles board)) == Just X = findFreeRow board column (row-1)
    | (M.lookup (row, column) (boardTiles board)) == Just O = findFreeRow board column (row-1)
	| otherwise = findFreeRow board column (row-1)


-- GameLoop -------------------------------------------------------------------------------

go :: Board -> IO ()
go board
	| whoWon board == 1 = putStrLn "Player One Wins!"
    | whoWon board == 2 = putStrLn "Player Two Wins!"
    | otherwise = do 
        col <- getMove (whosTurn board)
        legalMove <- isLegalMove board col
        let nextBoard  
        	| legalMove = placeMove board col (whosTurn board)
        	| otherwise = board
        putStrLn ("\n" ++ show nextBoard)
        go nextBoard


main :: IO ()
main = 
	go (chooseSizeBoard 7 6)







