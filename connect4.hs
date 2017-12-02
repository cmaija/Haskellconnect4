import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe, listToMaybe, fromJust)
import           Text.Read  (readMaybe)
import           System.Random

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
-- allTuples returns a list of tuples where the player
allTuples width height = [ ((x1,x2),G) | x1 <- [0..width], x2 <- [0..height]]

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
-- checkHoriz is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkLine (M.toList (getRow (boardTiles board) totalRowNum)) player = True
	| otherwise = horizontal board player (totalRowNum - 1)


-- recurse over each row ** Probably have to change the fact that im filtering out one player 
vertical :: Board -> Player -> Integer -> Bool
vertical board player 0 = checkLine (M.toList (getColumn (boardTiles board) 0)) player
vertical board player totalColNum
-- checkLine is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkLine (M.toList (getColumn (boardTiles board) totalColNum)) player = True
	| otherwise = vertical board player (totalColNum - 1)


diagonal :: Board -> Player -> Integer -> Bool
diagonal _ _ 0 = False
diagonal board player totalRowNum
-- checkHoriz is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkDiag (M.toList (boardTiles board) ) player board = True
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

 -- Check to see if player 1 or player 2 won, if no one has won, return 0
whoWon :: Board -> Int
whoWon board 
    | check board X = 1
    | check board O = 2
    | otherwise = 0

-- Check to see if the game is a draw (i.e. the board is full)
isDraw :: Board -> Bool
isDraw board
    | (numPlays board) >= (numSpaces board) = True
    | otherwise = False

-- Get the number of total plays so far
numPlays :: Board -> Integer
numPlays board = (pXPlays board) + (pOPlays board)

-- Get the total playable space on the board
numSpaces :: Board -> Integer 
numSpaces board = (boardColumns board) * (boardRows board)

-- Get the player who needs to play next
whosTurn :: Board -> Player
whosTurn board 
	| (pXPlays board) == (pOPlays board) = X
    | otherwise = O

-- Get number of plays Player 1 has made
pXPlays :: Board -> Integer
pXPlays board = foldr (\x y -> 1 + y) 0 (M.toList (filterPlayers (boardTiles board) X))

-- Get number of plays Player 2 has made
pOPlays :: Board -> Integer
pOPlays board = foldr (\x y -> 1 + y) 0 (M.toList (filterPlayers (boardTiles board) O))


-- Get the next players move for the one player game
getMoveAiGame :: Player -> Board -> IO Integer
getMoveAiGame player board  
	| player == X = 
		do
		    putStr (show player)
		    putStrLn ", please make your move!"
		    col <- getLine
		    case readMaybe col :: Maybe Integer of
			    Just x -> return x
			    Nothing -> putStrLn "Invalid number entered" >> getMoveAiGame player board
    | otherwise = 
    	do 
    		getAiMove board

-- Compute the next move that the AI will make 
getAiMove :: Board -> IO Integer
getAiMove board = 
	do
		tile <- getPlayerOneTile board
		col <- getCol tile
		return col

-- Get the column from a given tile
getCol :: ((Integer, Integer), Player) -> IO Integer
getCol ((x, y), player) = do 
	return y

-- Get the rightmost tile Player 1 has placed 
getPlayerOneTile :: Board -> IO ((Integer, Integer), Player)
getPlayerOneTile board = do
    return (last (M.toList (filterPlayers (boardTiles board) X)))

-- Get the current players next move
getMove :: Player -> IO Integer
getMove player = 
	do
		putStr (show player)
		putStrLn ", please make your move!"
		col <- getLine
		case readMaybe col :: Maybe Integer of
			Just x -> return x
			Nothing -> putStrLn "Invalid number entered" >> getMove player 

-- Check if a move is legal
isLegalMove :: Board -> Integer -> IO Bool
isLegalMove board col = 
	do
		let valid = (boardColumns board > col) && hasFreeSpace board col
		putStrLn (moveReply valid)
		return valid

-- Check if the given column has a free space
hasFreeSpace :: Board -> Integer -> Bool
hasFreeSpace board col 
	| (M.lookup (0, col) (boardTiles board)) == Just G = True
	| (M.lookup (0, col) (boardTiles board)) == Just X = False
	| (M.lookup (0, col) (boardTiles board)) == Just O = False
	| otherwise = False

-- Prints the reply for a valid or non valid move 
moveReply :: Bool -> String 
moveReply valid 
	| valid = ""
	| otherwise = "Illegal Move! Try again... "

-- Take the current players move and update the board accordingly
placeMove :: Board -> Integer -> Player -> Board 
placeMove board col player = Board (boardRows board) (boardColumns board) (M.insert (row,col) player (boardTiles board))
	where row = nextFreeRow board player col

-- Get the next unoccupied row in a given column
nextFreeRow :: Board -> Player -> Integer -> Integer
nextFreeRow board player column = findFreeRow board column ((boardRows board)-1)

-- Get the first row that does not contain a played tile in a given column 
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
    | isDraw board = putStrLn "It's a draw!"
    | otherwise = do 
        col <- getMove (whosTurn board) 
        legalMove <- isLegalMove board col
        let nextBoard  
        	| legalMove = placeMove board col (whosTurn board)
        	| otherwise = board
        putStrLn ("\n" ++ show nextBoard)
        go nextBoard


goAi :: Board -> IO ()
goAi board
    | whoWon board == 1 = putStrLn "Player One Wins!"
    | whoWon board == 2 = putStrLn "Player Two Wins!"
    | isDraw board = putStrLn "It's a draw!"
    | otherwise = do 
        col <- getMoveAiGame (whosTurn board) board
        legalMove <- isLegalMove board col
        let nextBoard  
        	| legalMove = placeMove board col (whosTurn board)
        	| otherwise = board
        putStrLn ("\n" ++ show nextBoard)
        goAi nextBoard

-- To play the game, call main
main :: IO ()
main = do
    putStrLn "One player: Type '1'"
    putStrLn "Two players: Type '2'"
    let numPlayers = 0
    players <- getLine
    case readMaybe players :: Maybe Integer of
		Just x -> do
			if x == 2 then go (chooseSizeBoard 7 6) else goAi (chooseSizeBoard 7 6)
		Nothing -> putStrLn "Invalid number entered" >> main 
    







