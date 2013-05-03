--Author: Tyler Dougherty
-- This program is a relatively simple tic-tac-toe game.

--Imports
import Control.Concurrent
import System.Random

data PosState = PosEmpty | PosX | PosO deriving (Show, Eq) --States for the board

--AI
getMove :: [[PosState]] -> Int -> Int
getMove board val =
	if open board 5 --Try for middle
	then 5
	else if length a > 0
		then (a !! 0)
		else if length b > 0
			then b !! 0
			else ([x | x <- [1..9], open board x] !! (val-1)) --Random move
	where a = [x | x <- [1..9], open board x, checkWin (replace board ((x-1)`div`3) ((x-1)`mod`3) PosO) PosO] --Winning condition for AI
	      b = [x | x <- [1..9], open board x, checkWin (replace board ((x-1)`div`3) ((x-1)`mod`3) PosX) PosX] --Winning condition for player

--Returns the state at the given location
--  +Uses the 1-9 index system as input
stateAt :: [[PosState]] -> Int -> PosState
stateAt board a = board !! ((a-1)`div`3) !! ((a-1)`mod`3)

--Checks if the given move is open
open :: [[PosState]] -> Int -> Bool
open b a = stateAt b a == PosEmpty
	
--Converts a state into the appropriate character
char :: PosState -> String
char PosEmpty = " "
char PosX     = "X"
char PosO     = "O"

--Prints the given board
toStr :: [[PosState]] -> Int -> String
toStr b 7 = ""
toStr b i
	| even i = "+-+-+-+\n" ++ toStr b (i+1)
	| otherwise = "|" ++ char (b !! (i`div`2) !! 0) ++
				  "|" ++ char (b !! (i`div`2) !! 1) ++
				  "|" ++ char (b !! (i`div`2) !! 2) ++
				  "|\n" ++ toStr b (i+1)
				  
--Replaces an element in the list with a new value
replace :: [[PosState]] -> Int -> Int -> PosState -> [[PosState]]
replace l a b state =
	let x = l !! a in
		(take a l) ++ [(take b x) ++ [state] ++ (drop (b+1) x)] ++ (drop (a+1) l)

--Promts the user and gets their move
playerMove :: IO Int
playerMove = do
	putStr "-> "
	m <- getLine
	return(read m :: Int)
	
--Checks the board for winning moves, and if there is one, prints the end-game messages
-- Parameter two specifies whether the function is checking for player or AI wins
checkWin :: [[PosState]] -> PosState -> Bool
-- Player wins
checkWin [[PosX,_,_],[PosX,_,_],[PosX,_,_]] PosX = True
checkWin [[_,PosX,_],[_,PosX,_],[_,PosX,_]] PosX = True
checkWin [[_,_,PosX],[_,_,PosX],[_,_,PosX]] PosX = True
checkWin [[PosX,PosX,PosX],[_,_,_],[_,_,_]] PosX = True
checkWin [[_,_,_],[PosX,PosX,PosX],[_,_,_]] PosX = True
checkWin [[_,_,_],[_,_,_],[PosX,PosX,PosX]] PosX = True
checkWin [[PosX,_,_],[_,PosX,_],[_,_,PosX]] PosX = True
checkWin [[_,_,PosX],[_,PosX,_],[PosX,_,_]] PosX = True
-- AI wins
checkWin [[PosO,_,_],[PosO,_,_],[PosO,_,_]] PosO = True
checkWin [[_,PosO,_],[_,PosO,_],[_,PosO,_]] PosO = True
checkWin [[_,_,PosO],[_,_,PosO],[_,_,PosO]] PosO = True
checkWin [[PosO,PosO,PosO],[_,_,_],[_,_,_]] PosO = True
checkWin [[_,_,_],[PosO,PosO,PosO],[_,_,_]] PosO = True
checkWin [[_,_,_],[_,_,_],[PosO,PosO,PosO]] PosO = True
checkWin [[PosO,_,_],[_,PosO,_],[_,_,PosO]] PosO = True
checkWin [[_,_,PosO],[_,PosO,_],[PosO,_,_]] PosO = True
-- Other
checkWin _ _ = False

--Checks if the game has ended in a draw
checkDraw :: [[PosState]] -> Bool
checkDraw board = (checkSub (board !! 0)) && (checkSub (board !! 1)) && (checkSub (board !! 2))
checkSub :: [PosState] -> Bool
checkSub a = ((a !! 0) /= PosEmpty) && ((a !! 1) /= PosEmpty) && ((a !! 2) /= PosEmpty)

--Initialization
main :: IO()
main = do
	let board = take 3 (cycle [take 3 (cycle [PosEmpty])]) --Set up the starting state of the board
	putStrLn (take 30 (cycle "-"))
	putStrLn "Move locations:"
	putStrLn "+-+-+-+\n|1|2|3|\n+-+-+-+\n|4|5|6|\n+-+-+-+\n|7|8|9|\n+-+-+-+\n\n"
	run board

--Recursive loop running the game
run :: [[PosState]] -> IO()
run board = do
	putStrLn $ toStr board 0 --Print the board
	m1 <- playerMove --Get the player's move
	if stateAt board m1 == PosEmpty
	then do
		let newBoard1 = replace board ((m1-1) `div` 3) ((m1-1) `mod` 3) PosX --Apply player's move
		if checkWin newBoard1 PosX --Check for player winning condition
		then do --Win!
			putStrLn $ toStr newBoard1 0
			putStrLn "You win!"
		else do --Not a win!
			if checkDraw newBoard1
			then do
				putStrLn $ toStr newBoard1 0
				putStrLn "Draw!"
			else do
				putStrLn $ toStr newBoard1 0
				putStrLn "The AI is thinking..."
				let moves = [x | x <- [1..9], open newBoard1 x]
				random <- randomRIO (1,length moves)
				let m2 = getMove newBoard1 random --Get AI's move
				let newBoard2 = replace newBoard1 ((m2-1) `div` 3) ((m2-1) `mod` 3) PosO --Apply AI's move
				threadDelay 500000 --Wait for AI's move (unneeded, added for realism)
				if checkWin newBoard2 PosO --Check for AI winning condition
				then do
					putStrLn $ toStr newBoard2 0
					putStrLn "You lose!"
				else
					run newBoard2
	else do
		putStrLn "Invalid move."
		threadDelay 500000
		run board