data PosState = PosEmpty | PosX | PosO deriving (Show) --States for the board

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
	| otherwise = "|" ++ char (b !! (i`mod`2-1) !! 0) ++
				  "|" ++ char (b !! (i`mod`2-1) !! 1) ++
				  "|" ++ char (b !! (i`mod`2-1) !! 2) ++
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
win? :: [[PosState]] -> IO()
win? [[PosX,_,_],[PosX,_,_],[PosX,_,_]] = 
--WORK MORE HERE
	
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
	let newBoard1 = replace board (m1 `div` 3) (m1 `mod` 3) PosX --Apply player's move
	--Win?
	--Print board
	--Get AI's move
	--Win?
	run newBoard2
	