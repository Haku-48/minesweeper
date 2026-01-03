module Grid where

import System.Random
import Cell
import ShuffleList

-- The type for a Cell position
type Position = (Int, Int)

-- The type for a Grid
type Grid = [[Cell]]


-- Method to show a Cell grid 
showGrid :: Grid -> String 
showGrid = unlines . map showRow

-- Method to replicate an object
replicateObj :: Int -> a -> [a]
replicateObj 0 _ = []
replicateObj n a = a : replicate (n-1) a 

-- Methods to get a random list of position
positions :: Int -> Int -> [Position]
positions i j = concat [[(x,y) | x <- [0..i-1]] | y <- [0..j-1]]

randomList :: StdGen -> Int -> Int -> ([Position],StdGen)
randomList gen i j = shuffle gen (positions i j)

-- Method to generate a full empty Grid
emptyGrid :: Int -> Int -> Grid
emptyGrid n m = replicateObj n (replicateObj m (C (Empty 0) Hidden)) 

-- Get a Cell with his position
getInRow :: Int -> [Cell] -> Cell 
getInRow 0 (x:_)  = x 
getInRow n (_:xs) = getInRow (n-1) xs 
getInRow _ []     = error "Wrong index !"

getPosition :: Position -> Grid -> Cell
getPosition (0,m) (row:_)  = getInRow m row  
getPosition (n,m) (_:rows) = getPosition (n-1,m) rows 
getPosition _ []           = error "Wrong index !" 

-- Place one bomb on the given position
placeInRow :: Int -> [Cell] -> [Cell]
placeInRow 0 (_:xs) = C Bomb Hidden:xs -- A changer en Hidden/Discover pour les tests
placeInRow m (x:xs) = x : placeInRow (m-1) xs 

placeOneBomb :: Position -> Grid -> Grid 
placeOneBomb (0,m) (l:ls) = (placeInRow m l:ls)
placeOneBomb (n,m) (l:ls)      = l : placeOneBomb (n-1,m) ls 

-- Method to place randomly some bombs on the grid
placeBombs :: Int -> StdGen -> [Position] -> Grid -> Grid 
placeBombs 0 _ _ grid = grid 
placeBombs n gen (x:xs) grid = placeBombs (n-1) gen xs (placeOneBomb x grid) 

-- Method to get all the neighboors of a Cell with a given position
getNeighbors :: Position -> Grid -> [Position]
getNeighbors (i,j) grid = filter (\(x,y) -> x `elem` [0..(length grid)-1] && y `elem` [0..(length (grid !! 0))-1]) 
    $ concat [[(x,y) | x <- [i-1..i+1]]| y <- [j-1..j+1]]

-- Method to set the number on a cell depending the number of bombs in his neighborhood
setInRow :: Int -> Int -> [Cell] -> [Cell]
setInRow 0 n (_:xs) = C (Empty n) Hidden:xs -- A changer en Hidden/Discover pour les tests
setInRow m n (x:xs) = x : setInRow (m-1) n xs

setNumber :: Position -> Int -> Grid -> Grid 
setNumber (0,m) n (l:ls) = case getPosition (0,m) (l:ls) of 
                            C Bomb _ -> (l:ls)
                            _        -> (setInRow m n l:ls)
setNumber (k,m) n (l:ls) = case getPosition (k,m) (l:ls) of 
                            C Bomb _ -> (l:ls)
                            _        -> l : setNumber (k-1,m) n ls

-- Method to know the number of bombs in a neighborhood of a position
numberOfBombsInNeigh :: Position -> Grid -> Int 
numberOfBombsInNeigh pos grid = foldl f 0 (getNeighbors pos grid)
    where
        f acc x = case getPosition x grid of 
                    C Bomb _ -> acc + 1
                    _        -> acc 

-- Method to set all the numbers in a Row
setAllNumber :: [Position] -> Grid -> Grid  
setAllNumber [] grid     = grid 
setAllNumber (x:xs) grid = setAllNumber xs grid' 
    where 
        grid' = (setNumber x (numberOfBombsInNeigh x grid) grid)

-- Method to generate a random Grid
generateGrid :: Int -> Int -> Int -> StdGen -> (Grid,StdGen) 
generateGrid i j n gen = (setAllNumber pos 
                        $ placeBombs n gen pos 
                        $ emptyGrid i j, gen')
    where (pos,gen') = randomList gen i j

-- Methods to discover a Pos
discoverInRow :: Int -> [Cell] -> [Cell]
discoverInRow 0 (x:xs) = case discoverCell x of 
                         Right cell -> cell:xs 
                         _          -> x:xs
discoverInRow n (x:xs) = x : discoverInRow (n-1) xs 

discoverPos :: Position -> Grid -> Grid
discoverPos (0,m) (l:ls) = discoverInRow m l : ls
discoverPos (n,m) (l:ls) = l : discoverPos (n-1,m) ls

discoverNeighbors :: Position -> Grid -> Grid 
discoverNeighbors pos grid = discoverNeighborsAux (getNeighbors pos grid) [] (discoverPos pos grid)

discoverNeighborsAux :: [Position] -> [Position] -> Grid -> Grid 
discoverNeighborsAux [] _ g = g 
discoverNeighborsAux (x:xs) seen g | x `elem` seen = discoverNeighborsAux xs seen (discoverPos x g) 
                                   | otherwise     = case getPosition x g of 
                                                    C (Empty 0) Hidden -> let g' = discoverPos x g 
                                                                          in discoverNeighborsAux (xs ++ getNeighbors x g') (x:seen) g'
                                                    C (Empty _) Hidden -> discoverNeighborsAux xs (x:seen) (discoverPos x g)
                                                    _                  -> discoverNeighborsAux xs (x:seen) g

discover :: Position -> Grid -> Grid 
discover pos grid = case getPosition pos grid of 
                    C (Empty 0) Hidden -> discoverNeighbors pos grid 
                    _                  -> discoverPos pos grid 

-- Methods to flag/unflag a Pos
flagInRow :: Int -> [Cell] -> [Cell]
flagInRow 0 (x:xs) = case flagCell x of 
                     Right cell -> cell:xs
                     _          -> x:xs 
flagInRow n (x:xs) = x : flagInRow (n-1) xs 

unflagInRow :: Int -> [Cell] -> [Cell]
unflagInRow 0 (x:xs) = case unflagCell x of 
                       Right cell -> cell : xs 
                       _          -> x:xs 
unflagInRow n (x:xs) = x : unflagInRow (n-1) xs

flagPos :: Position -> Grid -> Grid 
flagPos (0,m) (l:ls) = flagInRow m l : ls 
flagPos (n,m) (l:ls) = l : flagPos (n-1,m) ls

unflagPos :: Position -> Grid -> Grid 
unflagPos (0,m) (l:ls) = unflagInRow m l : ls 
unflagPos (n,m) (l:ls) = l : unflagPos (n-1,m) ls

-- Method to know if all the empty cells are discovered
allDiscovered :: Grid -> Bool 
allDiscovered [[]]        = True 
allDiscovered ([]:gs)     = allDiscovered gs 
allDiscovered ((x:xs):gs) = case x of 
                            C (Empty _) Hidden -> False
                            _                  -> allDiscovered (xs:gs) 

-- Flag all the bombs cell at the end 
flagBombInRow :: [Cell] -> [Cell]
flagBombInRow []                   = []
flagBombInRow ((C Bomb Hidden):ls) = (C Bomb Flagged): flagBombInRow ls 
flagBombInRow (c:ls)               = c : flagBombInRow ls 

flagBomb :: Grid -> Grid 
flagBomb []     = []
flagBomb (l:ls) = flagBombInRow l : flagBomb ls 

