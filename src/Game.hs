module Game where 

import System.Random
import System.IO
import Grid
import Cell
import Parser

-- The data for a Game state
data GameState = Won | Lost | Playing

-- The data for a Game
data Game = G Grid GameState Difficulty

-- Method to discover a Cell of the Game's Grid with a given position
discoverPosition :: Position -> Game -> Either String Game 
discoverPosition _ (G _ Won _)          = Left "Already won this game !"
discoverPosition _ (G _ Lost _)         = Left "Already lost this game !"
discoverPosition pos (G grid Playing d) = case discoverCell $ getPosition pos grid of
                                        Right cell -> case cell of 
                                                    C Bomb _ -> Right $ G (discover pos grid) Lost d
                                                    _        -> Right $ G (discover pos grid) Playing d
                                        Left err   -> Left err

-- Method to flag a Cell of the Game's Grid with a given position
flagPosition :: Position -> Game -> Either String Game 
flagPosition _ (G _ Won _)              = Left "Already won this game !"
flagPosition _ (G _ Lost _)             = Left "Already lost this game !"
flagPosition pos (G grid Playing d)     = case flagCell $ getPosition pos grid of 
                                        Right cell -> Right $ G (flagPos pos grid) Playing d
                                        Left err   -> Left err

-- Method to unflag a Cell of the Game's Grid with a given position
unflagPosition :: Position -> Game -> Either String Game 
unflagPosition _ (G _ Won _)            = Left "Already won this game !"
unflagPosition _ (G _ Lost _)           = Left "Already lost this game !"
unflagPosition pos (G grid Playing d)   = case unflagCell $ getPosition pos grid of 
                                        Right cell -> Right $ G (unflagPos pos grid) Playing d
                                        Left err   -> Left err

-- Method to run a Game command 
runGameCommand :: Command -> Game -> Either String Game 
runGameCommand (Discover pos) game = discoverPosition pos game 
runGameCommand (Flag pos) game     = flagPosition pos game 
runGameCommand (Unflag pos) game   = unflagPosition pos game

-- Method to choose a Difficulty
chooseDifficulty :: IO Difficulty
chooseDifficulty = do 
    _    <- putStr "Possible difficulties -> easy; medium; hard\n"
    _    <- putStr "Choose Difficulty > "
    hFlush stdout
    line <- getLine
    dif  <- case evalParser parseDifficulty line of 
            Just d  -> return d
            Nothing -> do 
                        _ <- putStr "Error while parsing difficulty ! Please retry !\n"
                        chooseDifficulty
    return dif

-- Create a new Game 
createGame :: IO Game
createGame = do 
    gen  <- getStdGen
    _    <- putStrLn "Welcome in Minesweeper"
    dif  <- chooseDifficulty
    game <- case dif of 
            Easy       -> return $ G (generateGrid 8 8 8 gen) Playing Easy
            Medium     -> return $ G (generateGrid 10 9 15 gen) Playing Medium 
            Hard       -> return $ G (generateGrid 14 9 27 gen) Playing Hard
    return game

-- Method to show every command
showCommands :: IO ()
showCommands = putStrLn "Every Commands -> :discover [x] [y]; :flag [x] [y]; :unflag [x] [y]; :newgame; :quit" >> putStrLn "Or use :d; :f; :u; :n; :q"


-- Method that represent a round in the game 
playRound :: Game -> IO ()
playRound (G grid Won _)        = do 
    _       <- putStrLn (showGrid grid)
    _       <- putStr "You won this game ! Congratulation ! Starting new game...\n"
    newGame <- createGame
    playRound newGame 
playRound (G grid Lost _)       = do
    _       <- putStrLn (showGrid grid) 
    _       <- putStr "You lost this game ! Starting new game...\n"
    newGame <- createGame
    playRound newGame 
playRound g@(G grid Playing _)  = do 
    _       <- putStrLn (showGrid grid) 
    _       <- showCommands
    _       <- putStr "Command > "
    hFlush stdout 
    line    <- getLine
    g'      <- case evalParser parseCommand line of 
                Just c    -> case c of 
                             Quit    -> return ()
                             NewGame -> do 
                                        game <- createGame
                                        playRound game 
                             e       -> do 
                                        game <- case runGameCommand e g of 
                                                Right g'@(G grid' _ d) -> case allDiscovered grid' of 
                                                                         True  -> playRound (G grid' Won d)
                                                                         False -> playRound g'
                                                Left err               -> do 
                                                                         _ <- putStrLn ("Error : " ++ err)
                                                                         playRound g
                                        return ()
                              
                Nothing   -> do 
                            putStr "Error while parsing command\n"
                            playRound g 
    return ()
    