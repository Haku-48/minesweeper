import Game
import Parser
import System.Random
import UIGame
import Brick

-- Main programm
main :: IO ()
--main = do 
--    gen <- getStdGen
--    (game,gen') <- createGame gen 
--    round <- playRound game gen' 
--    putStr "Thanks for playing !!!\n"
main = 
    getStdGen >>= \gen -> defaultMain app (GameCreation Easy gen) >> return ()

