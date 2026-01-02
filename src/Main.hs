import Game
import Parser
import UIGame
import Brick

-- Main programm
main :: IO ()
--main = do 
--    game <- createGame
--    round <- playRound game 
--    putStr "Thanks for playing !!!\n"
main = 
    defaultMain app (GameCreation Easy) >> return ()

