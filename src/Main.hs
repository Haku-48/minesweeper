import Game


-- Main programm
main :: IO ()
main = do 
    game <- createGame
    round <- playRound game 
    putStr "Thanks for playing !!!\n"