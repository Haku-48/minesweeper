module ShuffleList where 

import System.Random

-- Method to shuffle a List
-- Only used for the Random Grid creation
shuffle :: StdGen -> [a] -> ([a],StdGen)
shuffle gen [] = ([],gen)
shuffle gen xs = go gen xs [] 
  where 
    go g [] acc = (acc,g)
    go g ys acc = 
        let (i,g') = randomR (0,length ys -1) g 
            (y,ys') = removeAt i ys 
        in go g' ys' (y:acc)

removeAt :: Int -> [a] -> (a,[a])
removeAt i xs = (xs !! i, take i xs ++ drop (i+1) xs) 