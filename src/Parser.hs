-- File to parse the game commands
module Parser where

import Control.Applicative
import Control.Monad
import Grid 

-- The first part is inspired by my college lessons, i writed it again by myself but corrected it using lessons

-- Result type
type Result a = Maybe (a,String)

-- Parser data
data Parser a = MParser (String -> Result a)

-- Run the given parser
runParser :: Parser a -> String -> Result a 
runParser (MParser p) s = p s  

-- Eval a parser's result
evalParser :: Parser a -> String -> Maybe a 
evalParser p s = case runParser p s of 
                  Nothing    -> Nothing 
                  Just (r,_) -> Just r

-- Result of a parser
result :: Result a -> a 
result (Just (r,_)) = r 

-- Parser to get any character
anyChar :: Parser Char 
anyChar = MParser f 
    where 
        f ""    = Nothing
        f (c:s) = Just (c,s)

-- Functor instance for a Parser object 
instance Functor Parser where 
    fmap = liftM

-- Applicative instance for a Parser object
instance Applicative Parser where 
    pure v = MParser f 
            where 
                f str = Just (v,str)
    (<*>) = ap 

-- Alternative instance for a Parser object
instance Alternative Parser where 
    empty = MParser (\_ -> Nothing)

    p1 <|> p2 = MParser f 
                where 
                    f str = case runParser p1 str of 
                            Nothing -> runParser p2 str 
                            e       -> e 

-- Monad instance for a Parser object 
instance Monad Parser where 
    p >>= fp = MParser f 
            where 
                f str = case runParser p str of 
                        Nothing     -> Nothing 
                        Just (r,cs) -> runParser (fp r) cs 

-- Parser to get a character who matched with a given predicate 
carWhen :: (Char -> Bool) -> Parser Char 
carWhen pred = MParser f 
    where 
        f ""     = Nothing 
        f (x:xs) = if pred x then Just(x,xs) else Nothing

-- Parser to get a given character
car :: Char -> Parser Char 
car c = carWhen (== c)

-- Parser to get a given String
string :: String -> Parser String 
string ""     = pure ""
string (x:xs) = car x >> string xs >> pure (x:xs) 

-- The part below is all mine, i'll use parsers to create commands to play the main game

-- Predicate to know if a charactes is numeric
isNum :: Char -> Bool 
isNum c = c `elem` ['0'..'9']

-- Parser to get a number 
parseNumber :: Parser Int 
parseNumber = some (carWhen isNum) >>= \e -> pure (read e)

-- Parser to pass the potential blank
passBlank :: Parser () 
passBlank = many (car ' ') >> return ()

-- Parser to parse a Position 
parsePosition :: Parser Position
parsePosition = passBlank >> 
                parseNumber >>= \l ->
                passBlank >> 
                parseNumber >>= \m -> 
                return (l-1,m-1)

-- data for Command 
data Command = 
    Discover Position
    | Flag Position 
    | Unflag Position 
    | NewGame
    | Quit
    deriving Show

-- Parser to parse a Discover command
parseDiscover :: Parser Command 
parseDiscover = (string ":discover" <|> string ":d") >> parsePosition >>= \pos -> return (Discover pos)

-- Parser to parse a Flag command
parseFlag :: Parser Command 
parseFlag = (string ":flag" <|> string ":f") >> parsePosition >>= \pos -> return (Flag pos)

-- Parser to parse an Unflag command
parseUnflag :: Parser Command 
parseUnflag = (string ":unflag" <|> string ":u") >> parsePosition >>= \pos -> return (Unflag pos)

-- Parser to parse a NewGame command
parseNewGame :: Parser Command
parseNewGame = (string ":newgame" <|> string ":n") >> return NewGame

-- Parser to parse a Quit command
parseQuit :: Parser Command
parseQuit = (string ":quit" <|> string ":q") >> return Quit

-- Parser to parse any command 
parseCommand :: Parser Command 
parseCommand = parseDiscover <|> parseFlag <|> parseUnflag <|> parseNewGame <|> parseQuit

-- data to represent possibles difficulties
data Difficulty = Easy 
                  | Medium
                  | Hard 

-- Parser to parse the difficulty
parseDifficulty :: Parser Difficulty
parseDifficulty = (string "easy" >> return Easy) <|>
                  (string "medium" >> return Medium) <|>
                  (string "hard" >> return Hard)
