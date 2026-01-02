module UIGame where 

import System.Random
import Parser
import Cell
import Grid
import Game
import Brick ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt, put, get
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, attrName, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import qualified Brick.Main as M 
import Brick.Widgets.Center as C
import Brick.Widgets.Border as B 
import qualified Brick.Widgets.Border.Style as BS
import Brick.Widgets.Core
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty 

-- data for the UI State
data UIState = GameCreation Difficulty
               | InGame Game Position FlagMod Message
               | EndGame Grid Message

-- data to represent the flagMod of a Game
data FlagMod = ON | OFF
    deriving Eq

-- type to represent an info message
type Message = String

-- Main app
app :: App UIState () ()
app = App { appDraw = drawUi
          , appHandleEvent = handleEvent  
          , appChooseCursor = neverShowCursor
          , appStartEvent = pure ()
          , appAttrMap = const theMap
          }

-- data for differents buttons
data UIButton = EasyButton
              | MediumButton
              | HardButton
              | ContinueButton
              | QuitButton
    deriving (Show,Eq,Ord)

-- Method to draw the app
drawUi :: UIState -> [Widget ()]
drawUi st@(GameCreation diff)= 
     [ C.center $
      vBox
        [ C.hCenter $ border $ str "Minesweeper"
        , padTopBottom 1 $ C.center $ hBox (map (drawDifficultyButton diff) [Easy, Medium, Hard])
        ]
    ]
drawUi st@(InGame (G grid _ _) pos fm mess) = [C.center $ padRight (Pad 2) $ vBox [drawGrid grid pos,drawMessage mess] <+> drawFlagMod fm]
drawUi st@(EndGame grid mess)  = [C.center $ 
                                    vBox 
                                        [ C.hCenter $ padBottom (Pad 2) $ border $ str "EndGame"
                                        , C.center $  vBox [C.hCenter $ drawGrid grid (-1,-1),C.center $ drawMessage mess]]]

drawDifficultyButton :: Difficulty -> Difficulty -> Widget ()
drawDifficultyButton selected d =
    let label = show d
        w = padLeftRight 2 $ border $ str label
    in if selected == d
       then withAttr (attrName "selected") w
       else w

-- Navigate between difficulties
nextDifficulty :: Difficulty -> Difficulty
nextDifficulty Hard = Easy
nextDifficulty d    = succ d

prevDifficulty :: Difficulty -> Difficulty
prevDifficulty Easy = Hard
prevDifficulty d    = pred d

-- Navigate between positions 
leftPosition :: Position -> Grid -> Position
leftPosition (x,y) g = case x - 1 == -1 of 
                        True  -> (length g -1,y)
                        False -> (x-1,y)

rightPosition :: Position -> Grid -> Position
rightPosition (x,y) g = case x + 1 == (length g) of
                        True  -> (0,y)
                        False -> (x+1,y)

topPosition :: Position -> Grid -> Position
topPosition (x,y) g = case y - 1 == -1 of 
                        True  -> (x,length (g !! 0) -1)
                        False -> (x,y-1)

bottomPosition :: Position -> Grid -> Position
bottomPosition (x,y) g = case y + 1 == (length (g !! 0)) of
                        True  -> (x,0)
                        False -> (x,y+1)

drawFlagMod :: FlagMod -> Widget ()
drawFlagMod fm = withAttr (attrName (if fm == ON then "onMod" else "offMod")) $ border $ str "F" 

drawMessage :: Message -> Widget ()
drawMessage m = if m /= "" then border $ str m else str m 

drawStats :: Game -> Widget () 
drawStats = undefined 

drawGrid :: Grid -> Position ->  Widget () 
drawGrid grid actualPos = withBorderStyle BS.unicodeBold 
    $ B.borderWithLabel (str "MinesWeeper")
    $ vBox rows 
    where 
        rows        = [hBox $ cells r | r <- [0..width-1]]
        cells       = \x -> [padLeftRight 1 $ drawPos (y,x) | y <- [0..height-1]]
        height      = length grid
        width       = length (grid !! 0)
        drawPos     = \pos -> if pos == actualPos then 
                padTopBottom 1 $ withAttr borderAttr $ border $ drawCell (getPosition pos grid)
            else 
                padTopBottom 1 $ drawCell $ getPosition pos grid

drawCell :: Cell -> Widget () 
drawCell (C _ Hidden)               = withAttr (attrName "hiddenAttr") $ str "   " 
drawCell (C _ Flagged)              = withAttr (attrName "flaggedAttr") $ str " F "
drawCell (C Bomb Discovered)        = withAttr (attrName "bombAttr") $ str " B "
drawCell (C (Empty 0) Discovered)   = withAttr (attrName "emptyAttr") $ str "   "
drawCell (C (Empty n) Discovered)   = withAttr (getAttr n) $ str (" " ++ show n ++ " ")

getAttr :: Int -> AttrName 
getAttr 1 = (attrName "blueAttr")
getAttr 2 = (attrName "greenAttr")
getAttr 3 = (attrName "redAttr")
getAttr 4 = (attrName "yellowAttr")
getAttr 5 = (attrName "cyanAttr")
getAttr 6 = (attrName "magentaAttr")
getAttr _ = (attrName "blackAttr")



theMap :: AttrMap 
theMap = attrMap defAttr 
    [(attrName "selected", defAttr `withStyle` bold)
    ,(attrName "onMod", red `on` white `withStyle` bold)
    ,(attrName "offMod", white `on` black)
    ,(borderAttr, fg yellow `withStyle` bold)
    ,(attrName "hiddenAttr", black `on` black)
    ,(attrName "flaggedAttr", red `on` white)
    ,(attrName "bombAttr", black `on` brightRed)
    ,(attrName "emptyAttr", fg brightWhite)
    ,(attrName "blueAttr", brightBlue `on` white)
    ,(attrName "greenAttr", green `on` white)
    ,(attrName "redAttr", brightRed `on` white)
    ,(attrName "yellowAttr", brightYellow `on` white)
    ,(attrName "cyanAttr", brightCyan `on` white)
    ,(attrName "magentaAttr", brightMagenta `on` white)
    ,(attrName "blackAttr", brightBlack `on` white)]


-- Method to handle event 
handleEvent ::  BrickEvent () () -> EventM () UIState ()
handleEvent (VtyEvent (EvKey key [])) = do 
    st <- get 
    gen <- getStdGen 
    case st of 
        (GameCreation diff) -> case key of 
                               KLeft  ->  put $ GameCreation (prevDifficulty diff)
                               KRight ->  put $ GameCreation (nextDifficulty diff) 
                               KEnter ->  put $ InGame (createGameWithDiff diff gen) (0,0) OFF ""
                               KEsc   ->  halt  
                               _      ->  return ()
        (InGame game@(G grid state diff) pos fm mess) -> case key of 
                                                 KLeft      -> put $ InGame game (leftPosition pos grid) fm mess
                                                 KRight     -> put $ InGame game (rightPosition pos grid) fm mess
                                                 KUp        -> put $ InGame game (topPosition pos grid) fm mess
                                                 KDown      -> put $ InGame game (bottomPosition pos grid) fm mess
                                                 KChar 'f'  -> put $ InGame game pos (if fm == ON then OFF else ON) mess

                                                 KChar 'e'  -> put $ EndGame grid "Test"

                                                 KEsc       -> put $ GameCreation Easy 
                                                 KEnter     -> case fm of 
                                                                ON  -> case getPosition pos grid of 
                                                                        (C _ Flagged) -> case unflagPosition pos game of 
                                                                                            Right game' -> put $ InGame game' pos fm ""
                                                                                            Left err    -> put $ InGame game pos fm err 
                                                                        _             -> case flagPosition pos game of 
                                                                                            Right game' -> put $ InGame game' pos fm "" 
                                                                                            Left err    -> put $ InGame game pos fm err 
                                                                OFF -> case discoverPosition pos game of 
                                                                        Right (G grid' Lost _)              -> put $ EndGame grid' "You lost this game...\nPress Enter to continue or Escape to stop !"
                                                                        Right game'@(G grid' Playing _)     -> case allDiscovered grid' of 
                                                                                                                True    -> put $ EndGame grid' "You won this game !\nPress Enter to continue or Escape to stop !"
                                                                                                                False   -> put $ InGame game' pos fm ""
                                                                        Left err                           -> put $ InGame game pos fm err
                                                 _          -> return ()
        (EndGame grid mess) -> case key of 
                                KEnter   -> put $ GameCreation Easy
                                KEsc     -> halt 
                                _        -> return ()
handleEvent _ = return ()
                                                 
                                                 


