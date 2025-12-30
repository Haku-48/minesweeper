module UIGame where 

import Parser
import Game
import Brick 
import qualified Brick.Main as M 
import Brick.Widgets.Center as C
import Brick.Widgets.Border as B 
import Brick.Widgets.Core
import Graphics.Vty 

-- data for the UI State
data UIState = GameCreation Difficulty
               | InGame Game 
               | EndGame 

-- Main app
app :: App UIState () UIButton
app = App { s :: UIState
          , e :: ()
          , n :: UIButton  
          , appDraw = drawUi
          , appHandleEvent = handleEvent s
          , appChooseCursor = neverShowCursor
          , appStartEvent = return ()
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
drawUi :: UIState -> [Widget UIButton]
drawUi st@(GameCreation _)= 
    [vBox
        [C.hCenter 
            $ border
            $ str "Minesweeper"
        , fill ' '
        , drawDifficulty st
        , fill ' '
        , vLimit 3 $ hBox [
            drawQuitButton
            , fill ' '
            , drawContinueButton
        ]]]
drawUi st@(InGame g) = undefined 
drawUi st@(EndGame)  = undefined

drawDifficulty :: UIState -> Widget UIButton
drawDifficulty st =
    C.center 
        $ hBox
            [ drawDiffButton st Easy
            , drawDiffButton st Medium
            , drawDiffButton st Hard
            ]

drawDiffButton :: UIState -> Difficulty -> Widget UIButton
drawDiffButton (GameCreation diff) d = 
    let label = show d 
        w     = case d of 
                Easy    -> clickable EasyButton (padLeftRight 2 $ border $ str label)
                Medium  -> clickable MediumButton (padLeftRight 2 $ border $ str label)
                Hard    -> clickable HardButton (padLeftRight 2 $ border $ str label)
    in if diff == d 
        then withAttr (attrName "selected") w 
        else w

drawContinueButton :: Widget UIButton
drawContinueButton =
    clickable ContinueButton (border $ str "Continue")  

drawQuitButton :: Widget UIButton
drawQuitButton =
    clickable QuitButton (border $ str "Quit")

drawStats :: UIState -> Widget () 
drawStats = undefined 

drawGrid :: UIState -> Widget () 
drawGrid = undefined

theMap :: AttrMap 
theMap = attrMap defAttr 
    [(attrName "selected", white `on` black)]

-- Method to handle event 
handleEvent :: UIState -> BrickEvent UIButton () -> EventM UIButton UIState ()
handleEvent st (MouseDown name _ _ _) =
    case name of 
        EasyButton   -> put $ (GameCreation Easy)
        MediumButton -> put $ (GameCreation Medium)
        HardButton   -> put $ (GameCreation Hard)





ui :: Widget ()
ui = str "Hello, world!"

