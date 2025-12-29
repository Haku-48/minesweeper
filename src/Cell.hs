module Cell where

-- Data for Cell content
data CellContent = Bomb | Empty Int
-- Data for Cell state
data CellState = Hidden | Discovered | Flagged

-- Data for Cell object
data Cell = C CellContent CellState 

instance Show Cell where 
    show(C _ Hidden)              = "#"
    show(C _ Flagged)             = "F"
    show(C Bomb Discovered)       = "B"
    show(C (Empty 0) Discovered)  = "."
    show(C (Empty a) Discovered)  = show a 

-- Method to show a Cell row
showRow :: [Cell] -> String
showRow = unwords . map show

-- Method to flag a Cell 
flagCell :: Cell -> Either String Cell 
flagCell (C _ Flagged)      = Left "This Cell is already flagged !"
flagCell (C cont Hidden)    = Right (C cont Flagged)
flagCell (C _ Discovered)   = Left "This Cell is discovered and cannot be flagged !"

-- Method to unflag a Cell
unflagCell :: Cell -> Either String Cell 
unflagCell (C cont Flagged) = Right (C cont Hidden)
unflagCell (C _ Hidden)     = Left "This Cell is not flagged !"
unflagCell (C _ Discovered) = Left "This Cell is discovered and cannot be unflagged !"

-- Method to discover a Cell
discoverCell :: Cell -> Either String Cell
discoverCell (C cont Hidden)  = Right (C cont Discovered)
discoverCell (C _ Discovered) = Left "This Cell is already discovered !"
discoverCell (C _ Flagged)    = Left "This Cell is flagged and cannot be discovered !"

