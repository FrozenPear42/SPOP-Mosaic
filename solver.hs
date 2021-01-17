module Solver where

-- Coordinate system
-- 0--------- x
-- |
-- |
-- |
-- |
-- y

-- (y, x)
type Position = (Int, Int)

data CellState = Empty | Filled | Untouched | Expanded deriving (Eq, Show)

type CellValue = Maybe Int

type Cell = (CellValue, CellState)

type Row = [Cell]

type Board = [Row]

type Triple t = (t, t, t)

type Neighborhood = Triple (Triple Cell)

fromJust:: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

blankCell :: Cell
blankCell = (Nothing, Untouched)

-- |Extracts cell state
getCellState :: Cell -> CellState
getCellState (_, state) = state

-- |Extracts cell value
getCellValue :: Cell -> Int
getCellValue (value, _) = fromJust value

-- |Returns a triple constructed by applying a function to all items in a triple
mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a1, a2, a3) = (f a1, f a2, f a3)


-- |Reads puzzle file into list of strings (list of lines)
-- Each character in line is represented by either number or '.'
readPuzzle :: String -> IO [String]
readPuzzle filename = do
  contents <- readFile filename
  let puzzle = read contents :: [String]
  return puzzle

-- |Parses list of strings into Board
createBoard :: [String] -> Board
createBoard = map parseRow
  where
    parseRow :: String -> Row
    parseRow [] = []
    parseRow (c : cs) = parseField c : parseRow cs

    parseField :: Char -> Cell
    parseField '.' = blankCell
    parseField c = (digitToInt c, Untouched)

    digitToInt :: Char -> Maybe Int
    digitToInt c = Just (fromEnum c - 48)


-- |Checks if position is in board bounds
positionValid :: Board -> Position -> Bool
positionValid board (y, x)
  | x < 0 || y < 0 || x >= width || y >= height = False
  | otherwise = True
  where
    height = length board
    width = length (head board)

-- |Gets cell at given position. If position is out of bounds
-- Expanded cell will be returned instead
getCell :: Board -> Position -> Cell
getCell board (y, x)
  | not (positionValid board (y, x)) = (Nothing, Expanded)
  | otherwise = (board !! y) !! x

-- |Updates board with new cell state at given position
setBoardCell :: Board -> Position -> CellState -> Board
setBoardCell board (y, x) state
  | positionValid board (y, x) =  take y board ++ setInRow (board !! y) : drop (y+1) board
  | otherwise = board
  where
    (value, _) = board !! y !! x

    setInRow :: Row -> Row
    setInRow row = take x row ++ ((value, state) : drop (x+1) row)


-- |Updates board with new cell state at given position
neighborhoodOfCell :: Board -> Position -> Neighborhood
neighborhoodOfCell board (y, x) = ((nw, n, ne), (w, c, e), (sw, s, se))
  where
    boardCell = getCell board
    nw = boardCell (y -1, x -1)
    n = boardCell (y -1, x)
    ne = boardCell (y -1, x + 1)
    w = boardCell (y, x -1)
    c = boardCell (y, x)
    e = boardCell (y, x + 1)
    sw = boardCell (y + 1, x -1)
    s = boardCell (y + 1, x)
    se = boardCell (y + 1, x + 1)

-- |Returns if neighborhood is valid
neighborhoodValid :: Neighborhood -> Bool
neighborhoodValid nbh =
  getCellValue c >= countCellsInNeighborhood Filled nbh && getCellValue c <= 9 - countCellsInNeighborhood Empty nbh - countCellsInNeighborhood Expanded nbh
  where
    (_, (_, c, _), _) = nbh

-- |Solves single neighborhood
processNeighborhood :: Neighborhood -> Neighborhood
processNeighborhood nbh
  | getCellValue c - numberOfFilled == numberOfUntouched = setStateOfUntouchedCellsInNeighborhood Filled nbh
  | getCellValue c == numberOfFilled = setStateOfUntouchedCellsInNeighborhood Empty nbh
  | otherwise = nbh
  where
    (_, (_, c, _), _) = nbh
    numberOfFilled = countCellsInNeighborhood Filled nbh
    numberOfUntouched = countCellsInNeighborhood Untouched nbh
    numberOfExpanded = countCellsInNeighborhood Expanded nbh
    numberOfEmpty = countCellsInNeighborhood Empty nbh


-- |Returns a count of cells with given state in neighborhood
countCellsInNeighborhood :: CellState -> Neighborhood -> Int
countCellsInNeighborhood state ((nw, n, ne), (w, c, e), (sw, s, se)) = countCellsInList state listOfCells
  where
    listOfCells = [nw, n, ne, w, c, e, sw, s, se]

-- |Returns a count of cells with given state in list
countCellsInList :: CellState -> [Cell] -> Int
countCellsInList _ [] = 0
countCellsInList cellState ((_, state):xs)
  | state == cellState = 1 + countCellsInList cellState xs
  | otherwise = countCellsInList cellState  xs

-- |Returns a cell with changed state if untouched
setStateOfUntouchedCell :: CellState -> Cell -> Cell
setStateOfUntouchedCell newState (value, state)
  | state == Untouched = (value, newState)
  | otherwise = (value, state)

-- |Returns a neighborhood with changed state of all untouched cells
setStateOfUntouchedCellsInNeighborhood :: CellState -> Neighborhood -> Neighborhood
setStateOfUntouchedCellsInNeighborhood newState nbh = mapTriple (mapTriple (setStateOfUntouchedCell newState)) nbh

-- |Updates board with neighborhood on given position
updateBoard :: Board -> Position -> Neighborhood -> Board
updateBoard board (y, x) ((nw,n,ne), (w,c,e), (sw,s,se)) =
  foldr setCellWrapped board cells
  where
    cells = [((y-1, x-1), getCellState nw), ((y-1, x), getCellState n), ((y-1, x+1), getCellState ne)]
         ++ [((y, x-1), getCellState w), ((y, x), getCellState c), ((y, x+1), getCellState e)]
         ++ [((y+1, x-1), getCellState sw), ((y+1, x), getCellState s), ((y+1, x+1), getCellState se)]

    setCellWrapped :: (Position, CellState) -> Board -> Board
    setCellWrapped (pos, state) board = setBoardCell board pos state


processCellAtPosition :: Position -> Board -> Board
processCellAtPosition pos board = resultBoard
  where
    neighborhood = neighborhoodOfCell board pos
    resultBoard = updateBoard board pos (processNeighborhood neighborhood)


-- |Finds cells with numbers on board
findNumberedCells :: Board -> [Position]
findNumberedCells board = find board 0
  where
    find :: Board -> Int -> [Position]
    find [] _ = []
    find (row : rest) y = findInRow row 0 y ++ find rest (y + 1)

    findInRow :: Row -> Int -> Int -> [Position]
    findInRow [] _ _ = []
    findInRow ((cellValue, _) : rest) x y
      | x < 0 = []
      | cellValue /= Nothing = (y, x) : findInRow rest (x + 1) y
      | otherwise = findInRow rest (x + 1) y


-- |Counts cells of given state on board
countCells :: CellState -> Board -> Int
countCells state board = sum countedRows
  where
    -- |Counts cells of given state in row
    countInRow :: Row -> Int
    countInRow [] = 0
    countInRow ((_, cellState):xs)
      | cellState == state = 1 + countInRow xs
      | otherwise = countInRow xs

    countedRows = map countInRow board

-- |Counts cells of given state on board
countFilledCells :: Board -> Int
countFilledCells = countCells Filled

-- |Counts untouched cells on board
countUntouchedCells :: Board -> Int
countUntouchedCells = countCells Untouched

-- |Checks if two boards are equal
boardsEqual :: Board -> Board -> Bool
boardsEqual bA bB = cntA == cntB
  where
    cntA = countFilledCells bA
    cntB = countFilledCells bB

-- |Checks board is valid
boardValid :: Board -> Bool
boardValid board = foldl reducer True numberedCells
  where
    numberedCells = findNumberedCells board

    reducer :: Bool -> Position -> Bool
    reducer False pos = False
    reducer True pos = cellValid board pos

    -- |Checks if cell's neighborhood is valid
    cellValid :: Board -> Position -> Bool
    cellValid board pos = neighborhoodValid n where
      n = neighborhoodOfCell board pos

-- |Checks if board is complete (board is solved)
boardCompleted :: Board -> Bool
boardCompleted board = countUntouchedCells board == 0

-- |Prints board in pretty way
printBoard :: Board -> IO ()
printBoard = mapM_ printRow
  where
    printRow row = putStrLn (map toChar row)

    toChar :: Cell -> Char
    toChar (Nothing, Untouched) = 'x'
    toChar (Just x, Untouched) = toEnum (x + 48)
    toChar (_, Filled) = '#'
    toChar (_, Empty) = '_'
    toChar (_, Expanded) = '-'

-- |returns position of any unchecked position if exists
findAnyUncheckedCell:: Board -> Maybe Position
findAnyUncheckedCell board = find board 0
  where
    find :: Board -> Int -> Maybe Position
    find [] _ = Nothing
    find (row : rest) i  | getIndexInRow row 0 /= Nothing = Just (i, fromJust(getIndexInRow row 0))
                                  | otherwise = find rest (i+1)

    getIndexInRow :: [Cell] -> Int -> Maybe Int
    getIndexInRow [] _                 = Nothing
    getIndexInRow ((_, state):xs) i | state == Untouched    = Just i
                                             | otherwise             = getIndexInRow xs (i + 1)

-- |Solves board
solve :: Maybe Board -> Maybe Board
solve board
  | board == Nothing = Nothing
  | not (boardValid newBoard) = Nothing
  | boardCompleted newBoard = Just newBoard
  | newBoard == fromJust board = tryUncheckedCell newBoard
  | otherwise = solve (Just newBoard)
  where
    step :: Board -> Board
    step board = foldr processCellAtPosition board niceCells
      where
        niceCells = findNumberedCells board

    newBoard = step $fromJust board

    tryUncheckedCell:: Board -> Maybe (Board)
    tryUncheckedCell board
      | boardWithFilledCell /= Nothing = boardWithFilledCell
      | otherwise = boardWithEmptyCell
      where
        uncheckedCell = findAnyUncheckedCell(board)
        boardWithFilledCell = solve ( Just (setBoardCell board (fromJust uncheckedCell) Filled))
        boardWithEmptyCell = solve ( Just (setBoardCell board (fromJust uncheckedCell) Empty))

main = do
  puzzle <- readPuzzle "puzzles/heart.txt"
  let board = createBoard puzzle
      solved = solve (Just board)
  printBoard (fromJust solved)