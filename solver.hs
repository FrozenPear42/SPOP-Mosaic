module Main where

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

type Filter = Triple (Triple Bool)

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

isJust :: Maybe a -> Bool
isJust (Just a) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing m = not (isJust m)

blankCell :: Cell
blankCell = (Nothing, Untouched)

-- | Extracts cell state
getCellState :: Cell -> CellState
getCellState (_, state) = state

-- | Extracts cell value
getCellValue :: Cell -> Maybe Int
getCellValue (value, _) = value

-- | Returns a triple constructed by applying a function to all items in a triple
mapTriple :: (a -> b) -> Triple a -> Triple b
mapTriple f (a1, a2, a3) = (f a1, f a2, f a3)

-- | Reads puzzle file into list of strings (list of lines)
--  Each character in line is represented by either number or '.'
readPuzzle :: String -> IO [String]
readPuzzle filename = do
  contents <- readFile filename
  let puzzle = read contents :: [String]
  return puzzle

-- | Parses list of strings into Board
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

-- | Checks if position is in board bounds
positionValid :: Board -> Position -> Bool
positionValid board (y, x)
  | x < 0 || y < 0 || x >= width || y >= height = False
  | otherwise = True
  where
    height = length board
    width = length (head board)

-- | Gets cell at given position. If position is out of bounds
--  Expanded cell will be returned instead
getCell :: Board -> Position -> Cell
getCell board (y, x)
  | not (positionValid board (y, x)) = (Nothing, Expanded)
  | otherwise = (board !! y) !! x

-- | Updates board with new cell state at given position
setBoardCell :: Board -> Position -> CellState -> Board
setBoardCell board (y, x) state
  | positionValid board (y, x) = take y board ++ setInRow (board !! y) : drop (y + 1) board
  | otherwise = board
  where
    (value, _) = board !! y !! x

    setInRow :: Row -> Row
    setInRow row = take x row ++ ((value, state) : drop (x + 1) row)

setNeighborhoodCellState :: Neighborhood -> CellState -> Position -> Neighborhood
setNeighborhoodCellState (up, center, down) state pos
  | pos == (-1, -1) = ((updateCellState nw, n, ne), center, down)
  | pos == (-1, 0) = ((nw, updateCellState n, ne), center, down)
  | pos == (-1, 1) = ((nw, n, updateCellState ne), center, down)
  | pos == (0, -1) = (up, (updateCellState w, c, e), down)
  | pos == (0, 0) = (up, (w, updateCellState c, e), down)
  | pos == (0, 1) = (up, (w, c, updateCellState e), down)
  | pos == (1, -1) = (up, center, (updateCellState sw, s, se))
  | pos == (1, 0) = (up, center, (sw, updateCellState s, se))
  | pos == (1, 1) = (up, center, (sw, s, updateCellState se))
  | otherwise = ((nw, n, ne), (w, c, e), (sw, s, se))
  where
    ((nw, n, ne), (w, c, e), (sw, s, se)) = (up, center, down)
    updateCellState :: Cell -> Cell
    updateCellState cell = (getCellValue cell, state)

fillNeighborhoodCellStates :: Neighborhood -> CellState -> [Position] -> Neighborhood
fillNeighborhoodCellStates nbh state = foldl reducer nbh
  where
    reducer nbh = setNeighborhoodCellState nbh state

-- | Updates board with new cell state at given position
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

getMiddleCell :: Neighborhood -> Cell
getMiddleCell (_, (_, c, _), _) = c

-- | Returns if neighborhood is valid
neighborhoodValid :: Neighborhood -> Bool
neighborhoodValid nbh =
  middleValue >= filledCellsCount && middleValue <= 9 - emptyCellsCount - expandedCellsCount
  where
    middleValue = fromJust (getCellValue (getMiddleCell nbh))
    filledCellsCount = countCellsInNeighborhood Filled nbh
    emptyCellsCount = countCellsInNeighborhood Empty nbh
    expandedCellsCount = countCellsInNeighborhood Expanded nbh

-- | Solves single neighborhood
processNeighborhood :: Neighborhood -> Neighborhood
processNeighborhood nbh
  | cValue == numberOfFilled + numberOfUntouched = fillUntouchedNeighborhoodWithState nbh Filled
  | cValue == numberOfFilled = fillUntouchedNeighborhoodWithState nbh Empty
  | otherwise = applyKnownSolutions nbh
  where
    ((nw, n, ne), (w, c, e), (sw, s, se)) = nbh
    cValue = fromJust (getCellValue c)
    numberOfFilled = countCellsInNeighborhood Filled nbh
    numberOfUntouched = countCellsInNeighborhood Untouched nbh

    applyKnownSolutions :: Neighborhood -> Neighborhood
    applyKnownSolutions nbh = foldr ($) nbh solutions
      where
        solutions = [diff3east, diff3west, diff3north, diff3south]

        diff3west nbh = diff3x nbh (getCellValue w) [(-1, 1), (0, 1), (1, 1)]
          where
            (_, (w, _, _), _) = nbh
        diff3east nbh = diff3x nbh (getCellValue e) [(-1, -1), (0, -1), (1, -1)]
          where
            (_, (_, _, e), _) = nbh
        diff3north nbh = diff3x nbh (getCellValue n) [(1, -1), (1, 0), (1, 1)]
          where
            ((_, n, _), _, _) = nbh
        diff3south nbh = diff3x nbh (getCellValue s) [(-1, -1), (-1, 0), (-1, 1)]
          where
            (_, _, (_, s, _)) = nbh

        diff3x :: Neighborhood -> CellValue -> [Position] -> Neighborhood
        diff3x nbh cellValue positions
          | isJust cellValue && cValue - fromJust cellValue >= 3 =
            fillNeighborhoodCellStates nbh Filled positions
          | otherwise = nbh

unwrapNeighborhoodFlat :: Neighborhood -> [Cell]
unwrapNeighborhoodFlat ((nw, n, ne), (w, c, e), (sw, s, se)) = [nw, n, ne, w, c, e, sw, s, se]

-- | Returns a count of cells with given state in neighborhood
countCellsInNeighborhood :: CellState -> Neighborhood -> Int
countCellsInNeighborhood state nbh = countCellsInList state listOfCells
  where
    listOfCells = unwrapNeighborhoodFlat nbh

    countCellsInList :: CellState -> [Cell] -> Int
    countCellsInList _ [] = 0
    countCellsInList cellState ((_, state) : rest)
      | state == cellState = 1 + countCellsInList cellState rest
      | otherwise = countCellsInList cellState rest

-- | Returns a neighborhood with changed state of all untouched cells
fillUntouchedNeighborhoodWithState :: Neighborhood -> CellState -> Neighborhood
fillUntouchedNeighborhoodWithState nbh newState =
  mapTriple (mapTriple (updateCellIfUntouched newState)) nbh

-- | Updates Cell state if it's Untouched
updateCellIfUntouched :: CellState -> Cell -> Cell
updateCellIfUntouched newState (value, state)
  | state == Untouched = (value, newState)
  | otherwise = (value, state)

-- | Updates board with neighborhood on given position
updateBoard :: Board -> Position -> Neighborhood -> Board
updateBoard board (y, x) ((nw, n, ne), (w, c, e), (sw, s, se)) =
  foldr setCellWrapped board cells
  where
    cells =
      [ ((y -1, x -1), getCellState nw),
        ((y -1, x), getCellState n),
        ((y -1, x + 1), getCellState ne),
        ((y, x -1), getCellState w),
        ((y, x), getCellState c),
        ((y, x + 1), getCellState e),
        ((y + 1, x -1), getCellState sw),
        ((y + 1, x), getCellState s),
        ((y + 1, x + 1), getCellState se)
      ]

    setCellWrapped :: (Position, CellState) -> Board -> Board
    setCellWrapped (pos, state) board = setBoardCell board pos state

processCellAtPosition :: Position -> Board -> Board
processCellAtPosition pos board = resultBoard
  where
    neighborhood = neighborhoodOfCell board pos
    resultBoard = updateBoard board pos (processNeighborhood neighborhood)

-- | Finds cells with numbers on board
findNumberedCells :: Board -> [Position]
findNumberedCells board = find board 0
  where
    find :: Board -> Int -> [Position]
    find [] _ = []
    find (row : rest) y = findInRow row 0 y ++ find rest (y + 1)

    findInRow :: Row -> Int -> Int -> [Position]
    findInRow [] _ _ = []
    findInRow ((middleValue, _) : rest) x y
      | x < 0 = []
      | isJust middleValue = (y, x) : findInRow rest (x + 1) y
      | otherwise = findInRow rest (x + 1) y

-- | Counts cells of given state on board
countCells :: CellState -> Board -> Int
countCells state board = sum countedRows
  where
    countInRow :: Row -> Int
    countInRow [] = 0
    countInRow ((_, cellState) : xs)
      | cellState == state = 1 + countInRow xs
      | otherwise = countInRow xs

    countedRows = map countInRow board

-- | Counts cells of given state on board
countFilledCells :: Board -> Int
countFilledCells = countCells Filled

-- | Counts untouched cells on board
countUntouchedCells :: Board -> Int
countUntouchedCells = countCells Untouched

-- | Checks if two boards are equal
boardsEqual :: Board -> Board -> Bool
boardsEqual bA bB = cntA == cntB
  where
    cntA = countFilledCells bA
    cntB = countFilledCells bB

-- | Checks board is valid
boardValid :: Board -> Bool
boardValid board = foldl reducer True numberedCells
  where
    numberedCells = findNumberedCells board

    reducer :: Bool -> Position -> Bool
    reducer False pos = False
    reducer True pos = cellValid board pos
    cellValid :: Board -> Position -> Bool
    cellValid board pos = neighborhoodValid n
      where
        n = neighborhoodOfCell board pos

-- | Checks if board is complete (board is solved)
boardCompleted :: Board -> Bool
boardCompleted board = countUntouchedCells board == 0

-- | Prints board in pretty way
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

-- | returns position of any unchecked position if exists
findAnyUncheckedCell :: Board -> Maybe Position
findAnyUncheckedCell board = find board 0
  where
    find :: Board -> Int -> Maybe Position
    find [] _ = Nothing
    find (row : rest) y
      | isJust (getIndexInRow row 0) = Just (y, fromJust (getIndexInRow row 0))
      | otherwise = find rest (y + 1)

    getIndexInRow :: [Cell] -> Int -> Maybe Int
    getIndexInRow [] _ = Nothing
    getIndexInRow (cell : rest) x
      | getCellState cell == Untouched = Just x
      | otherwise = getIndexInRow rest (x + 1)

-- | Solves board
solve :: Maybe Board -> Maybe Board
solve board
  | isNothing board = Nothing
  | not (boardValid newBoard) = Nothing
  | boardCompleted newBoard = Just newBoard
  | newBoard == fromJust board = tryUncheckedCell newBoard
  | otherwise = solve (Just newBoard)
  where
    step :: Board -> Board
    step board = foldr processCellAtPosition board niceCells
      where
        niceCells = findNumberedCells board

    newBoard = step (fromJust board)

    tryUncheckedCell :: Board -> Maybe Board
    tryUncheckedCell board
      | isJust newBoardWithFilledCell = newBoardWithFilledCell
      | otherwise = newBoardWithEmptyCell
      where
        uncheckedCell = fromJust (findAnyUncheckedCell board)
        boardWithFilledCell = setBoardCell board uncheckedCell Filled
        boardWithEmptyCell = setBoardCell board uncheckedCell Empty

        newBoardWithFilledCell = solve (Just boardWithFilledCell)
        newBoardWithEmptyCell = solve (Just boardWithEmptyCell)

main = do
  putStrLn "Path to file with puzzle:"
  path <- getLine
  puzzle <- readPuzzle path
  let board = createBoard puzzle
      solved = solve (Just board)
  printBoard (fromJust solved)