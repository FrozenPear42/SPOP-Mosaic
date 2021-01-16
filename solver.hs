module Solver where

-- Coordinate system
-- 0--------- x
-- |
-- |
-- |
-- |
-- y
data CellState = Empty | Filled | Untouched | Expanded deriving (Eq, Show)

type CellValue = Maybe Int

type Cell = (CellValue, CellState)

type Row = [Cell]

type Board = [Row]

-- (y, x)
type Position = (Int, Int)

type Triple t = (t, t, t)

type Neighborhood = Triple (Triple Cell)

blankCell :: Cell
blankCell = (Nothing, Untouched)

readPuzzle :: String -> IO [String]
readPuzzle filename = do
  contents <- readFile filename
  let puzzle = read contents :: [String]
  return puzzle

createBoard :: [String] -> Board
createBoard rows = map parseRow rows
  where
    parseRow :: String -> Row
    parseRow [] = []
    parseRow (c : cs) = parseField c : parseRow cs

    parseField :: Char -> Cell
    parseField '.' = blankCell
    parseField c = (digitToInt c, Untouched)

    digitToInt :: Char -> Maybe Int
    digitToInt c = Just (fromEnum c - 48)

emptyRow :: Int -> Row
emptyRow 1 = [blankCell]
emptyRow n = blankCell : emptyRow (n - 1)

getCell :: Board -> Position -> Cell
getCell board (y, x)
  | x < 0 || y < 0 || x >= width || y >= height = (Nothing, Expanded)
  | otherwise = (board !! y) !! x
  where
    height = length board
    width = length (head board)

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

processNeighborhood :: Neighborhood -> Neighborhood
processNeighborhood nbh = nbh

-- TODO: do all heuristic checks

updateBoard :: Board -> Position -> Neighborhood -> Board
updateBoard board (y, x) nbh = board

-- TODO: write cells to board

processCellAtPosition :: Position -> Board -> Board
processCellAtPosition pos board = resultBoard
  where
    neighborhood = neighborhoodOfCell board pos
    resultBoard = updateBoard board pos (processNeighborhood neighborhood)

findNumberedCells :: Board -> [Position]
findNumberedCells board = find board (length board - 1)
  where
    find :: Board -> Int -> [Position]
    find [] _ = []
    find _ 0 = []
    find (row : rest) y = findInRow row (length row) y ++ find rest (y - 1)

    findInRow :: Row -> Int -> Int -> [Position]
    findInRow ((cellValue, _) : rest) x y
      | x < 0 = []
      | cellValue /= Nothing = (y, x) : findInRow rest (x - 1) y
      | otherwise = findInRow rest (x - 1) y


countCells :: CellState -> Board -> Int
countCells state board = sum countedRows
  where
    countInRow :: Row -> Int
    countInRow ((_, state):xs)
      | state == state = 1 + countInRow xs
      | otherwise = countInRow xs

    countedRows = map countInRow board

countFilledCells :: Board -> Int
countFilledCells = countCells Filled

countUntouchedCells :: Board -> Int
countUntouchedCells = countCells Untouched


boardsEqual :: Board -> Board -> Bool 
boardsEqual bA bB = cntA == cntB 
  where 
    cntA = countFilledCells bA
    cntB = countFilledCells bB

boardValid :: Board -> Bool


solve :: Board -> (Board, Bool)
solve board 
  | boardValid newBoard && countUntouchedCells newBoard == 0 = newBoard
  -- | boardValid newBoard && boardsEqual board newBoard = solve -- TODO: Backtrack  
  | boardValid newBoard not True = (newBoard, False)  
  where
    step :: Board -> Board
    step board = foldr processCellAtPosition board niceCells
      where
        niceCells = findNumberedCells board

    newBoard = step board

main = do
  puzzle <- readPuzzle "puzzles/heart.txt"
  let board = createBoard puzzle
      solved = solve board
  print solved