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

blankCell :: Cell
blankCell = (Nothing, Untouched)

readPuzzle :: String -> IO [String]
readPuzzle filename = do
  contents <- readFile filename
  let puzzle = read contents :: [String]
  return puzzle

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


positionValid :: Board -> Position -> Bool
positionValid board (y, x)
  | x < 0 || y < 0 || x >= width || y >= height = True
  | otherwise = False
  where
    height = length board
    width = length (head board)


getCell :: Board -> Position -> Cell
getCell board (y, x)
  | not (positionValid board (y, x)) = (Nothing, Expanded)
  | otherwise = (board !! y) !! x

setBoardCell :: Board -> Position -> CellState -> Board
setBoardCell board (y, x) state 
  | positionValid board (y, x) =  take y board ++ setInRow (board !! y) : drop (y+1) board
  | otherwise = board
  where  
    (value, _) = board !! y !! x

    setInRow :: Row -> Row
    setInRow row = take x row ++ ((value, state) : drop (x+1) row)



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

getCellState :: Cell -> CellState
getCellState (_, state) = state

updateBoard :: Board -> Position -> Neighborhood -> Board
updateBoard board (y, x) ((nw,n,ne), (w,c,e), (sw,s,se)) = 
  foldr setCellWrapped board cells
  where
    cells = [((y-1, x-1), getCellState nw), ((y-1, x), getCellState n), ((y-1, x+1), getCellState n)]
         ++ [((y, x-1), getCellState w), ((y, x), getCellState c), ((y, x+1), getCellState e)]
         ++ [((y+1, x-1), getCellState sw), ((y+1, x), getCellState s), ((y+1, x+1), getCellState se)]

    setCellWrapped :: (Position, CellState) -> Board -> Board
    setCellWrapped (pos, state) board = setBoardCell board pos state


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

-- probably could use numberedCells as secondInput for optimization
boardValid :: Board -> Bool
boardValid board = foldl reducer True numberedCells
  where
    numberedCells = findNumberedCells board

    reducer :: Bool -> Position -> Bool
    reducer False pos = False 
    reducer True pos = cellValid board pos 

    cellValid :: Board -> Position -> Bool
    cellValid board pos = neighborhoodValid n where
      n = neighborhoodOfCell board pos



boardCompleted :: Board -> Bool
boardCompleted board = countUntouchedCells board == 0

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


solve :: Board -> (Board, Bool)
solve board 
  | boardValid newBoard && boardCompleted newBoard = newBoard
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