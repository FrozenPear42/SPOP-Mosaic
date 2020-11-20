readPuzzle :: String -> IO [String]
readPuzzle filename = do
  contents <- readFile filename
  let puzzle = read contents :: [String]
  return puzzle

main = do
  puzzle <- readPuzzle "puzzles/heart.txt"
  print puzzle