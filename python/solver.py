import os
import json
from enum import Enum
from dataclasses import dataclass

class CellState(Enum):
    UNTOUCHED = 0
    EMPTY = 1
    FILLED = 2

@dataclass
class Cell():
     value: int
     state: CellState

def read_puzzle(path):
    with open(path) as file:
        content = file.read()
        content = content.replace("\n", "")
        puzzle = json.loads(content)
        return puzzle


def make_cell(c):
    if c == '.':
        return Cell(None, CellState.UNTOUCHED)
    else:
        return Cell(ord(c) - 0x30, CellState.UNTOUCHED)


def convert_puzzle_to_board(puzzle):
    parsed =  [[ make_cell(c) for c in row] for row in puzzle]
    return list(map(list, zip(*parsed)))

def print_board(board):
    height = len(board)
    width = len(board[0])

    for y in range(height):
        s = ""
        for x in range(width):
            if board[x][y].state == CellState.UNTOUCHED:
                if board[x][y].value == None:
                    s += "."
                else:
                    s += str(board[x][y].value)
            elif board[x][y].state == CellState.EMPTY:
                s += "O"
            elif board[x][y].state == CellState.FILLED:
                s += "X"
        print(s)
                


def fill_board(board, center, value):
    x = center[0]
    y = center[1]
    height = len(board)
    width = len(board[0])

    for sx in [x-1, x, x+1]:
        for sy in [y-1, y, y+1]:
            if sx < 0 or sx >= width or sy < 0 or sy >= height:
                pass
            else:
                if board[sx][sy].state == CellState.UNTOUCHED:
                    board[sx][sy].state = value        

def main():
    puzzle = read_puzzle("../puzzles/duck.txt")
    board = convert_puzzle_to_board(puzzle)
    height = len(board)
    width = len(board[0])

    nice_cells = []
    for y in range(height):
        for x in range(width):
            if board[x][y].value != None:
                nice_cells.append((x,y, board[x][y].value))

    updated = True
    while updated:
        print("\niteration")
        print_board(board)
        updated = False
        new_nice_cells = []
        for c in nice_cells:
            x = c[0]
            y = c[1]
            cell = board[x][y]

            empty = 0
            filled = 0
            neighbor_cells = 9
            for sx in [x-1, x, x+1]:
                for sy in [y-1, y, y+1]:
                    if sx < 0 or sx >= width or sy < 0 or sy >= height:
                        neighbor_cells -= 1    
                    else:
                        if board[sx][sy].state == CellState.EMPTY:
                            empty += 1
                        elif board[sx][sy].state == CellState.FILLED:
                            filled += 1

            # print((x, y, cell.value, cell.state, neighbor_cells, empty, filled))
            if neighbor_cells == cell.value:
                fill_board(board, (x, y), CellState.FILLED)
                print("filled cell (corner case): "+ str((x, y)))
                updated = True
            elif cell.value == 0:
                fill_board(board, (x, y), CellState.EMPTY)
                updated = True
                print("emptied cell (value 0): "+ str((x, y)))
            elif empty == neighbor_cells - cell.value:
                fill_board(board, (x, y), CellState.FILLED)
                updated = True
                print("filled cell (known all empty): " + str((x, y)))
            elif filled == cell.value:
                fill_board(board, (x, y), CellState.EMPTY)
                print("emptied cell (known all filled): " + str((x, y)))
                updated = True
            else:
                new_nice_cells.append(c) 
        nice_cells = new_nice_cells

if __name__ == "__main__":
    main()    

# Optimizations
# next to wall if difference between two neighbor cell values is 2 then shared space determines one row
# if difference between two neighbor cell values is 3 then shared space determines one row
