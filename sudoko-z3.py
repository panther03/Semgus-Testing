from z3 import *

filled = [[5,0,0,0,0,0,0,0,0],
         [6,3,0,0,5,0,7,9,0],
         [0,0,1,8,0,0,0,2,6],
         [0,9,7,5,6,0,8,0,0],
         [8,0,4,0,0,0,6,0,3],
         [0,0,3,0,8,7,9,1,0],
         [7,8,0,0,0,6,1,0,0],
         [0,1,5,0,9,0,0,6,4],
         [0,0,0,0,0,0,0,0,7]]
#filled = [[0,0,0,0,0,0,0,0,0],
#          [0,0,0,0,0,0,0,0,0],
#          [0,0,0,0,0,0,0,0,0],
#          [0,0,0,0,0,0,0,0,0],
#          [0,0,1,0,0,0,0,0,0],
#          [0,0,0,0,0,0,2,0,0],
#          [0,0,0,0,0,0,0,0,0],
#          [0,0,0,0,0,0,0,0,0],
#          [0,0,0,0,0,0,0,0,0]]
#
def square(board, index):
    x = ((index) % 3) * 3
    y = (index // 3) * 3 
    
    square = []
    for i in range(x, x+3):
        for j in range(y, y+3):
            #print(i,j)
            square.append(board[i][j])
    #print(index)
    #print(square)
    return square

s = Solver()

board = [[Int(f"x{i}y{j}") for j in range(9)] for i in range(9)]

for row in board:
    s.add([Distinct(row)])
for col_i in range(9):
    s.add([Distinct(list(map(lambda row: row[col_i], board)))])
for square_i in range(9):
    out_square = square(board, square_i)
    s.add([Distinct(out_square)])
    
for i in range(9):
    for j in range(9):
        s.add(And(board[i][j] >= 1,board[i][j] <= 9))
        if filled[i][j] != 0:
           s.add(filled[i][j] == board[i][j])



print(s.check())
m = s.model()

print_board = [("|-" * 9 + '|')] * 9

for i in range(9):
    for j in range(9):
        xpos = 2 * i + 1
        print_board[j] = print_board[j][:xpos] + str(m[board[i][j]].as_long()) + print_board[j][xpos+1:]

for row in print_board:
    print(row)