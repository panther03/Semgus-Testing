from z3 import *

queens = [(Int(f"qx{i}"),Int(f"qy{i}")) for i in range(8)]

s = Solver()

nums = []
for i in range(8):
    nums.append(And(1 <= queens[i][0], queens[i][0] <= 8))
    nums.append(And(1 <= queens[i][1], queens[i][1] <= 8))

print(list(map(lambda x: x[0], queens)))
rows = ([Distinct(list(map(lambda x: x[0], queens)))]) # list of X coords for queens
cols = ([Distinct(list(map(lambda x: x[1], queens)))])
diags = ([Distinct(list(map(lambda x: x[1] - x[0], queens)))])

s.add(rows)
s.add(cols)
s.add(diags)
s.add(nums)
s.check()
m = s.model()


board = [("|-" * 8 + '|')] * 8
for i in range(8):
    xpos = 2*(m[queens[i][0]]).as_long()-1
    ypos = (m[queens[i][1]]).as_long()-1
    board[ypos] = board[ypos][:xpos] + 'Q' + board[ypos][xpos+1:]

for row in board:
    print(row)
