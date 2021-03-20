from z3 import *

# playing around with lists

A = Array('A', IntSort(), IntSort())

x = Int('x')
l = [5, 3, 2, 1, 6, 4, 7, 10, 9, 8]
s = Solver()

# For every element E in l there exists an index i in 0 < i < length such that Select(A, i) == E

for i in range(len(l)):
    s.add(Or([Select(A, i) == E for E in l]))
    
s.add(Distinct([Select(A, i) for i in range(len(l))]))

# try to indicate that the list needs to contain elements only from l, exactly once

#forall y in A: Implies(x != y, Implies(Before(x,y), x <= y))
for i in range(len(l)):
    for j in range(len(l)):
        Implies( i <= j, Select(A,i) < Select(A,j))

print(s.check())
x = s.model()
for i in range(len(l)):
    print(Select(x[A], i))