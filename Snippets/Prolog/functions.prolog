fac(0, 1).
fac(A, B) :- A > 0,
             Ax is A - 1,
             fac(Ax, C),
             B is A * C.

sigma(A, A, A).
sigma(A, B, N) :- B > A,
                  Ax is A + 1,
                  sigma(Ax, B, Nx),
                  N is Nx + A.
