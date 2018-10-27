#ifndef MATRIX_H
#define MATRIX_H

#define _1DMAT_INDEX(i,j)  (i * MAT_WIDTH + j)
#define MAT_CELL_1D(m,i,j) (m[_1DMAT_INDEX(i,j)])
#define MAT_CELL_2D(m,i,j) (m[i][j])

#endif /* MATRIX_H */
