import curveball as cb

import sys
import os
import csv
import numpy as np

def main():

    #Ale
    print("in main()")

    # load the original matrix from the CSV file
    mat = np.loadtxt('../matrices/matrix.csv', delimiter=',', skiprows=1)

    #print("original matrix")
    #print(mat.astype(int))
    # print(type(mat))
    # print(type(mat[0][0]))

    print()
    r_hp = cb.find_presences(mat)
    print()

    itermax = 100
    mat_cb = cb.curve_ball(mat, r_hp, itermax)

    #print("randomized matrix")
    #print(mat_cb)


    # Check row totals
    rows_tot_rnd = np.sum(mat_cb, axis=1)
    rows_tot_original = np.sum(mat, axis=1)
    row_totals_equal = np.all(rows_tot_rnd == rows_tot_original)
    print("Row totals are equal:", row_totals_equal)

    # Check column totals
    cols_tot_rnd = np.sum(mat_cb, axis=0)
    cols_tot_original = np.sum(mat, axis=0)
    col_totals_equal = np.all(cols_tot_rnd == cols_tot_original)
    print("Column totals are equal:", col_totals_equal)


if __name__ == '__main__':
    sys.exit(main())
