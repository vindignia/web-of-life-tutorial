'''adapted from # Strona, G., et al., Nature Commun. vol. 5, 4114 (2014). DOI 10.1038/ncomms5114 '''
from random import sample, randint, shuffle
import numpy as np

def find_presences(input_matrix):
	num_rows, num_cols = input_matrix.shape
	hp = []
	iters = num_rows if num_cols >= num_rows else num_cols
	input_matrix_b = input_matrix if num_cols >= num_rows else np.transpose(input_matrix)
	for r in range(iters):
		hp.append(list(np.where(input_matrix_b[r] == 1)[0]))
	return hp

def curve_ball(input_matrix, r_hp, num_iterations=-1):
	num_rows, num_cols = input_matrix.shape
	l = range(len(r_hp))
	num_iters = 5*min(num_rows, num_cols) if num_iterations == -1 else num_iterations
	for rep in range(num_iters):
		AB = sample(l, 2)
		a = AB[0]
		b = AB[1]
		ab = set(r_hp[a])&set(r_hp[b]) # common elements
		l_ab=len(ab)
		l_a=len(r_hp[a])
		l_b=len(r_hp[b])
		if l_ab not in [l_a,l_b]:		
			tot=list(set(r_hp[a]+r_hp[b])-ab)
			ab=list(ab)	
			shuffle(tot)
			L=l_a-l_ab
			r_hp[a] = ab+tot[:L]
			r_hp[b] = ab+tot[L:]
	out_mat = np.zeros(input_matrix.shape, dtype='int8') if num_cols >= num_rows else np.zeros(input_matrix.T.shape, dtype='int8')
	for r in range(min(num_rows, num_cols)):
		out_mat[r, r_hp[r]] = 1
	result = out_mat if num_cols >= num_rows else out_mat.T
	return result

def main():
	print("in main()")
	print()

	# load the original matrix from the CSV file
	mat = np.loadtxt('../matrices/matrix.csv', delimiter=',', skiprows=1)

	r_hp = find_presences(mat)
	itermax = 100
	mat_cb = curve_ball(mat, r_hp, itermax)

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
	import sys
	sys.exit(main())