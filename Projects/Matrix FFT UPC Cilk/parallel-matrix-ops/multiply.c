#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/wait.h>

/*
#define m1_data_name "m1_data_name"
#define m1_matrix_name "m1_matrix_name"

#define m2_data_name "m2_data_name"
#define m2_matrix_name "m2_matrix_name"

#define res_data_name "res_data_name"
#define res_matrix_name "res_matrix_name"
*/

struct matrix {
	int rows,
			cols;
	int data_id,
			matrix_id;
	key_t data_key,
				matrix_key;
	double *data,
				 **matrix;
};

void parse_input();
void free_all();
void die(const char *);
void allocate_matrix(int, struct matrix *, int, int);
void deallocate_matrix(struct matrix *);
void multiply();
void close_all_files();

int num_rows_per_process = 0,
		start_row_for_process = 0,
		num_processes = 0;

FILE *input_file = NULL,
		 *output_file = NULL;

struct matrix input_mat1,
							input_mat2,
							result_mat;

int main (int argc, char *argv[]) {
	char *input_filename = NULL,
			 *output_filename = NULL;
	int i = 0, j = 0,
			write_res = 0,
			dummy_status = 0;
	pid_t pid = 0;

	/* check for number of arguments */
	if (argc < 4) die("Invalid number of arguments\n");

	/* set variables to arguments */
	input_filename = argv[1];
	output_filename = argv[2];
	num_processes = atoi(argv[3]);

	/* open input file for reading */
	input_file = fopen(input_filename, "r");

	/* check if input file was opened correctly */
	if (input_file == NULL) die("Cannot open input file for reading\n");

	/* open output file for writing */
	output_file = fopen(output_filename, "w+");

	/* check if output file was opened correctly */
	if (output_file == NULL) die("Cannot open output file for writing\n");

	/* check if number of processes argument is valid */
	if (num_processes <= 0) die("Invalid number of processes\n");

	/* parse input file into the two matrixes */
	parse_input();

	/* check if matrix can be properly divided */
	if (input_mat1.rows % num_processes != 0)
		die("First matrix cannot be divided properly\n");

	/* set the number of rows per process */
	num_rows_per_process = input_mat1.rows / num_processes;

	/* allocate result matrix */
	allocate_matrix(3, &result_mat, input_mat1.rows, input_mat2.cols);

	/* initialize result matrix values (make sure they contain 0s) */
	for (i = 0; i < result_mat.rows; i++)
		for (j = 0; j < result_mat.cols; j++)
			result_mat.matrix[i][j] = 0;

	/* fork */
	for (i = 0; i < num_processes; i++) {
		start_row_for_process = i * num_rows_per_process;
		pid = fork();

		switch(pid) {
			case -1:	/* error at parent */
				die("Error forking\n");
				break;
			case 0:		/* child */
				multiply();
				close_all_files();
				return 0;
				break;
			default:	/* parent */
				break;
		}
	}

	/* wait for all child processes */
	for (i = 0; i < num_processes; i++)
		wait(&dummy_status);

	/* write the result matrix to the ouput file */
	for (i = 0; i < result_mat.rows; i++) {
		for (j = 0; j < result_mat.cols; j++) {
			write_res = fprintf(output_file, "%lf ", result_mat.matrix[i][j]);

			if (write_res < 0)
				goto write_err;
		}

		write_res = fprintf(output_file, "\n");

		if (write_res < 0)
			goto write_err;
	}

write_err:
	/* check if a write error happened */
	if (write_res < 0) die("Error writing to output file\n");

	free_all();
	return EXIT_SUCCESS;
}

void multiply() {
	int i = 0, j = 0, k = 0;

	for (i = start_row_for_process;
			 i < start_row_for_process + num_rows_per_process;
			 i++) {
		for (j = 0; j < result_mat.cols; j++) {
			for (k = 0; k < input_mat1.cols; k++) {
				result_mat.matrix[i][j] +=
					(input_mat1.matrix[i][k] * input_mat2.matrix[k][j]);
			}
		}
	}
}

void die(const char *msg) {
	fprintf(stderr, msg);
	free_all();
	exit(EXIT_FAILURE);
}

void close_all_files() {
	if (input_file != NULL) fclose(input_file);
	if (output_file != NULL) fclose(output_file);
}

void free_all() {
	close_all_files();

	deallocate_matrix(&input_mat1);
	deallocate_matrix(&input_mat2);
	deallocate_matrix(&result_mat);
}

void deallocate_matrix(struct matrix *m) {
	//if (m->matrix != NULL) free(m->matrix);
	//if (m->data != NULL) free(m->data);
	if (m->matrix != NULL) {
		shmdt(m->matrix);
		shmctl(m->matrix_id, IPC_RMID, NULL);
	}

	if (m->data != NULL) {
		shmdt(m->data);
		shmctl(m->data_id, IPC_RMID, NULL);
	}
}

void allocate_matrix(int num, struct matrix *m, int rows, int cols) {
	int i = 0;

	/*
	char *data_name = NULL,
			 *matrix_name = NULL;

	switch(num) {
		case 1:	// for input matrix 1
			data_name = m1_data_name;
			matrix_name = m1_matrix_name;
			break;
		case 2: // for input matrix 2
			data_name = m2_data_name;
			matrix_name = m1_matrix_name;
			break;
		case 3: // for result matrix
			data_name = res_data_name;
			matrix_name = res_matrix_name;
			break;
		default:
			break;
	}
	*/

	/* generate the key for the data */
	m->data_key = ftok("multiply", num + 5);

	/* check if key was generated */
	if (m->data_key == -1) die("Cannot generate data key for matrix\n");

	/* generate key for the matrix */
	m->matrix_key = ftok("multiply", num + 10);

	/* check if key was generated */
	if (m->matrix_key == -1)
		die("Cannot generate matrix key for matrix\n");

	/* set matrix size */
	m->rows = rows;
	m->cols = cols;

	/* allocate space for matrix */
	// m->data = malloc(m->rows * m->cols * sizeof(double));
	/* check if allocation failed */
	// if (m->data == NULL)
	//	die("Cannot allocate space for matrix data\n");
	m->data_id = shmget(m->data_key, m->rows * m->cols * sizeof(double),
			0644 | IPC_CREAT);

	/* check if space was allocated */
	if (m->data_id == -1)
		die("Cannot allocate space for matrix data\n");

	/* get the shared memory segment */
	m->data = (double *) shmat(m->data_id, NULL, 0);

	/* allocate space for the matrix rows */
	// m->matrix = malloc(m->rows * sizeof(double *));
	/* check if allocation failed */
	// if (m->matrix == NULL)
	//	die("Cannot allocate space for matrix rows\n");
	m->matrix_id = shmget(m->matrix_key, m->rows * sizeof(double *),
			0644 | IPC_CREAT);

	/* check if space was allocated */
	if (m->matrix_id == -1)
		die("Cannot allocated space for matrix\n");

	/* get the shared memory segment */
	m->matrix = (double **) shmat(m->matrix_id, NULL, 0);

	/* initialize rows array for result matrix */
	for (i = 0; i < m->rows; i++)
		m->matrix[i] = m->data + (i * m->cols);
}

void parse_input() {
	int i = 0, j = 0;

	/* read the first line */
	fscanf(input_file, "%d %d %d %d\n",
			&(input_mat1.rows), &(input_mat1.cols),
			&(input_mat2.rows), &(input_mat2.cols));

	/* check sanity of data */
	if (input_mat1.cols != input_mat2.rows)
		die("Malformed matrix size data\n");

	allocate_matrix(1, &input_mat1, input_mat1.rows, input_mat1.cols);
	allocate_matrix(2, &input_mat2, input_mat2.rows, input_mat2.cols);

	/* read first matrix into array */
	for (i = 0; i < input_mat1.rows; i++) {
		for (j = 0; j < input_mat1.cols; j++) {
			fscanf(input_file, "%lf", &(input_mat1.matrix[i][j]));
		}
	}

	/* read second matrix into array */
	for (i = 0; i < input_mat2.rows; i++) {
		for (j = 0; j < input_mat2.cols; j++) {
			fscanf(input_file, "%lf", &(input_mat2.matrix[i][j]));
		}
	}
}
