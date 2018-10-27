/*
 *	This file is part of Fred's Maze Solver.
 *
 *	Fred's Maze Solver is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	Fred's Maze Solver is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with Fred's Maze Solver.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ROWS 41
#define COLS 81

int maze_solve_helper(char maze[ROWS][COLS], int r, int c, int pr, int pc);
int maze_solve_helper_2(char maze[ROWS][COLS]);
int maze_solve(char maze[ROWS][COLS]);
void maze_print(char maze[ROWS][COLS]);

int main (int argc, char *argv[]) {
	FILE *input = NULL;
	char maze[ROWS][COLS];
	char *line = NULL;
	size_t line_len = 0;
	ssize_t read_chars = 0;
	int line_num = 0,
		chars_check = 0,
		r = 0,
		c = 0;
	
	if (argc != 2) {	/* 1 for program name and 1 for parameter */
		fprintf(stderr, "Invalid number of arguments.\n");
		return 1;
	}

	input = fopen(argv[1], "r");
	if (!input) {		/* could not open the file for reading */
		fprintf(stderr, "Could not open maze file for reading.\n");
		return 2;
	}

	/* read file into maze 2d-array */
	while ((read_chars = getline(&line, &line_len, input)) != -1) {
		if (read_chars != COLS + 1) {
			fprintf(stderr, "Invalid maze file, invalid data.\n");
			goto missing_data;
		}

		/* check for some invalid data */
		chars_check = 0;
		while (chars_check < read_chars - 1) {
			if (line[chars_check] != '#' && line[chars_check] != ' ') {
				fprintf(stderr, "Invalid character found, invalid data.\n");
				goto invalid_data_found;
			}

			chars_check++;
		}

		strncpy(maze[line_num], line, COLS);
		line_num++;
		/* no need to free line here because getline will use it again */
	}

	free(line);
	fclose(input);

	/* check for some invalid data */
	if (line_num != ROWS) {
		fprintf(stderr, "Maze too small or too large, invalid data.\n");
		return 3;
	}

	/* check entrance and exit */
	if (maze[0][1] != ' ' || maze[40][79] != ' ') {
		fprintf(stderr, "No entrance or exit point.\n");
		return 4;
	}

	/* check if the borders have openings other than the entrance and exit */
	r = 0;
	c = 2;
	while (c < COLS) {
		if (maze[r][c] != '#') {
			fprintf(stderr, "There is an opening in the outside wall.\n");
			return 3;
		}
		c++;
	}

	r = ROWS - 1;
	c = 0;
	while (c < COLS - 2) {
		if (maze[r][c] != '#') {
			fprintf(stderr, "There is an opening in the outside wall.\n");
			return 3;
		}
		c++;
	}

	r = 0;
	c = COLS - 1;
	while (r < ROWS) {
		if (maze[r][c] != '#') {
			fprintf(stderr, "There is an opening in the outside wall.\n");
			return 3;
		}
		r++;
	}

	r = 0;
	c = 0;
	while (r < ROWS) {
		if (maze[r][c] != '#') {
			fprintf(stderr, "There is an opening in the outside wall.\n");
			return 3;
		}
		r++;
	}

	/* run solver */
	if (!(maze_solve(maze))) {
		fprintf(stderr, "Cannot solve maze.\n");
		return 5;
	}

	maze_print(maze);

	return 0;

invalid_data_found:
	free(line);
	fclose(input);
	return 3;

missing_data:
	free(line);
	fclose(input);
	return 6;
}

int maze_solve(char maze[ROWS][COLS]) {
	return maze_solve_helper_2(maze);
 	// return maze_solve_helper(maze, 0, 1, 0, 0);
}

/* iterative dead-end filler implementation of the maze solver */
int maze_solve_helper_2(char maze[ROWS][COLS]) {
	int r = 0,
		c = 0,
		walls = 0,
		reloop = 1,
		old_r = 0,
		old_c = 0,
		new_r = 0,
		new_c = 0;

	/* fill in the dead ends with '*' */
	while (reloop) {
		reloop = 0;
		r = 0;
		c = 0;
		walls = 0;
		while (r < ROWS) {
			walls = 0;
			c = 0;
	
			while (c < COLS) {
				walls = 0;

				if (maze[r][c] == ' ') {
					if (r != 0)
						if (maze[r - 1][c] == '#' || maze[r - 1][c] == '*')
							walls++;
					if (c != 0)
						if (maze[r][c - 1] == '#' || maze[r][c - 1] == '*')
							walls++;
					if (r != ROWS)
						if (maze[r + 1][c] == '#' || maze[r + 1][c] == '*')
							walls++;
					if (c != COLS)
						if (maze[r][c + 1] == '#' || maze[r][c + 1] == '*')
							walls++;
	
					if (walls >= 3) {
						maze[r][c] = '*';
						reloop = 1;
					}
				}
				c++;
			}
			r++;
		}
	}

	/* replace the empty spaces with '.' and the '*' with ' ' */
	r = 0;
	c = 0;
	while (r < ROWS) {
		c = 0;
		while (c < COLS) {
			if (maze[r][c] == ' ') maze[r][c] = '.';
			if (maze[r][c] == '*') maze[r][c] = ' ';
			c++;
		}
		r++;
	}

	/* check the path from the entrance to exit */
	r = 0;
	c = 1;
	while (1) {
		if (r == 40 && c == 79) goto success;
		
		if (r != 0) {
			new_r = r - 1;
			new_c = c;
			if (!(new_r == old_r && new_c == old_c) && maze[new_r][new_c] == '.') {
				old_r = r;
				old_c = c;
				r = new_r;
				c = new_c;
				continue;
			}
		}

		new_r = r;
		new_c = c - 1;
		if (!(new_r == old_r && new_c == old_c) && maze[new_r][new_c] == '.') {
			old_r = r;
			old_c = c;
			r = new_r;
			c = new_c;
			continue;
		}

		new_r = r + 1;
		new_c = c;
		if (!(new_r == old_r && new_c == old_c) && maze[new_r][new_c] == '.') {
			old_r = r;
			old_c = c;
			r = new_r;
			c = new_c;
			continue;
		}

		new_r = r;
		new_c = c + 1;
		if (!(new_r == old_r && new_c == old_c) && maze[new_r][new_c] == '.') {
			old_r = r;
			old_c = c;
			r = new_r;
			c = new_c;
			continue;
		}
		return 0;
	}

success:
	return 1;
}

/* recursive depth-first search implementation of the maze solver */
int maze_solve_helper(char maze[ROWS][COLS], int r, int c, int pr, int pc) {
	int res = 0,
		nr = 0,
		nc = 0;

	if (r == 40 && c == 79) goto success;

	if (r != 0) {
		nr = r - 1;
		nc = c;
		if (!(nr == pr && nc == pc) && maze[nr][nc] != '#')
			res = maze_solve_helper(maze, nr, nc, r, c);

		if (res) goto success;
	}

	nr = r;
	nc = c - 1;
	if (!(nr == pr && nc == pc) && maze[nr][nc] != '#')
		res = maze_solve_helper(maze, nr, nc, r, c);
	if (res) goto success;
	
	nr = r + 1;
	nc = c;
	if (!(nr == pr && nc == pc) && maze[nr][nc] != '#')
		res = maze_solve_helper(maze, nr, nc, r, c);
	if (res) goto success;

	nr = r;
	nc = c + 1;
	if (!(nr == pr && nc == pc) && maze[nr][nc] != '#')
		res = maze_solve_helper(maze, nr, nc, r, c);
	if (res) goto success;
	
	return 0;

success:
	maze[r][c] = '.';
	return 1;
}

void maze_print(char maze[ROWS][COLS]) {
	int cur_line = 0,
		cur_char = 0;

	while (cur_line < ROWS) {
		cur_char = 0;

		while (cur_char < COLS) {
			putchar(maze[cur_line][cur_char]);
			cur_char++;
		}
		putchar('\n');
		cur_line++;
	}
}

