use std::str::FromStr;

pub fn run(input: &str) {
  todo!()
}

// Ideas:
// DONE 02. parsing the string to create the matrix
// DONE 03. parse the input list of numbers (drawn)
// DONE 04. implement - mark the number on the board
// DONE 05. check whether a specific row or column is fully marked
// DONE 06. add a function that determines if a board is winning or not
//       a. possible impl could check each row & col for a win
//       b. could be made more efficient
// DONE 07. implement scoring function
// DONE  a. sum of unmarked
//      08. split input into draws and board definitions,
//      09. split board definition into a vector of boards
// DONE 10. keep track of marked cells in a matrix of bool
//      11. to represent numbers with the times-table

struct Board {
  cells: Vec<Vec<u8>>,
  marked: Vec<Vec<bool>>,
  last_marked_number: Option<u8>,
}

impl Board {
  fn contains(&self, n: u8) -> bool {
    for row in &self.cells {
      if row.contains(&n) {
        return true;
      }
    }
    false
  }

  fn locate(&self, n: u8) -> Option<(usize, usize)> {
    for (row_id, row) in self.cells.iter().enumerate() {
      for (col_id, &cell) in row.iter().enumerate() {
        if cell == n {
          return Some((row_id, col_id));
        }
      }
    }
    None
  }

  fn mark(&mut self, n: u8) {
    if let Some((row, col)) = self.locate(n) {
      self.marked[row][col] = true;
      self.last_marked_number = Some(n);
    }
  }

  fn is_marked(&self, row: usize, col: usize) -> bool {
    self.marked[row][col]
  }

  fn row_is_marked(&self, row: usize) -> bool {
    self.marked[row].iter().all(|&cell| cell)
  }

  fn col_is_marked(&self, col: usize) -> bool {
    self.marked.iter().all(|row| row[col])
  }

  fn has_won(&self) -> bool {
    (0..5).any(|i| self.col_is_marked(i) || self.row_is_marked(i))
  }

  fn sum_of_unmarked_numbers(&self) -> u32 {
    let mut acc = 0;
    for i in 0..5 {
      for j in 0..5 {
        if !self.is_marked(i, j) {
          acc += self.cells[i][j] as u32;
        }
      }
    }
    acc
  }

  fn last_marked_number(&self) -> Option<u8> {
    self.last_marked_number
  }

  fn score(&self) -> Option<u32> {
    self
      .last_marked_number()
      .map(|number| number as u32 * self.sum_of_unmarked_numbers())
  }
}

impl FromStr for Board {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let cells: Vec<Vec<u8>> = s
      .lines()
      .map(|line| {
        line
          .split_ascii_whitespace()
          .map(|number| number.parse::<u8>().unwrap())
          .collect()
      })
      .collect();
    let marked = vec![vec![false; cells[0].len()]; cells.len()];
    Ok(Self { cells, marked, last_marked_number: None })
  }
}

fn parse_draws(s: &str) -> impl Iterator<Item = u8> + '_ {
  s.split(',').map(|n| n.parse::<u8>().unwrap())
}

#[cfg(test)]
mod tests {
  use super::*;
  use indoc::indoc;

  #[test]
  fn given_input_create_board() {
    let board = Board::from_str(&create_board_data()).unwrap();
    assert!(board.contains(7));
    assert!(!board.contains(77));
    assert!(board.contains(9));
  }

  #[test]
  fn locates_number_on_board() {
    let board = Board::from_str(&create_board_data()).unwrap();
    assert_eq!(board.locate(7), Some((2, 4)));
    assert_eq!(board.locate(0), Some((0, 4)));
    assert_eq!(board.locate(8), Some((1, 0)));
    assert_eq!(board.locate(99), None);
  }

  #[test]
  fn board_marks_positions() {
    let mut board = Board::from_str(&create_board_data()).unwrap();
    board.mark(7);
    assert!(board.is_marked(2, 4));
    assert!(!board.is_marked(1, 4));
  }

  #[test]
  fn board_row_is_marked() {
    let mut board = Board::from_str(&create_board_data()).unwrap();
    board.mark(8);
    board.mark(2);
    board.mark(23);
    board.mark(4);
    board.mark(24);
    assert!(board.row_is_marked(1));
    assert!(!board.row_is_marked(0));
  }

  #[test]
  fn board_column_is_marked() {
    let mut board = Board::from_str(&create_board_data()).unwrap();
    board.mark(13);
    board.mark(2);
    board.mark(9);
    board.mark(10);
    board.mark(12);
    assert!(board.col_is_marked(1));
    assert!(!board.col_is_marked(0));
  }

  #[test]
  fn board_is_won_for_all_items_in_column_marked_1() {
    let mut board = Board::from_str(&create_board_data()).unwrap();
    board.mark(13);
    assert!(!board.has_won());
    board.mark(2);
    board.mark(9);
    board.mark(10);
    board.mark(12);
    assert!(board.has_won());
  }

  #[test]
  fn board_is_won_for_all_items_in_column_marked_2() {
    let mut board = Board::from_str(&create_board_data()).unwrap();
    board.mark(22);
    assert!(!board.has_won());
    board.mark(8);
    board.mark(21);
    board.mark(6);
    board.mark(1);
    assert!(board.has_won());
  }

  #[test]
  fn board_is_won_for_all_items_in_row_marked_1() {
    let mut board = Board::from_str(&create_board_data()).unwrap();
    board.mark(22);
    assert!(!board.has_won());
    board.mark(13);
    board.mark(17);
    board.mark(11);
    board.mark(0);
    assert!(board.has_won());
  }

  #[test]
  fn board_sum_of_unmarked_numbers_1() {
    let mut board = Board::from_str(&create_board_data()).unwrap();
    board.mark(22);
    board.mark(13);
    board.mark(17);
    board.mark(11);
    board.mark(8);
    board.mark(23);
    board.mark(24);
    board.mark(21);
    board.mark(9);
    board.mark(14);
    board.mark(16);
    board.mark(18);
    board.mark(20);
    board.mark(91);
    assert_eq!(board.sum_of_unmarked_numbers(), 2 + 4 + 7 + 6 + 10 + 3 + 5 + 1 + 12 + 15);
    board.mark(15);
    assert_eq!(board.sum_of_unmarked_numbers(), 2 + 4 + 7 + 6 + 10 + 3 + 5 + 1 + 12);
  }

  #[test]
  fn last_marked_number() {
    let mut board = Board::from_str(&create_board_data()).unwrap();
    assert_eq!(board.last_marked_number(), None);
    board.mark(22);
    board.mark(13);
    board.mark(17);
    board.mark(11);
    assert_eq!(board.last_marked_number(), Some(11));
  }

  #[test]
  fn board_can_score_1() {
    let mut board = Board::from_str(&create_alt_board_data()).unwrap();
    assert_eq!(board.score(), None);
    board.mark(14);
    board.mark(21);
    board.mark(17);
    board.mark(7);
    board.mark(4);
    board.mark(9);
    board.mark(23);
    board.mark(11);
    board.mark(5);
    board.mark(2);
    board.mark(0);
    assert_eq!(board.score(), Some(0));
    board.mark(24);
    assert_eq!(board.score(), Some(4512));
  }

  #[test]
  fn can_parse_draws() {
    let draw_data = create_draws_data();
    let mut draws = parse_draws(&draw_data);
    assert_eq!(draws.next(), Some(7));
  }

  fn create_draws_data() -> String {
    String::from(indoc! {"
      7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
    "})
  }

  fn create_alt_board_data() -> String {
    String::from(indoc! {"
      14 21 17 24  4
      10 16 15  9 19
      18  8 23 26 20
      22 11 13  6  5
       2  0 12  3  7
    "})
  }

  // TODO: Change this to a static string
  fn create_board_data() -> String {
    String::from(indoc! {"
      22 13 17 11  0
       8  2 23  4 24
      21  9 14 16  7
       6 10  3 18  5
       1 12 20 15 91
    "})
  }
}
