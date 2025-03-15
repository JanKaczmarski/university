use std::collections::HashSet;

fn main() {
    let board: [[u8; 9]; 9] = [
        [5, 3, 0, 0, 7, 0, 0, 0, 0],
        [6, 0, 0, 1, 9, 5, 0, 0, 0],
        [0, 9, 8, 0, 0, 0, 0, 6, 0],
        [8, 0, 0, 0, 6, 0, 0, 0, 3],
        [4, 0, 0, 8, 0, 3, 0, 0, 1],
        [7, 0, 0, 0, 2, 0, 0, 0, 6],
        [0, 6, 0, 0, 0, 0, 2, 8, 0],
        [0, 0, 0, 4, 1, 9, 0, 0, 5],
        [0, 0, 0, 0, 8, 0, 0, 7, 9],
    ];

    println!("{}", is_valid_sudoku(&board))
}

fn is_valid_sudoku(board: &[[u8; 9]; 9]) -> bool {
    // Check for valid digit range (0-9)
    if board.iter().flatten().any(|&x| x > 9) {
        return false;
    }

    // check rows
    for row in board.iter() {
        if !is_unique(row) {
            return false;
        }
    }

    // check cols
    for col in 0..9 {
        let mut column = [0; 9];
        for row in 0..9 {
            column[row] = board[row][col];
        }
        if !is_unique(&column) {
            return false;
        }
    }

    // check subgrids
    for subgrid_row in (0..9).step_by(3) {
        for subgrid_col in (0..9).step_by(3) {
            let mut subgrid = [0; 9];
            let mut index = 0;
            for row in subgrid_row..subgrid_row + 3 {
                for col in subgrid_col..subgrid_col + 3 {
                    subgrid[index] = board[row][col];
                    index += 1;
                }
            }
            if !is_unique(&subgrid) {
                return false;
            }
        }
    }

    true
}

// Check if row/col/subgrid is unique
fn is_unique(arr: &[u8; 9]) -> bool {
    let mut seen = HashSet::new();
    for &num in arr.iter() {
        if num != 0 && !seen.insert(num) {
            return false;
        }
    }
    true
}

// Moduł testowy
#[cfg(test)]
mod tests {
    use super::*;

    // Valid sudoku
    #[test]
    fn test_valid_sudoku() {
        let board: [[u8; 9]; 9] = [
            [5, 3, 0, 0, 7, 0, 0, 0, 0],
            [6, 0, 0, 1, 9, 5, 0, 0, 0],
            [0, 9, 8, 0, 0, 0, 0, 6, 0],
            [8, 0, 0, 0, 6, 0, 0, 0, 3],
            [4, 0, 0, 8, 0, 3, 0, 0, 1],
            [7, 0, 0, 0, 2, 0, 0, 0, 6],
            [0, 6, 0, 0, 0, 0, 2, 8, 0],
            [0, 0, 0, 4, 1, 9, 0, 0, 5],
            [0, 0, 0, 0, 8, 0, 0, 7, 9],
        ];

        assert!(is_valid_sudoku(&board))
    }

    // Number not in range 0..9 present
    #[test]
    fn test_number_range_invalid() {
        let board: [[u8; 9]; 9] = [
            [5, 3, 0, 0, 7, 0, 0, 0, 0],
            [6, 0, 0, 1, 9, 5, 0, 0, 11],
            [0, 9, 8, 0, 0, 0, 0, 6, 0],
            [8, 0, 0, 0, 6, 0, 0, 0, 3],
            [4, 0, 0, 8, 0, 3, 0, 0, 1],
            [7, 0, 0, 0, 2, 0, 0, 0, 6],
            [0, 6, 0, 0, 0, 0, 2, 8, 0],
            [0, 0, 0, 4, 1, 9, 0, 0, 5],
            [0, 0, 0, 0, 8, 0, 0, 7, 9],
        ];

        assert!(!is_valid_sudoku(&board))
    }

    // row isn't unique
    #[test]
    fn test_row_invalid() {
        let board: [[u8; 9]; 9] = [
            [5, 3, 0, 5, 7, 0, 0, 0, 0],
            [6, 0, 0, 1, 9, 5, 0, 0, 0],
            [0, 9, 8, 0, 0, 0, 0, 6, 0],
            [8, 0, 0, 0, 6, 0, 0, 0, 3],
            [4, 0, 0, 8, 0, 3, 0, 0, 1],
            [7, 0, 0, 0, 2, 0, 0, 0, 6],
            [0, 6, 0, 0, 0, 0, 2, 8, 0],
            [0, 0, 0, 4, 1, 9, 0, 0, 5],
            [0, 0, 0, 0, 8, 0, 0, 7, 9],
        ];

        assert!(!is_valid_sudoku(&board))
    }

    // col isn't unique
    #[test]
    fn test_col_invalid() {
        let board: [[u8; 9]; 9] = [
            [5, 3, 0, 5, 7, 0, 0, 0, 0],
            [6, 0, 0, 1, 9, 5, 0, 0, 0],
            [5, 9, 8, 0, 0, 0, 0, 6, 0],
            [8, 0, 0, 0, 6, 0, 0, 0, 3],
            [4, 0, 0, 8, 0, 3, 0, 0, 1],
            [7, 0, 0, 0, 2, 0, 0, 0, 6],
            [0, 6, 0, 0, 0, 0, 2, 8, 0],
            [0, 0, 0, 4, 1, 9, 0, 0, 5],
            [0, 0, 0, 0, 8, 0, 0, 7, 9],
        ];

        assert!(!is_valid_sudoku(&board))
    }

    // subgrid isn't unique
    #[test]
    fn test_subgrid_invalid() {
        let board: [[u8; 9]; 9] = [
            [5, 3, 0, 5, 7, 0, 0, 0, 0],
            [6, 0, 0, 1, 9, 5, 0, 0, 0],
            [0, 9, 8, 0, 0, 0, 0, 6, 0],
            [8, 0, 0, 0, 6, 0, 0, 0, 3],
            [4, 0, 0, 8, 0, 3, 0, 0, 1],
            [7, 0, 0, 0, 2, 0, 0, 0, 6],
            [0, 6, 0, 0, 0, 0, 2, 8, 0],
            [0, 0, 0, 4, 1, 9, 0, 0, 5],
            [0, 0, 6, 0, 8, 0, 0, 7, 9],
        ];

        assert!(!is_valid_sudoku(&board))
    }
}
