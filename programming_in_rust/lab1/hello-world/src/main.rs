use std::io;
use std::io::Write;

const X_SYMBOL: char = 'X';
const O_SYMBOL: char = 'O';
const EMPTY_FIELD: char = ' ';

const PLAYER_ASSOCIATION: [char; 2] = [X_SYMBOL, O_SYMBOL];

const BOARD_SIZE: usize = 3;

enum GameState {
    Undecided,
    Invalid,
    Won,
    Lost,
    Draw,
}

fn main() {
    let mut board = [[EMPTY_FIELD; BOARD_SIZE]; BOARD_SIZE];
    let mut flag = GameState::Undecided;
    let mut player = 0;

    println!("Welcome to X/O game!");

    while matches!(flag, GameState::Undecided) || matches!(flag, GameState::Invalid) {
        flag = play_round(player, &mut board);

        match flag {
            GameState::Won => println!(
                "Player {} WON! Congratulations!!!",
                PLAYER_ASSOCIATION[player as usize]
            ),
            GameState::Lost => println!(
                "Player {} WON! Congratulations!!!",
                PLAYER_ASSOCIATION[((player + 1) % 2) as usize]
            ),
            GameState::Draw => println!("This round is a DRAW!!!"),
            GameState::Undecided => println!("No Winner yet. New ROUND begins!"),

            // We repeat this round once more, by not chaning the player in last step of while loop
            GameState::Invalid => continue,
        }

        player = (player + 1) % 2;
    }
}

/// Serves as round interface. Will collect input from user and return to caller, whether user
/// has won the round or not
///
/// # Arguments
///
/// * `player_symbol` - symbol identyfing current player
/// * `board` - board that the game is being played on.
///
/// # Returns
///
/// * `GameState` - notifies user about the round resolution.
///
fn play_round(player_id: i32, board: &mut [[char; BOARD_SIZE]; BOARD_SIZE]) -> GameState {
    let mut input = String::new();

    print_board(board);

    get_user_input(
        &mut input,
        format!(
            "Gracz {player_id} ({}), Twój ruch (wprowadź numer pola od 1 do 9): ",
            PLAYER_ASSOCIATION[player_id as usize]
        ),
    );

    let input: i32 = input.trim().parse().unwrap();
    let input = input - 1;

    if input < 0 || input > 8 {
        println!(
            "Liczba {} nie należy do zakresu od 1 do 9. Proszę podać liczbę ponownie.",
            input + 1
        );
        return GameState::Invalid;
    }

    let selected_field = &mut board[input as usize / BOARD_SIZE][input as usize % BOARD_SIZE];

    if *selected_field != ' ' {
        println!("Wybrane pole jest już zajęte. Proszę wybrać inne pole.");
        return GameState::Invalid;
    }

    *selected_field = PLAYER_ASSOCIATION[player_id as usize];

    check_game_state(board, player_id)
}

fn get_user_input(input: &mut String, message: String) {
    print!("{}", message);
    // Makes print to be flushed on stdout
    io::stdout().flush().unwrap();
    std::io::stdin().read_line(input).unwrap();
}

// TODO: jk: Adjust this function to print n-sized board
// btw this is chat-gpt generated func
fn print_board(board: &[[char; BOARD_SIZE]; BOARD_SIZE]) {
    for i in 0..3 {
        for j in 0..3 {
            if j < 2 {
                print!("{} | ", board[i][j]);
            } else {
                print!("{}", board[i][j]);
            }
        }
        println!();
        if i < 2 {
            println!("---------");
        }
    }
}

// TODO: jk: Adjust these functions to check for n-sized board
// btw this is chat-gpt generated func
fn check_game_state(board: &[[char; BOARD_SIZE]; BOARD_SIZE], player_id: i32) -> GameState {
    let player_symbol = if player_id == 0 { X_SYMBOL } else { O_SYMBOL };

    // Check rows and columns
    for i in 0..3 {
        if board[i][0] != ' ' && board[i][0] == board[i][1] && board[i][1] == board[i][2] {
            return if board[i][0] == player_symbol {
                GameState::Won
            } else {
                GameState::Lost
            };
        }
        if board[0][i] != ' ' && board[0][i] == board[1][i] && board[1][i] == board[2][i] {
            return if board[0][i] == player_symbol {
                GameState::Won
            } else {
                GameState::Lost
            };
        }
    }

    // Check diagonals
    if board[0][0] != ' ' && board[0][0] == board[1][1] && board[1][1] == board[2][2] {
        return if board[0][0] == player_symbol {
            GameState::Won
        } else {
            GameState::Lost
        };
    }
    if board[0][2] != ' ' && board[0][2] == board[1][1] && board[1][1] == board[2][0] {
        return if board[0][2] == player_symbol {
            GameState::Won
        } else {
            GameState::Lost
        };
    }

    // Check for draw or undecided
    for i in 0..3 {
        for j in 0..3 {
            if board[i][j] == ' ' {
                return GameState::Undecided;
            }
        }
    }

    GameState::Draw // No empty spaces and no winner, it's a draw
}
