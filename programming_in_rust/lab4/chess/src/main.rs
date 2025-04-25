#[derive(Debug)]
struct Position {
    x: u8,
    y: u8,
}

#[derive(Debug)]
enum Color {
    White,
    Black,
}

#[derive(Debug)]
enum Chessman {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Chessman {}

fn main() {
    println!("Hello, world!");
}
