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
    Pawn { position: Position, color: Color },
    Knight { position: Position, color: Color },
    Bishop { position: Position, color: Color },
    Rook { position: Position, color: Color },
    Queen { position: Position, color: Color },
    King { position: Position, color: Color },
}

fn main() {
    let pawn = Chessman::Pawn {
        position: Position { x: 0, y: 1 },
        color: Color::White,
    };

    println!("{:?}", pawn);
}
