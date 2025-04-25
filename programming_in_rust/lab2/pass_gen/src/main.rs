fn main() {
    let s = "3-598-21508-8";
    println!("{}", is_valid_isbn(s));
}

// TODO: add edge cases here
fn is_valid_isbn(s: &str) -> bool {
    let mut res = 0;
    let mut i = 0;

    for c in s.chars() {
        println!("Analyzing char: {} at index: {}", c, i);
        if c == '-' {
            println!("Inside -");
            i -= 1;
        } else if c.is_digit(10) {
            println!("c Is Digit");
            res += (10 - i as i32) * c.to_digit(10).unwrap() as i32;
        } else if c == 'X' {
            println!("c is X");
            res += (10 - i as i32) * 10
        } else {
            println!("Reached final statement");
            return false;
        }

        i += 1;
    }

    res % 11 == 0
}
