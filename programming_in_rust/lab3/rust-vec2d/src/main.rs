mod vec2d;

use vec2d::Vec2d;

fn main() {
    //let r = Vec2d { x: 1.0, y: 2.0 };
    let r2 = Vec2d::unit(5.0, 3.0);

    println!("{}", r2);
}
