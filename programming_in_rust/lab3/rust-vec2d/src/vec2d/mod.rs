use std::fmt::Display;
use std::fmt::Formatter;
use std::ops::{Add, Sub};

#[derive(Debug)] // allows to print the structure in debug mode (ie. to use {:?})
pub struct Vec2d {
    pub x: f32,
    pub y: f32,
}

impl Display for Vec2d {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {}]", self.x, self.y)
    }
}

impl Vec2d {
    pub fn unit(x: f32, y: f32) -> Self {
        let length = (x.powi(2) + y.powi(2)).sqrt();
        Vec2d {
            x: x / length,
            y: y / length,
        }
    }
}

impl Add for Vec2d {
    type Output = Self; // Output describes the resulting type after applying the + operator.
                        // Self describes the current type, here it means Vec2d

    fn add(self, other: Vec2d) -> Self {
        Vec2d {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Sub for Vec2d {
    type Output = Self;

    fn sub(self, other: Vec2d) -> Self {
        Vec2d {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unit_vector_has_length_1() {
        let v = Vec2d::unit(3.0, 4.0);
        let length = (v.x.powi(2) + v.y.powi(2)).sqrt();
        assert!((length - 1.0).abs() < 1e-5);
    }

    #[test]
    fn add_two_vectors() {
        let u = Vec2d { x: 2.0, y: 4.0 };
        let v = Vec2d { x: 2.0, y: 1.0 };

        let got = u + v;
        let want = Vec2d { x: 4.0, y: 5.0 };

        assert!(got.x == want.x && got.y == want.y);
    }

    #[test]
    fn subtract_two_vectors() {
        let u = Vec2d { x: 2.0, y: 4.0 };
        let v = Vec2d { x: 2.0, y: 1.0 };

        let got = u - v;
        let want = Vec2d { x: 0.0, y: 3.0 };

        assert!(got.x == want.x && got.y == want.y);
    }
}
