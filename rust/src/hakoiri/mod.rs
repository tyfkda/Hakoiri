mod piece;
mod solver;

pub use piece::parse_board;
pub use solver::solve;

pub const BOARD_W: usize = 4;
pub const BOARD_H: usize = 5;


#[derive(Clone, Debug, Default, PartialEq)]
pub struct Pos {
    pub x: u8,
    pub y: u8,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub enum Size {
    S1x1 = 0,
    S2x1 = 1,
    S1x2 = 2,
    S2x2 = 3,
    #[default]
    Empty = 4,
}

impl Size {
    pub fn w(self) -> u8 {
        (self as u8 & 1) + 1
    }

    pub fn h(self) -> u8 {
        (self as u8 >> 1) + 1
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Dir {
    Left,
    Right,
    Up,
    Down,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
// pub type BoardStr = Vec<Option<Size>>;
pub struct BoardStr { b: Vec<Size>, }

pub type Piece = (char, Size);

pub const SPACE: char = '.';

pub fn board_at(board: &BoardStr, pos: &Pos) -> Size {
    board.b[pos.y as usize * BOARD_W + pos.x as usize]
}

pub fn find_spaces(board: &BoardStr) -> Vec<Pos> {
    let mut spaces = Vec::new();
    let mut start = 0;
    let mut b = &board.b[0..];
    loop {
        if let Some(i) = b.iter().position(|x| *x == Size::Empty) {
            let j = i + start;
            spaces.push(Pos {x: (j % BOARD_W) as u8, y: (j / BOARD_W) as u8});
            start += i + 1;
            b = &b[(i + 1)..];
        } else {
            break;
        }
    }
    spaces
}
