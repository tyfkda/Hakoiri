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

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Size {
    pub w: u8,
    pub h: u8,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Dir {
    Left,
    Right,
    Up,
    Down,
}

pub type BoardStr = String;
pub type Piece = (char, Size);

pub const SPACE: char = '.';

pub fn board_at(board: &BoardStr, pos: &Pos) -> char {
    board.as_bytes()[pos.y as usize * BOARD_W + pos.x as usize] as char
}

pub fn find_spaces(mut board: &str) -> Vec<Pos> {
    let mut spaces = Vec::new();
    let mut start = 0;
    loop {
        if let Some(i) = board.find(SPACE) {
            let j = i + start;
            spaces.push(Pos {x: (j % BOARD_W) as u8, y: (j / BOARD_W) as u8});
            start += i + 1;
            board = &board[(i + 1)..];
        } else {
            break;
        }
    }
    spaces
}
