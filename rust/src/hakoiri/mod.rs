mod piece;

pub use piece::parse_board;

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

pub type BoardStr = String;
pub type Piece = (char, Size);

pub const SPACE: char = ' ';

pub fn board_at(board: &BoardStr, pos: &Pos) -> char {
    board.as_bytes()[pos.y as usize * BOARD_W + pos.x as usize] as char
}
