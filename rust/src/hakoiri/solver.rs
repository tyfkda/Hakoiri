use super::{BoardStr, Piece, Pos, find_spaces};

pub fn solve(board: BoardStr, positions: Vec<Pos>, pieces: Vec<Piece>) {
    let mut solver = Solver::new(pieces);
    solver.solve(board, positions);
}

#[derive(Clone, Debug, PartialEq)]
pub struct Solver {
    pieces: Vec<Piece>,
}

impl Solver {
    fn new(pieces: Vec<Piece>) -> Self {
        Self {
            pieces,
        }
    }

    fn solve(&mut self, board: BoardStr, positions: Vec<Pos>) {
        println!("Positions: {:?}", &positions);
        let spaces = find_spaces(&board);
        println!("Spaces: {:?}", &spaces);
    }
}
