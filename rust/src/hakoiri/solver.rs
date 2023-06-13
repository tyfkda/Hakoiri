use super::{BoardStr, Dir, Piece, Pos, Size, BOARD_H, BOARD_W, SPACE, board_at, find_spaces};

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
        let movables = self.find_movable_pieces(&board, &positions);
        println!("Movables: {:?}", &movables);
    }

    fn find_movable_pieces(&self, board: &BoardStr, positions: &[Pos]) -> Vec<(usize, Dir)> {
        let spaces = find_spaces(board);

        let mut movables = Vec::new();
        for i in 0..positions.len() {
            for space in &spaces {
                if let Some(dir) = is_adjacent(&positions[i], &self.pieces[i].1, space) {
                    if is_movable(&positions[i], &self.pieces[i].1, dir, &board) {
                        let target = (i, dir);
                        if movables.iter().all(|e| *e != target) {
                            movables.push((i, dir));
                        }
                    }
                }
            }
        }
        movables
    }
}

fn is_adjacent(pos: &Pos, size: &Size, space: &Pos) -> Option<Dir> {
    if space.x >= pos.x && space.x < pos.x + size.w {
        if space.y + 1 == pos.y {
            return Some(Dir::Up);
        } else if space.y == pos.y + size.h {
            return Some(Dir::Down);
        }
    }
    if space.y >= pos.y && space.y < pos.y + size.h {
        if space.x + 1 == pos.x {
            return Some(Dir::Left);
        } else if space.x == pos.x + size.w {
            return Some(Dir::Right);
        }
    }
    None
}

fn is_movable(pos: &Pos, size: &Size, dir: Dir, board: &BoardStr) -> bool {
    if dir == Dir::Left || dir == Dir::Right {
        if dir == Dir::Left { if pos.x <= 0 { return false; } }
        else { if pos.x + size.w >= BOARD_W as u8 { return false; } }

        let ax = if dir == Dir::Left { pos.x - 1} else { pos.x + size.w };
        (0..size.h).all(|i| board_at(board, &Pos {x: ax, y: pos.y + i}) == SPACE)
    } else {
        if dir == Dir::Up { if pos.y <= 0 { return false; } }
        else { if pos.y + size.h >= BOARD_H as u8 { return false; } }

        let ay = if dir == Dir::Up { pos.y - 1} else { pos.y + size.h };
        (0..size.w).all(|i| board_at(board, &Pos {x: pos.x + i, y: ay}) == SPACE)
    }
}
