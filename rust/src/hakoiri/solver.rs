use std::collections::{HashSet, VecDeque};

use super::{BoardStr, Dir, Piece, Pos, Size, BOARD_H, BOARD_W, board_at, find_spaces};

const GOAL_POS: Pos = Pos {x: 1, y: 3};

pub fn solve(board: &BoardStr, positions: &[Pos], pieces: &[Piece]) -> Option<(i32, Solution, Vec<Pos>)> {
    let positions = positions.into_iter().map(|p| p.clone()).collect::<Vec<_>>();
    let mut solver = Solver::new(pieces);
    solver.solve(board.clone(), positions)
}

pub type Solution = Vec<(usize, Dir)>;

#[derive(Clone, Debug, PartialEq)]
pub struct Solver<'a> {
    pieces: &'a [Piece],
}

impl<'a> Solver<'a> {
    fn new(pieces: &'a [Piece]) -> Self {
        Self {
            pieces,
        }
    }

    fn solve(&mut self, board: BoardStr, positions: Vec<Pos>) -> Option<(i32, Solution, Vec<Pos>)> {
        let mut deq = VecDeque::from([(0, board, positions, Vec::new() as Vec<(usize, Dir)>)]);
        let mut board_hashes: HashSet<BoardStr> = HashSet::new();

        // let mut check_count = 0;
        while !deq.is_empty() {
            // check_count += 1;
            let (steps, board, positions, hands) = deq.pop_front().unwrap();
            if board_hashes.contains(&board) { continue; }
            self.register_board(&mut board_hashes, &board);
            if self.is_solved(&board, &positions) {
                return Some((steps, hands, positions));
            }

            let movables = self.find_movable_pieces(&board, &positions);
            for (i, dir) in movables {
                let mut board2 = board.clone();
                let mut pos = positions[i].clone();
                self.move_piece(&mut board2, &mut pos, i, dir);
                if !board_hashes.contains(&board2) {
                    let mut positions2 = positions.clone();
                    positions2[i] = pos;
                    let mut hands2 = hands.clone();
                    hands2.push((i, dir));
                    if !hands.is_empty() && i == hands[hands.len() - 1].0 {
                        deq.push_front((steps, board2, positions2, hands2));
                    } else {
                        deq.push_back((steps + 1, board2, positions2, hands2));
                    }
                }
            }
        }
        None
    }

    fn register_board(&self, board_hashes: &mut HashSet<BoardStr>, board: &BoardStr) {
        board_hashes.insert(board.clone());

        let mut flipx = board.clone();
        for y in 0..BOARD_H {
            let i = y * BOARD_W;
            for x in 0..BOARD_W / 2 {
                flipx.b.swap(i + x, i + BOARD_W - 1 - x);
            }
        }
        board_hashes.insert(flipx);
    }

    fn is_solved(&self, _board: &BoardStr, positions: &[Pos]) -> bool {
        positions[0] == GOAL_POS
    }

    fn move_piece(&mut self, board: &mut BoardStr, pos: &mut Pos, i: usize, dir: Dir) {
        let (_, size) = self.pieces[i];
        put_board(board, pos, size, Size::Empty);
        match dir {
            Dir::Left  => pos.x -= 1,
            Dir::Right => pos.x += 1,
            Dir::Up    => pos.y -= 1,
            Dir::Down  => pos.y += 1,
        }
        put_board(board, pos, size, size);
    }

    fn find_movable_pieces(&self, board: &BoardStr, positions: &[Pos]) -> Vec<(usize, Dir)> {
        let spaces = find_spaces(board);

        let mut movables = Vec::new();
        for i in 0..positions.len() {
            for space in &spaces {
                if let Some(dir) = is_adjacent(&positions[i], self.pieces[i].1, space) {
                    if is_movable(&positions[i], self.pieces[i].1, dir, &board) {
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

fn is_adjacent(pos: &Pos, size: Size, space: &Pos) -> Option<Dir> {
    let w = size.w();
    let h = size.h();
    if space.x >= pos.x && space.x < pos.x + w {
        if space.y + 1 == pos.y {
            return Some(Dir::Up);
        } else if space.y == pos.y + h {
            return Some(Dir::Down);
        }
    }
    if space.y >= pos.y && space.y < pos.y + h {
        if space.x + 1 == pos.x {
            return Some(Dir::Left);
        } else if space.x == pos.x + w {
            return Some(Dir::Right);
        }
    }
    None
}

fn is_movable(pos: &Pos, size: Size, dir: Dir, board: &BoardStr) -> bool {
    let w = size.w();
    let h = size.h();
    if dir == Dir::Left || dir == Dir::Right {
        if dir == Dir::Left { if pos.x <= 0 { return false; } }
        else { if pos.x + w >= BOARD_W as u8 { return false; } }

        let ax = if dir == Dir::Left { pos.x - 1} else { pos.x + w };
        (0..h).all(|i| board_at(board, &Pos {x: ax, y: pos.y + i}) == Size::Empty)
    } else {
        if dir == Dir::Up { if pos.y <= 0 { return false; } }
        else { if pos.y + h >= BOARD_H as u8 { return false; } }

        let ay = if dir == Dir::Up { pos.y - 1} else { pos.y + h };
        (0..w).all(|i| board_at(board, &Pos {x: pos.x + i, y: ay}) == Size::Empty)
    }
}

fn put_board(board: &mut BoardStr, pos: &Pos, size: Size, s: Size) {
    let w = size.w();
    for i in 0..size.h() {
        for j in 0..w {
            board.b[(pos.y + i) as usize * BOARD_W + (pos.x + j) as usize] = s;
        }
    }
}
