use std::collections::{HashSet, VecDeque};

use super::{BoardStr, Dir, Piece, Pos, Size, BOARD_H, BOARD_W, SPACE, board_at, find_spaces};

const GOAL_POS: Pos = Pos {x: 1, y: 3};

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
        let mut deq = VecDeque::from([(0, board.clone(), positions.clone(), Vec::new() as Vec<(usize, Dir)>)]);
        let mut board_hashes: HashSet<BoardStr> = HashSet::new();

        let mut check_count = 0;
        while !deq.is_empty() {
            check_count += 1;
            let (steps, board, positions, hands) = deq.pop_front().unwrap();
            if board_hashes.contains(&board) { continue; }
            board_hashes.insert(board.clone());
            if self.is_solved(&board, &positions) {
                println!("Solved!, steps={steps}, check=#{check_count}, left={}, hash={}", deq.len(), board_hashes.len());
                println!("Hands #{}: {:?}", hands.len(), &hands);
                print_board(&board);
                break;
            }

            let movables = self.find_movable_pieces(&board, &positions);
            for (i, dir) in movables {
                let mut board2 = board.clone();
                let mut pos = positions[i].clone();
                self.move_piece(&mut board2, &mut pos, i, dir);
                if !board_hashes.contains(&board2) {
                    let mut positions2 = positions.clone();
                    positions2[i] = pos;
                    let steps2 = steps + if !hands.is_empty() && i == hands[hands.len() - 1].0 { 0 } else { 1 };
                    let mut hands2 = hands.clone();
                    hands2.push((i, dir));
                    deq.push_back((steps2, board2, positions2, hands2));
                }
            }
        }
    }

    fn is_solved(&self, _board: &BoardStr, positions: &Vec<Pos>) -> bool {
        positions[0] == GOAL_POS
    }

    fn move_piece(&mut self, board: &mut BoardStr, pos: &mut Pos, i: usize, dir: Dir) {
        let (c, size) = &self.pieces[i];
        put_board(board, pos, size, SPACE);
        match dir {
            Dir::Left  => pos.x -= 1,
            Dir::Right => pos.x += 1,
            Dir::Up    => pos.y -= 1,
            Dir::Down  => pos.y += 1,
        }
        put_board(board, pos, size, *c);
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

fn put_board(board: &mut BoardStr, pos: &Pos, size: &Size, c: char) {
    for i in 0..size.h {
        for j in 0..size.w {
            unsafe { board.as_bytes_mut()[(pos.y + i) as usize * BOARD_W + (pos.x + j) as usize] = c as u8; }
        }
    }
}

fn print_board(board: &BoardStr) {
    for i in 0..BOARD_H {
        println!("{:}", &board[(i * BOARD_W)..((i + 1) * BOARD_W)]);
    }
}
