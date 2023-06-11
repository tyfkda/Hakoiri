use super::{BoardStr, Piece, Pos, Size, BOARD_H, BOARD_W, SPACE, board_at};

fn hex2decimal(c: char) -> Result<u8, ()> {
    if ('0'..='9').contains(&c) {
        Ok(c as u8 - '0' as u8)
    } else if ('A'..='F').contains(&c) {
        Ok(c as u8 - ('A' as u8 - 10))
    } else if ('a'..='f').contains(&c) {
        Ok(c as u8 - ('a' as u8 - 10))
    } else {
        Err(())
    }
}

fn parse_piece(board: &BoardStr, x: usize, y: usize) -> Result<Size, String> {
    let mut w = 1;
    let c = board_at(board, &Pos {x: x as u8, y: y as u8});
    while x + w < BOARD_W && board_at(board, &Pos {x: (x + w) as u8, y: y as u8}) == c {
        w += 1;
    }
    let mut h = 1;
    while y + h < BOARD_H && board_at(board, &Pos {x: x as u8, y: (y + h) as u8}) == c {
        for xx in x..x + w {
            if board_at(board, &Pos {x: xx as u8, y: (y + h) as u8}) != c {
                return Err(format!("Illegal at ({x}, {y}), ({w}, {h}), {}", c as char));
            }
        }
        h += 1;
    }
    return Ok(Size {w: w as u8, h: h as u8});
}

pub fn parse_board(board: String) -> Result<(BoardStr, Vec<Pos>, Vec<Piece>), String> {
    let mut bitboard = 0u64;
    let mut positions = Vec::new();
    let mut pieces = Vec::new();
    let mut x = 0;
    let mut y = 0;
    while y < BOARD_H {
        if bitboard & (1 << (y * BOARD_W + x)) == 0 {
            let c = board_at(&board, &Pos {x: x as u8, y: y as u8});
            if c != SPACE {
                let size = parse_piece(&board, x, y)?;
                for yy in y..y + size.h as usize {
                    for xx in x..x + size.w as usize {
                        bitboard |= 1 << (yy * BOARD_W + xx);
                    }
                }

                let pos = Pos {x: x as u8, y: y as u8};
                x += size.w as usize - 1;

                let i = hex2decimal(c as char)
                    .or_else(|_| Err(format!("Illegal character {}", c as char)))? as usize;
                if i >= positions.len() {
                    positions.resize(i + 1, Pos::default());
                    pieces.resize(i + 1, Piece::default());
                }
                positions[i] = pos;
                pieces[i] = (c, size);
            }
        }
        x += 1;
        if x >= BOARD_W {
            x = 0;
            y += 1;
        }
    }
    return Ok((board, positions, pieces));
}
