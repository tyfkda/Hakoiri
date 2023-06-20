use super::{BoardStr, Piece, Pos, Size, BOARD_H, BOARD_W, SPACE};

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

pub fn rawboard_at(boardstr: &String, pos: &Pos) -> char {
    boardstr.as_bytes()[pos.y as usize * BOARD_W + pos.x as usize] as char
}

fn parse_piece(rawboard: &String, x: usize, y: usize) -> Result<Size, String> {
    let mut w = 1;
    let c = rawboard_at(rawboard, &Pos {x: x as u8, y: y as u8});
    while x + w < BOARD_W && rawboard_at(rawboard, &Pos {x: (x + w) as u8, y: y as u8}) == c {
        w += 1;
    }
    let mut h = 1;
    while y + h < BOARD_H && rawboard_at(rawboard, &Pos {x: x as u8, y: (y + h) as u8}) == c {
        for xx in x..x + w {
            if rawboard_at(rawboard, &Pos {x: xx as u8, y: (y + h) as u8}) != c {
                return Err(format!("Illegal at ({x}, {y}), ({w}, {h}), {}", c as char));
            }
        }
        h += 1;
    }
    assert!(w <= 2);
    assert!(h <= 2);
    let size = match (w, h) {
        (1, 1) => Size::S1x1,
        (2, 1) => Size::S2x1,
        (1, 2) => Size::S1x2,
        (2, 2) => Size::S2x2,
        (_, _) => { panic!("Illegal size: ({w}, {h})"); },
    };
    return Ok(size);
}

pub fn parse_board(rawboard: String) -> Result<(BoardStr, Vec<Pos>, Vec<Piece>), String> {
    let mut bitboard = 0u64;
    let mut board = vec![Size::Empty; BOARD_W * BOARD_H];
    let mut positions = Vec::new();
    let mut pieces = Vec::new();
    let mut x = 0;
    let mut y = 0;
    while y < BOARD_H {
        if bitboard & (1 << (y * BOARD_W + x)) == 0 {
            let c = rawboard_at(&rawboard, &Pos {x: x as u8, y: y as u8});
            if c != SPACE {
                let size = parse_piece(&rawboard, x, y)?;
                let w = size.w();
                let h = size.h();
                for yy in y..y + h as usize {
                    for xx in x..x + w as usize {
                        bitboard |= 1 << (yy * BOARD_W + xx);
                        board[yy * BOARD_W + xx] = size;
                    }
                }

                let pos = Pos {x: x as u8, y: y as u8};
                x += w as usize - 1;

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
    return Ok((BoardStr {b: board}, positions, pieces));
}
