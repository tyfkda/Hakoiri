mod hakoiri;

use std::error::Error;

use hakoiri::{parse_board, solve, Pos, Piece, SPACE, BOARD_W, BOARD_H};

fn print_board(positions: &[Pos], pieces: &[Piece]) {
    let mut s = [SPACE; BOARD_W * BOARD_H];
    for (pos, (c, size)) in positions.iter().zip(pieces) {
        let w = size.w();
        let h = size.h();
        for i in 0..h {
            for j in 0..w {
                s[(pos.y + i) as usize * BOARD_W + (pos.x + j) as usize] = *c;
            }
        }
    }
    for i in 0..BOARD_H {
        println!("{}", &s[(i * BOARD_W)..((i + 1) * BOARD_W)].iter().collect::<String>());
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let initial_arrange: String = [
        "1002",
        "1002",
        "3554",
        "3784",
        "6..9",
    ].join("");
    let (board, positions, pieces) = parse_board(initial_arrange)?;

    let Some((steps, hands, solved_positions)) = solve(&board, &positions, &pieces) else {
        println!("No solution");
        return Ok(());
    };

    println!("Solved!, steps={steps}");
    println!("Hands #{}: {:?}", hands.len(), &hands);
    print_board(&solved_positions, &pieces);

    Ok(())
}
