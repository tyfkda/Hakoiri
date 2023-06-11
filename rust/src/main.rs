mod hakoiri;

use std::error::Error;

use hakoiri::{parse_board, solve};

fn main() -> Result<(), Box<dyn Error>> {
    let initial_arrange: String = [
        "1002",
        "1002",
        "3554",
        "3784",
        "6..9",
    ].join("");
    let (board, positions, pieces) = parse_board(initial_arrange)?;

    solve(board, positions, pieces);

    Ok(())
}
