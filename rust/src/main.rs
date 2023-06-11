mod hakoiri;

use hakoiri::{parse_board};

fn main() {
    let initial_arrange: String = [
        "1002",
        "1002",
        "3554",
        "3784",
        "6..9",
    ].join("");
    let arrange = parse_board(initial_arrange);

    println!("{:?}", &arrange);
}
