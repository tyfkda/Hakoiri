mod hakoiri;

use clap::Parser;
use std::error::Error;
use std::io::{self, BufRead, Write};

use hakoiri::{parse_board, solve, Dir, Piece, Pos, SPACE, BOARD_W, BOARD_H};

#[derive(Parser, Debug)]
#[command(about, long_about = None)]
struct Args {
    /// Print solution interactively.
    #[arg(long)]
    interactive: bool,

    /// Initial board arrangement (length: 5x4).
    #[arg(long)]
    board: Option<String>,
}

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

fn put_arrow(s: &mut [char; (BOARD_W * 4) * (BOARD_H * 2)], x: usize, y: usize, arrow: char, dx: usize, dy: usize) {
    let line_width = BOARD_W * 4;
    s[y * line_width + x] = arrow;
    s[(y + dy) * line_width + (x + dx)] = arrow;
}

fn print_interactive_board(positions: &[Pos], pieces: &[Piece], hand_opt: Option<(usize, Dir)>) {
    let mut s = [' '; (BOARD_W * 4) * (BOARD_H * 2)];
    let line_width = BOARD_W * 4;
    for (pos, (c, size)) in positions.iter().zip(pieces) {
        let w = size.w() as usize;
        let h = size.h() as usize;
        for i in 0..h * 2 {
            let y = (pos.y as usize) * 2 + i;
            for j in 0..w * 4 {
                let x = (pos.x as usize) * 4 + j;
                let cc = if i == 0 {
                    if j == 0              { *c }
                    else if j == w * 4 - 1 { '┓' }
                    else                   { '━' }
                } else if i == h * 2 - 1 {
                    if j == 0              { '┗' }
                    else if j == w * 4 - 1 { '┛' }
                    else                   { '━' }
                } else {
                    if j == 0 || j == w * 4 - 1 { '┃' }
                    else if j & 1 != 0          { '<' }
                    else                        { '>' }
                };
                s[y * line_width + x] = cc;
            }
        }
    }

    if let Some((ip, dir)) = hand_opt {
        let pos = &positions[ip];
        let size = pieces[ip].1;
        let w = size.w() as usize;
        let h = size.h() as usize;
        let x = (pos.x as usize) * 4;
        let y = (pos.y as usize) * 2;
        match dir {
            hakoiri::Dir::Left =>  { put_arrow(&mut s, x - 1,         y + h - 1, '←', 0, 1); },
            hakoiri::Dir::Right => { put_arrow(&mut s, x + w * 4    , y + h - 1, '→', 0, 1); },
            hakoiri::Dir::Up =>    { put_arrow(&mut s, x + w * 2 - 1, y - 1,     '↑', 1, 0); },
            hakoiri::Dir::Down =>  { put_arrow(&mut s, x + w * 2 - 1, y + h * 2, '↓', 1, 0); },
        };
    }

    for i in 0..BOARD_H * 2 {
        println!("{}", &s[(i * BOARD_W * 4)..((i + 1) * BOARD_W * 4)].iter().collect::<String>());
    }
}

fn cls() {
    print!("\x1b[2J");
}

fn locate(x: usize, y: usize) {
    print!("\x1b[{};{}H", y + 1, x + 1);
}

fn clear_line_after() {
    print!("\x1b[K");
}

fn print_solution_interactive(positions: &Vec<Pos>, pieces: &[Piece], hands: &[(usize, hakoiri::Dir)]) {
    let mut positions = positions.clone();
    let stdin = io::stdin();
    let mut step = 0;
    let mut step2 = 0;
    let mut line = String::new();
    let mut last = usize::MAX;

    cls();
    loop {
        locate(0, 0);
        clear_line_after();
        let hand_opt = if step >= hands.len() {
            println!("step #{step2:3}: ");
            None
        } else {
            let (ip, dir) = hands[step];
            let arrow = match dir {
                hakoiri::Dir::Left =>  "←",
                hakoiri::Dir::Right => "→",
                hakoiri::Dir::Up =>    "↑",
                hakoiri::Dir::Down =>  "↓",
            };
            println!("step #{:3}: {:}, {arrow:}", step2 + 1, pieces[ip].0);
            Some((ip, dir))
        };
        print_interactive_board(&positions, pieces, hand_opt);
        clear_line_after();
        if step >= hands.len() {
            println!("Solved!");
            break;
        }

        // Wait input.
        print!("> ");
        io::stdout().flush().unwrap();
        stdin.lock().read_line(&mut line).unwrap();

        let (ip, dir) = hands[step];
        let pos = &mut positions[ip];
        match dir {
            hakoiri::Dir::Left =>  pos.x -= 1,
            hakoiri::Dir::Right => pos.x += 1,
            hakoiri::Dir::Up =>    pos.y -= 1,
            hakoiri::Dir::Down =>  pos.y += 1,
        }
        step += 1;
        if last != ip {
            step2 += 1;
        }
        last = ip;
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let initial_arrange = if let Some(board) = args.board {
        if board.len() != BOARD_W * BOARD_H {
            return Err("Invalid board length".into());
        }
        board
    } else {
        [
            "1002",
            "1002",
            "3554",
            "3784",
            "6..9",
        ].join("")
    };

    let (board, positions, pieces) = parse_board(initial_arrange)?;

    let Some((steps, hands, solved_positions)) = solve(&board, &positions, &pieces) else {
        println!("No solution");
        return Ok(());
    };

    if args.interactive {
        print_solution_interactive(&positions, &pieces, &hands);
    } else {
        println!("Solved!, steps={steps}");
        println!("Hands #{}: {:?}", hands.len(), &hands);
        print_board(&solved_positions, &pieces);
    }

    Ok(())
}
