use console::Term;
use itertools::Itertools;

use crate::{
    etc::{
        gridvec::GridVec,
        intcode::{codeParser, run_code, IntMachine, MachineState},
    },
    Solution, SolutionPair,
};
use core::panic;

///////////////////////////////////////////////////////////////////////////////
// solution1 :: [Int] -> Int
// solution1 code =
//   let
//     output = machineOutputs $ runCodeWInputST [] code
//     objects = chunksOf 3 output
//     tilePos = [(x, y) | [x, y, objId] <- objects, objId == 2]
//    in
//     length tilePos
fn solution1(code: &[i64]) -> usize {
    let output = run_code(code, &[]).0;
    output
        .into_iter()
        .chunks(3)
        .into_iter()
        .filter_map(|chunk_iter| {
            chunk_iter.dropping(2).next().and_then(|third_val| {
                if matches!(third_val, 2) {
                    Some(())
                } else {
                    None
                }
            })
        })
        .count()
}
fn char_to_int(c: char) -> i64 {
    c as i64 - 107
}
fn play_game(code: &[i64]) -> i64 {
    let (_, tail) = code.split_at(1);
    let modified_code = [&[2], tail].concat();
    let mut machine = IntMachine::new(&modified_code);
    let mut game_ar = GridVec::new(' ', 0, 0, 33, 50);
    let mut auto_input = 0;
    let mut score = 0;
    let term = Term::stdout();
    while machine.state != MachineState::Halted {
        let _ = term.clear_screen();
        println!("{score}");
        print!("{game_ar}");
        let user_input = term.read_char();
        if let Ok(user_input) = user_input {
            let input = if user_input == 'a' {
                auto_input
            } else {
                char_to_int(user_input)
            }; // char to ord -107
            if input == -4 {
                return play_game(code);
            };
            if !(-1..=1).contains(&input) {
                auto_input = 0;
                continue;
            }
            let outputs = machine.run_machine(&[input]);
            if outputs.is_empty() {
                auto_input = 0;
                continue;
            }
            type ITriplet = (i64, i64, i64);
            let out_triplets: Vec<ITriplet> = outputs
                .into_iter()
                .chunks(3)
                .into_iter()
                .map(|mut it| (it.next().unwrap(), it.next().unwrap(), it.next().unwrap()))
                .collect();
            let (res, updates) = out_triplets
                .iter()
                .partition::<Vec<ITriplet>, _>(|(x, y, _)| *x == -1 && *y == 0);

            let platform_pos = updates
                .iter()
                .find_map(|(x, _, piece)| if *piece == 3 { Some(*x) } else { None })
                .unwrap_or(0);
            let ball_pos = updates
                .iter()
                .find_map(|(x, _, piece)| if *piece == 4 { Some(*x) } else { None })
                .unwrap_or(0);
            // let mut ball_pos_it  = updates.iter().filter_map(|(x,_, piece)| if *piece == 4 {Some(*x)} else {None});
            //let ball_pos = if let Some(pos) = ball_pos_it.next() {pos} else {0};
            if let Some((_, _, new_score)) = res.first() {
                score = *new_score;
            }

            auto_input = if ball_pos > platform_pos { 1 } else { -1 };
            for (x, y, id) in updates {
                game_ar[(y, x)] = obj_id_to_char(id);
            }
        }
    }
    println! {"{} Score = {}\n Play again (y/n)?", if game_ar.vec.contains
    (&'!') {"Game over!"} else {"You won!"}, score};
    if let Ok('y') = term.read_char() {
        play_game(code)
    } else {
        score
    }
}
fn play_game_auto(code: &[i64]) -> i64 {
    let (_, tail) = code.split_at(1);
    let modified_code = [&[2], tail].concat();
    let mut machine = IntMachine::new(&modified_code);
    let mut game_ar = GridVec::new(' ', 0, 0, 33, 50);
    let mut input = 0;
    let mut score = 0;
    while machine.state != MachineState::Halted {
        let outputs = machine.run_machine(&[input]);
        if outputs.is_empty() {
            input = 0;
            continue;
        }
        type ITriplet = (i64, i64, i64);
        let out_triplets: Vec<ITriplet> = outputs
            .into_iter()
            .chunks(3)
            .into_iter()
            .map(|mut it| (it.next().unwrap(), it.next().unwrap(), it.next().unwrap()))
            .collect();
        let (res, updates) = out_triplets
            .iter()
            .partition::<Vec<ITriplet>, _>(|(x, y, _)| *x == -1 && *y == 0);
        let platform_pos = updates
            .iter()
            .find_map(|(x, _, piece)| if *piece == 3 { Some(*x) } else { None })
            .unwrap_or(0);
        let ball_pos = updates
            .iter()
            .find_map(|(x, _, piece)| if *piece == 4 { Some(*x) } else { None })
            .unwrap_or(0);
        if let Some((_, _, new_score)) = res.first() {
            score = *new_score;
        }
        input = if ball_pos > platform_pos { 1 } else { -1 };
        for (x, y, id) in updates {
            game_ar[(y, x)] = obj_id_to_char(id);
        }
    }

    score
}

fn obj_id_to_char(id: i64) -> char {
    match id {
        0 => ' ',
        1 => 'x',
        2 => '!',
        3 => 'T',
        4 => 'O',
        _ => panic!("Wrong id"),
    }
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = codeParser("input/13.txt");
    let sol1: usize = solution1(&code);
    play_game(&code);
    let sol2: i64 = play_game_auto(&code);

    (Solution::from(sol1), Solution::from(sol2))
}
