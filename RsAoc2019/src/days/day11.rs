use itertools::Itertools;

use crate::{
    etc::{
        gridvec::{change_direction, turn, Direction, GridPos, GridVec},
        intcode::{self, codeParser, IntMachine, MachineState},
    },
    Solution, SolutionPair,
};
use std::{
    fmt::Display,
    fs::read_to_string,
    ops::{Index, IndexMut, RangeBounds},
};

fn paint_ship(init_tile: bool, code: &[i64]) -> (GridVec<bool>, GridVec<bool>) {
    let mut ship_grid = GridVec::<bool>::new(false, -45, -45, 45, 45);
    ship_grid[(0, 0)] = init_tile;
    let mut painted_panels = GridVec::<bool>::new(false, -45, -45, 45, 45);
    let mut computer = IntMachine::new(code);
    let mut current_pos = (0, 0);
    let mut current_dir = Direction::U;
    loop {
        let current_color = ship_grid[current_pos];
        let outputs = computer.run_machine(&[if current_color { 1 } else { 0 }]);
        if computer.state == MachineState::Halted {
            break;
        } else {
            let (new_color, turn_code) = (outputs[0], outputs[1]);
            ship_grid[current_pos] = new_color == 1;
            if new_color == 1 {
                painted_panels[current_pos] = true
            };
            current_dir = change_direction(&current_dir, turn_code);
            current_pos = turn(&current_pos, &current_dir);
        }
    }
    (ship_grid, painted_panels)
}
fn solution1(code: &[i64]) -> u64 {
    let (_, painted_panels) = paint_ship(false, code);
    painted_panels.vec.into_iter().filter(|el| *el).count() as u64
}
fn solution2(code: &[i64]) -> String {
    let (final_paint_encoded, _) = paint_ship(true, code);
    let finalPaint = final_paint_encoded.remap(|val| if *val { 'O' } else { ' ' }); /*  (\x -> if x>0 then 'O' else ' ') <$> finalPaintEncoded */
    format!("{}", finalPaint)
    //in unlines $  charGridToStr finalPaint
}

pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = codeParser("input/11.txt");
    let sol1: u64 = solution1(&code);
    let sol2: String = solution2(&code);

    (Solution::from(sol1), Solution::from(sol2))
}
