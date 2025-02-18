use crate::{etc::intcode::run_code, intcode, Solution, SolutionPair};
use std::fs::read_to_string;
///////////////////////////////////////////////////////////////////////////////

pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = intcode::codeParser("input/5.txt");
    let sol1 = *intcode::run_code(&code, &[1]).0.last().unwrap();
    let sol2 = *intcode::run_code(&code, &[5]).0.last().unwrap();

    (Solution::from(sol1 as u64), Solution::from(sol2 as u64))
}
