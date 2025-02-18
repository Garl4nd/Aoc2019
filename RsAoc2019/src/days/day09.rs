use crate::{intcode, Solution, SolutionPair};
use std::fs::read_to_string;

///////////////////////////////////////////////////////////////////////////////

pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = intcode::codeParser("input/9.txt");
    let sol1: u64 = *intcode::run_code(&code, &[1]).0.last().unwrap() as u64;
    let sol2: u64 = *intcode::run_code(&code, &[2]).0.last().unwrap() as u64;

    (Solution::from(sol1), Solution::from(sol2))
}
