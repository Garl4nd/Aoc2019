use crate::{
    etc::intcode::{codeParser, talk_to_machine},
    Solution, SolutionPair,
};
use std::fs::read_to_string;

///////////////////////////////////////////////////////////////////////////////

pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = codeParser("input/25.txt");
    talk_to_machine(&code, "");
    let sol1: u64 = 0;
    let sol2: u64 = 0;

    (Solution::from(sol1), Solution::from(sol2))
}
