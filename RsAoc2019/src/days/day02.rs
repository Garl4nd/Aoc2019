use crate::{etc::intcode::run_code, intcode, Solution, SolutionPair};
use std::fs::read_to_string;
// fn runCode(code: &Vec<usize>, noun: usize, verb: usize) -> Vec<usize> {
//     let mut newCode = code.clone();
//     newCode[1] = noun;
//     newCode[2] = verb;
//     for idx in (0..code.len()).step_by(4) {
//         match newCode[idx] {
//             1 => {
//                 let (inputPos1, inputPos2, outputPos) =
//                     (newCode[idx + 1], newCode[idx + 2], newCode[idx + 3]);
//                 newCode[outputPos] = newCode[inputPos1] + newCode[inputPos2];
//             } // newCode[idx+1] + newCode[idx+2];}
//             2 => {
//                 let (inputPos1, inputPos2, outputPos) =
//                     (newCode[idx + 1], newCode[idx + 2], newCode[idx + 3]);
//                 newCode[outputPos] = newCode[inputPos1] * newCode[inputPos2];
//             } // newCode[idx+1] + newCode[idx+2];}
//             99 => {
//                 break;
//             }
//             _ => {
//                 continue;
//             }
//         }
//     }
//     newCode
// }
fn solution1(code: &Vec<usize>) -> usize {
    let mut new_code = code.clone();
    new_code[1] = 12;
    new_code[2] = 2;
    //dbg!(runCode(code, 12, 2)[0]);
    let machine = run_code(&new_code, &[]);    
    machine.code[0]
}
fn solution2(code: &Vec<usize>) -> usize {
    let target: usize = 19690720;
    for noun in (0..=99) {
        for verb in (0..=99) {
            let mut new_code = code.clone();
            new_code[1] = noun;
            new_code[2] = verb;
            if run_code(&new_code, &[]).code[0] == target {
                return (100 * noun + verb) as usize;
            }
        }
    }
    0
}

// fn parseFile(filename: &str) -> Vec<usize> {
//     let file: String = read_to_string(filename).unwrap().lines().take(1).collect();
//     println!("vals = {:?}", file.split(',').collect::<Vec<_>>());
//     file.split(',').map(|s| s.parse().unwrap()).collect()
// }

///////////////////////////////////////////////////////////////////////////////

pub fn solve() -> SolutionPair {
    // let vec = parseFile("input/2.txt");

    let code = intcode::codeParser("input/2.txt");
    let sol1: u64 = solution1(&code) as u64;
    let sol2: u64 = solution2(&code) as u64;
    let mut machine = intcode::IntMachine::new(&code);
    machine.run_machine(&Vec::new());
    //dbg!(machine.code);
    (Solution::from(sol1), Solution::from(sol2))
}
