use console::Term;
use itertools::Itertools;

use crate::{
    etc::{
        gridvec::{self, GridVec},
        intcode::{codeParser, run_code, IntMachine},
    },
    Solution, SolutionPair,
};
use std::fs::read_to_string;

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
        .filter_map(|mut chunk_iter| {
            chunk_iter.next();
            chunk_iter.next();
            chunk_iter.next()
        })
        .filter(|val| matches!(val, 2))
        .count()
}
fn play_game(code: &[i64]) {
    let (head, tail) = code.split_at(1);
    let modified_code = [&[2], tail].concat();
    let machine = IntMachine::new(&modified_code);
    let mut game_ar = GridVec::new(' ', 0, 0, 50, 50);
    let mut auto_input = 0;
    let mut score = 0;
    let mut term = Term::stdout();
    loop {
        let user_input = term.read_char();
        if let Ok(user_input) = user_input {
            let input = if user_input == 'a' { auto_input } else { 0 }; // char to ord -107
                                                                        // conversion
        }
    }
}

// playGame :: [Int] -> IO ()
// playGame code = do
//   machine <- createMachine @IOArray (2 : tail code)
//   let gameAr = A.listArray ((0, 0), (dim, dim)) [' ' | _ <- [0 .. dim], _ <- [0 .. dim]]
//
//   flip fix (gameAr, 0, 0) $ \loop (currentAr, autoInput, score) -> do
//     userInput <- getChar
//     let input = if userInput == 'a' then autoInput else subtract 107 . ord $ userInput
//     when (input == -4) $ playGame code
//     when (input < -1 || input > 1) $ do
//       -- putStrLn "Invalid input!"
//       loop (currentAr, 0, score)
//     -- print input
//
//     outputs <- getOutputs =<< runMachine [input] machine
//     when (null outputs) $ loop (currentAr, 0, score)
//     -- print outputs
//     let (resList, updates) = partition (\[x, y, _] -> x == -1 && y == 0) $ chunksOf 3 outputs
//     let platformPos = case [x | [x, _, 3] <- updates] of [] -> 0; f : _ -> f
//         ballPos = case [x | [x, _, 4] <- updates] of [] -> 0; f : _ -> f
//         updatedScore = case resList of
//           [] -> score
//           res : _ -> let [_, _, newScore] = res in newScore
//     clearScreen
//     putStrLn $ "score = " <> show updatedScore
//     let newAr = updateGameAr updates currentAr
//     putStrLn $ unlines . charGridToStr $ newAr
//     loop (newAr, if ballPos > platformPos then 1 else -1, updatedScore)
pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = codeParser("input/13.txt");
    let sol1: usize = solution1(&code);
    let sol2: u64 = 0;

    (Solution::from(sol1), Solution::from(sol2))
}
