use core::time;
use std::thread::sleep;

use itertools::Itertools;

use crate::{
    etc::intcode::MachineState,
    intcode::{self, IntMachine},
    Solution, SolutionPair,
};

///////////////////////////////////////////////////////////////////////////////

// amplifierLoop :: forall a m r. (MArray a Int m, MonadRef r m) => [Int] -> [Int] -> m Int
// amplifierLoop code settings = do
//   machines :: [Machine a r] <- sequence [runMachine [s] =<< createMachine code | s <- settings]
//   let singleRun inp0 = foldM (\input machine -> let outputs = runMachine [input] machine >>= getOutputs in last <$> outputs) inp0 machines
//   flip fix 0 $ \loop inp -> do
//     output <- singleRun inp
//     state <- readRef . mState . last $ machines
//     if state == Halted
//       then return output
//       else loop output
//
// solution1 :: [Int] -> Int
// solution1 code = maxAmplifierOutput code [0 .. 4]
//
// solution2 :: [Int] -> Int
// solution2 code = maxAmplifierOutput code [5 .. 9]
//
// maxAmplifierOutput :: [Int] -> [Int] -> Int
// maxAmplifierOutput code settingRange = maximum $ runST resultsST
//  where
//   resultsST = mapM (amplifierLoopST code) $ permutations settingRange
//   amplifierLoopST :: forall s. [Int] -> [Int] -> ST s Int
//   amplifierLoopST = amplifierLoop @(STUArray s)
fn serial_run(machines: &mut [IntMachine], input: i64) -> i64 {
    let mut power = input;
    for machine in machines {
        let outputs = machine.run_machine(&[power]);
        if let Some(output) = outputs.last() {
            power = *output;
        }
    }
    power
}
fn amplifier_loop(code: &[i64], settings: &[i64]) -> i64 {
    let mut machines = settings
        .iter()
        .map(|s| {
            let mut machine = intcode::IntMachine::new(code);
            let output = machine.run_machine(&[*s]);
            machine
        })
        .collect_vec();
    let mut power = 0;
    while machines.last().unwrap().state != MachineState::Halted {
        power = serial_run(&mut machines, power)
    }
    power
}
fn max_power(code: &[i64], setting_range: &[i64]) -> i64 {
    let setting_perms = setting_range.iter().permutations(setting_range.len());
    setting_perms
        .map(|settings| {
            let s = settings.iter().cloned().cloned().collect_vec();
            amplifier_loop(code, &s)
        })
        .max()
        .unwrap()
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = intcode::codeParser("input/7.txt");

    let sol1: u64 = max_power(&code, &[0, 1, 2, 3, 4]) as u64;
    let sol2: u64 = max_power(&code, &[5, 6, 7, 8, 9]) as u64;

    (Solution::from(sol1), Solution::from(sol2))
}
