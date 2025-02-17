use std::collections::VecDeque;

// data Machine a r = Machine
//   { mCode :: a Int Int
//   , ptrRef :: r Int
//   , baseRef :: r Int
//   , inputsRef :: r [Int]
//   , outputsRef :: r [Int]
//   , mState :: r MachineState
//   }
// data MachineResult = MachineResult
//   { finalCode :: Code
//   , machineOutputs :: [Int]
//   , machineState :: MachineState
//   }
//   deriving (Show)
#[derive(PartialEq, Eq)]
enum MachineState {
    Running,
    Halted,
    WaitingForInput,
}
#[derive(Debug, Clone)]
pub enum InputMode {
    Position,
    Immediate,
    Relative,
}
pub struct IntMachine {
    code: [usize; 1000],
    ptr: usize,
    base: usize,
    state: MachineState,
}

const TERNARY_OPS : [usize;] = [0];
impl IntMachine {
    pub fn new(code: &[usize]) -> Self {
        let mut machine_code = [0; 1000];
        machine_code[..code.len()].copy_from_slice(code);

        IntMachine {
            code: machine_code,
            ptr: 0,
            base: 0,
            state: MachineState::Running,
        }
    }

    pub fn run_machine(&mut self, inputs: &[usize]) -> Vec<usize> {
        let mut input_queue = VecDeque::<usize>::from(inputs.to_owned());
        let mut outputs = Vec::new();
        while self.state == MachineState::Running {
            if let Some(output) = self.step_machine(&mut input_queue) {
                outputs.push(output);
            }
        }
        outputs
    }

    fn step_machine(&mut self, input_queue: &mut VecDeque<usize>) -> Option<usize> {
        let (modes, op_code) = parse_modes_and_opcode(self.code[self.ptr]);
        match op_code{
            _ if op_code 
            
        }
    }
}

pub fn parse_modes_and_opcode(value: usize) -> ((InputMode, InputMode, InputMode), usize) {
    let mode_digits = value / 100;
    let op_code = value % 100;
    let mut modes = Vec::new();
    let mut n = mode_digits;
    for _ in 0..3 {
        modes.push(num_to_mode(n % 10));
        n /= 10;
    }
    (
        (modes[0].clone(), modes[1].clone(), modes[2].clone()),
        op_code,
    )
}
fn num_to_mode(num: usize) -> InputMode {
    match num {
        0 => InputMode::Position,
        1 => InputMode::Immediate,
        2 => InputMode::Relative,
        _ => panic!("Wrong mode!"),
    }
}
