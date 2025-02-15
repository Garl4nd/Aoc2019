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

struct IntMachine {
    code: [usize; 1000],
    ptr: usize,
    base: usize,
    outputs: Vec<usize>,
    state: MachineState,
}

fn create_machine(code: &[usize]) -> IntMachine {
    let mut machine_code = [0; 1000];
    machine_code[..code.len()].copy_from_slice(code);

    IntMachine {
        code: machine_code,
        ptr: 0,
        base: 0,
        outputs: Vec::new(),
        state: MachineState::Running,
    }
}
fn run_machine(machine: &mut IntMachine, inputs: &[usize]) -> Vec<usize> {
    while machine.state == MachineState::Running {
        step_machine(machine, inputs)
    }
    Vec::new()
}
