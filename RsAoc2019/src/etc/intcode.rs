use std::{collections::VecDeque, fs::read_to_string} ;

pub fn codeParser(filename: &str) -> Vec<usize> {
    let file: String = read_to_string(filename).unwrap().lines().take(1).collect();
    println!("vals = {:?}", file.split(',').collect::<Vec<_>>());
    file.split(',').map(|s| s.parse().unwrap()).collect()
}

#[derive(PartialEq, Eq)]
enum MachineState {
    Running,
    Halted,
    WaitingForInput,
}
#[derive(Debug, Clone)]
pub enum PosType{
    Relative, 
    Absolute
}
#[derive(Debug, Clone)]
pub enum InputMode {
    Immediate,
    Position(PosType)
}
enum UnaryInst {
    Input, 
    Output, 
    MoveBase 
}
enum BinaryInst {
    JumpIfTrue, 
    JumpIfFalse
}
enum TernaryInst{
    Add, 
    Mul, 
    Less, 
    Equals
}
enum Inst{
    Unary(UnaryInst), Binary (BinaryInst), Ternary (TernaryInst), Halt}
    
pub struct IntMachine {
    pub code: [usize; 1000],
    ptr: usize,
    base: usize,
    pub state: MachineState,
}

fn opcode_to_inst(opcode : usize) -> Inst 
{
    match opcode{
        1 => {Inst::Ternary (TernaryInst::Add)},
        2 => {Inst::Ternary (TernaryInst::Mul)},
        3 => {Inst::Unary (UnaryInst::Input)},
        4 => {Inst::Unary (UnaryInst::Output)}, 
        5 => {Inst::Binary (BinaryInst::JumpIfTrue)}, 
        6 => {Inst::Binary (BinaryInst::JumpIfFalse)}, 
        7 => {Inst::Ternary (TernaryInst::Less)}, 
        8 => {Inst::Ternary (TernaryInst::Equals)}, 
        9 => {Inst::Unary (UnaryInst::MoveBase)}, 
        99 => {Inst::Halt},
        _ => panic!("Wrong opcode!")
    }
}
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
// Ternary op -> do
//              inputVal1 <- valGetter mode1 =<< readArray mCode (ptr + 1)
//              inputVal2 <- valGetter mode2 =<< readArray mCode (ptr + 2)
//              outputPos <- positionGetter mode3 =<< readArray mCode (ptr + 3)
//              writeArray mCode outputPos (funcForTernary op inputVal1 inputVal2)
//              modifyRef ptrRef (+ 4)
// let positionGetter mode pos = case mode of
//         PositionMode -> return pos
//         RelativeMode -> (+ pos) <$> readRef baseRef
//         _ -> error "This is not allowed"
//   let valGetter mode pos = case mode of
//         ImmediateMode -> return pos
//         _ -> positionGetter mode pos >>= readArray mCode/         B
fn position_getter(&self, position: usize, pos_type: PosType) -> usize 
    {
        match pos_type{
            PosType::Absolute => {position},
            PosType::Relative => {self.base + position}, 
        }
    }
fn val_getter (&self, value: usize, mode: InputMode) -> usize
    {
        match mode{
            InputMode::Immediate => {value}, 
            InputMode::Position(pos_type) => {self.code[self.position_getter(value, pos_type)]}
        }
    }
//              inputVal1 <- valGetter mode1 =<< readArray mCode (ptr + 1)
//              inputVal2 <- valGetter mode2 =<< readArray mCode (ptr + 2)
//              outputPos <- positionGetter mode3 =<< readArray mCode (ptr + 3)
//              writeArray mCode outputPos (funcForTernary op inputVal1 inputVal2)
//              modifyRef ptrRef (+ 4)

    fn step_machine(&mut self, input_queue: &mut VecDeque<usize>) -> Option<usize> {
        let ((mode1, mode2, mode3), op_code) = parse_modes_and_opcode(self.code[self.ptr]);
        let mut output = None;
        match opcode_to_inst(op_code) {
            Inst::Ternary(ternInst) => {
             let inputVal1 = self.val_getter(self.code[self.ptr + 1], mode1);
             let inputVal2 = self.val_getter(self.code[self.ptr + 2], mode2);
             if let InputMode::Position(pos_type) = mode3 
                { 
                let outputPos = self.position_getter(self.code[self.ptr + 3], pos_type);
                self.code[outputPos] = match (ternInst){
                        TernaryInst::Add => {inputVal1 + inputVal2},
                        TernaryInst::Mul => {inputVal1 * inputVal2},
                        TernaryInst::Less => {if inputVal1 < inputVal2 {1} else {0}},
                        TernaryInst::Equals => {if inputVal1 == inputVal2 {1} else {0}}
                    };
                self.ptr += 4;
                }
            else {
                    panic!("Immediate mode found for output pos ");
                }
            }
            Inst::Binary(binInst) => {
            let inputVal1 = self.val_getter(self.code[self.ptr + 1], mode1);
            let inputVal2 = self.val_getter(self.code[self.ptr + 2], mode2);
            let new_pos = match (binInst)
                {
                    BinaryInst::JumpIfTrue => {if inputVal1 >0 {inputVal2} else {self.ptr +3}}
                    BinaryInst::JumpIfFalse => {if inputVal1 ==0 {inputVal2} else  {self.ptr+3}}
                };
            self.ptr = new_pos;
 //        Unary Input ->  
 //                let PositionMode posType = mode1
 //                targetPos <- positionGetter posType =<< readArray mCode (ptr + 1)
 //                inputLs <- readRef inputsRef
 //                case inputLs of
 //                  [] -> writeRef mState WaitingForInput
 //                  (currentInput : remainingInput) -> do
 //                    writeArray mCode targetPos currentInput
 //                    writeRef inputsRef remainingInput
 //                    modifyRef ptrRef (+ 2)
 //        Unary op ->  do
 //                targetVal <- valGetter mode1 =<< readArray mCode (ptr + 1)
 //                case op of
 //                  Output ->  modifyRef outputsRef (++ [targetVal])
 //                  MoveBase -> modifyRef baseRef (+ targetVal)
 //                modifyRef ptrRef (+ 2)
 // 
            },
            Inst::Unary(UnaryInst::Input) => {
                if let InputMode::Position(pos_type) = mode1 
                {
                    let target_pos = self.position_getter(self.code[self.ptr+1], pos_type);
                    if let Some(current_input) = input_queue.pop_front()
                    {
                        self.code[target_pos] = current_input;
                        self.ptr+=2;
                    }
                    else 
                    {
                        self.state = MachineState::WaitingForInput;
                    }

                }
                else {
                    panic!("Immediate mode for input!")
                }
            },
            Inst::Unary(una_inst) => {
                let target_val = self.val_getter(self.code[self.ptr +1], mode1);
                match una_inst{
                UnaryInst::Output => {output = Some(target_val)}
                UnaryInst::MoveBase => {self.base += target_val}
                _ => {unreachable!()}
                };
                self.ptr += 2;

        } 
                
            Inst::Halt => {self.state = MachineState::Halted}
        
        };
        output
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
        0 => InputMode::Position(PosType::Absolute),
        1 => InputMode::Immediate,
        2 => InputMode::Position(PosType::Relative),
        _ => panic!("Wrong mode!"),
    }
}
pub fn run_code(code: &[usize], input: &[usize]) -> IntMachine
{
        let mut machine = IntMachine::new(code);
        machine.run_machine(input);
        machine
    }

