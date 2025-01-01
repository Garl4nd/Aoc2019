use std::{env, fs::read_to_string, usize};

fn runCode(code: &Vec<i32>, noun: usize, verb: usize) -> Vec<usize>
{
    let mut newCode : Vec<usize> = code.iter().map(|&n| n as usize).collect() ;
    newCode[1] = noun; newCode[2] = verb;
    for idx in (0..code.len()).step_by(4)
{ 
        
     match newCode[idx]{
        1 => {let (inputPos1, inputPos2, outputPos) = (newCode[idx+1], newCode[idx+2], newCode[idx+3]);
                newCode[outputPos] = newCode[inputPos1] + newCode[inputPos2];} // newCode[idx+1] + newCode[idx+2];}
        2 => {let (inputPos1, inputPos2, outputPos) = (newCode[idx+1], newCode[idx+2], newCode[idx+3]);
                newCode[outputPos] = newCode[inputPos1] * newCode[inputPos2];} // newCode[idx+1] + newCode[idx+2];}
        99 => {break;}
        _ => {continue;}
        } 
    }    
    newCode
}
fn solution1(code: &Vec<i32>) -> usize 
{
    runCode(code, 12, 2)[0]
}
fn solution2(code: &Vec<i32>) -> usize
{
    let target : usize = 19690720;
    for noun in (0..=99){
        for verb in (0..=99)
    {
            if runCode(code, noun, verb)[0] == target {return (100*noun+verb) as usize}
            }
        }
     0    
}
    

fn parseFile(filename: &str) -> Vec<i32>
{
    let file : String = read_to_string(filename).unwrap().lines().take(1).collect();
    println!("vals = {:?}", file.split(',').collect::<Vec<_>>());
    file.split(',').map(|s| s.parse().unwrap()).collect()
    
}
fn main() {

    let args: Vec<String> = env::args().collect();
    let vec = parseFile(&args[1]);
        
    println!{"Original code is{:?}", vec}
    println!{"Transformed code is{:?}", runCode(&vec, 12, 2)}

    println!{"Solution 1 = {}", solution1(&vec)}
    println!{"Solution 2 = {}", solution2(&vec)}
}
