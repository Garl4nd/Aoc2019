use std::{env, fs::read_to_string, time::Instant};

use itertools::Itertools;
struct ParseError {
    message: String
}
fn parseFile(filename : &str) -> Result<Vec<i32>, ParseError>{
   if let Ok(fileReadRes) = read_to_string(filename){
    Ok(fileReadRes.lines().map(|ln| ln.parse().unwrap()).collect())
   }
   else
    {Err(ParseError{message : String::from("Fail to read file")})}
}
fn solution1(nums: &[i32]) -> i32 {
    nums.iter().map(|num| (num /3) -2 ).sum()
}
fn fuel(num: &i32) -> i32{
    let mut remMas = *num;
    let mut sum = 0 ;
    loop {
     remMas= remMas/3 - 2;
        if remMas <0 {break;}  
     sum += remMas 
    }
    sum 
}
fn fuel2(num: &i32) -> i32 {
    itertools::iterate(*num, |n| n /3 -2).dropping(1).take_while(|&n| n>0 ).sum()
}
fn solution2Imp(nums: &[i32]) -> i32 {
    nums.iter().map(fuel).sum()
}
fn solution2(nums: &[i32]) -> i32 {
    nums.iter().map(fuel2).sum()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{}",fuel(&100756));
    match parseFile(&args[1]) 
    { Ok(nums) => {
        println!("Solution 1 = {}", solution1(&nums));
        let start = Instant::now();
        println!("Solution 2 = {}", solution2(&nums));
        let duration = start.elapsed();
        println!("It took {:?}", duration);
        let start2 = Instant::now();
        println!("Solution 2 = {}", solution2Imp(&nums));
        let duration2 = start2.elapsed();
        println!("It took {:?}", duration2);
       }
        Err(err) => {
        println!("Failed! {}",err.message)
    }
    }
}
