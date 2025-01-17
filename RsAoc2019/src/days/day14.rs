use crate::{Solution, SolutionPair};
use std::{collections::HashMap, fs::read_to_string}; 

///////////////////////////////////////////////////////////////////////////////
type Resource = (String, i32); 
#[derive(Debug)]
struct Reaction
    {
        amount: i32, 
        reqs: Vec<Resource>
    }
fn parse_resource(string : &str) -> Resource 
{
    let mut strIt = string.trim().split(' ');
    let amount = strIt.next().unwrap().trim().parse().unwrap();
    let name = strIt.next().unwrap().trim().to_string();
    (name, amount)
}
fn parse_file(file : &str) -> HashMap<String, Reaction>
{
    let reaction_it = file.lines().map(|line| {let mut parts = line.split("=>"); (parts.next().unwrap(), parts.next().unwrap())});
    reaction_it.map(|(resources, product)| {let (name,  amount) = parse_resource(product); (name.clone(),
        Reaction{amount, reqs: resources.split(',').map(parse_resource).collect()})}).collect() 
}
fn required_ore (reaction_map : &HashMap<String, Reaction>, target: &str) -> i32 
{
    if let Some(Reaction{amount, reqs}) = reaction_map.get(target)
    {
        1
    }
    else {
    0}
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/14.txt").unwrap();
    let reacts = parse_file(&file);
    dbg!(reacts);
    let sol1: u64 = 0;
    let sol2: u64 = 0;

    (Solution::from(sol1), Solution::from(sol2))
}
