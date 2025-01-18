use crate::{Solution, SolutionPair};
use std::{collections::HashMap, fs::read_to_string};

///////////////////////////////////////////////////////////////////////////////
type Resource<'a> = (&'a str, i64);
#[derive(Debug)]
struct Reaction<'a> {
    prod_count: i64,
    reqs: Vec<Resource<'a>>,
}
fn parse_resource(string: &str) -> Resource {
    let mut str_it = string.trim().split(' ');
    let amount = str_it.next().unwrap().trim().parse().unwrap();
    let name = str_it.next().unwrap().trim();
    (name, amount)
}
fn parse_file(file: &str) -> HashMap<&str, Reaction> {
    let reaction_it = file.lines().map(|line| {
        let mut parts = line.split("=>");
        (parts.next().unwrap(), parts.next().unwrap())
    });
    reaction_it
        .map(|(resources, product)| {
            let (name, prod_count) = parse_resource(product);
            (
                name,
                Reaction {
                    prod_count,
                    reqs: resources.split(',').map(parse_resource).collect(),
                },
            )
        })
        .collect()
}

fn binary_search(f : impl Fn(i32) -> bool, x0: i32, x1 : i32) -> i32 {
    if x0 +1 == x1 || f(x0)
        {x0}
    else {
        let m = (x0 + x1) /2 ; 
        if f(m) {
            binary_search(f, x0, m)
        }
        else 
    {
            binary_search(f, m, x1)}
    }
}
fn consume_supplies_and_find_required_amount(
    target: &str, 
    requested_amount: i64,
    available_resources: &mut HashMap<&str, i64>
) -> i64
{
    let available_amount = available_resources[target];
    let consumed_amount = std::cmp::min(available_amount, requested_amount); 
    *available_resources.get_mut(target).unwrap() -= consumed_amount ; 
    requested_amount - consumed_amount 
}
fn minimum_req<'a>(
    reaction_map: &HashMap<&str, Reaction<'a>>,
    target: &str,
    requested_amount: i64,
    available_resources: &mut HashMap<&'a str, i64>,
    produced_resoucers: &mut HashMap<& str, i64>,
) {
    if target == "ORE" {
        available_resources
            .entry("ORE")
            .and_modify(|o| *o += requested_amount)
            .or_insert(0);
        produced_resoucers
            .entry("ORE")
            .and_modify(|o| *o += requested_amount)
            .or_insert(0);
    } else {
        let Reaction { prod_count, reqs } = &reaction_map[target];
        let new_product = consume_supplies_and_find_required_amount(target, requested_amount, available_resources);
        let reps = (new_product +1) / prod_count;
       for (reactant, reactant_amount) in reqs {
            let new_reactant = consume_supplies_and_find_required_amount(reactant, reps*reactant_amount, available_resources);
           minimum_req(
                reaction_map,
                reactant,
                new_reactant,
                available_resources,
                produced_resoucers,
            );            
            *available_resources.get_mut(reactant).unwrap() -= new_reactant ; 
              }
        *available_resources.get_mut(target).unwrap() += new_product;
        *produced_resoucers.get_mut(target).unwrap() +=  new_product ;
    };
}
fn required_ore(reaction_map: &HashMap<&str, Reaction>, fuel: i64) -> i64{
    let mut available_resoures: HashMap<&str, i64> = reaction_map.keys().map(|k| (*k, 0)).collect();
    available_resoures.insert("ORE", 0);
    // dbg!(&reacts);
    let mut produced_resources = available_resoures.clone();
     minimum_req(
        reaction_map,
        "FUEL",
        fuel,
        &mut available_resoures,
        &mut produced_resources,
    );
    produced_resources["ORE"]
}
fn solution1(reaction_map: &HashMap<&str, Reaction>) -> u64 
{
    required_ore(reaction_map, 1) as u64 
}
fn solution2(reaction_map: &HashMap<&str, Reaction>) -> u64 
{

    binary_search(|fuel| required_ore(reaction_map, fuel as i64) > 1_000_000_000_000, 10_000, 10_000_000) as u64
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/14.txt").unwrap();
    let reaction_map  = parse_file(&file);
    let sol1: u64 = solution1(&reaction_map);
    let sol2: u64 = solution2(&reaction_map);
    (Solution::from(sol1), Solution::from(sol2))
}
