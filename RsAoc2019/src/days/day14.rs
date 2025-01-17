use crate::{Solution, SolutionPair};
use std::{collections::HashMap, fs::read_to_string};

///////////////////////////////////////////////////////////////////////////////
type Resource<'a> = (&'a str, i32);
#[derive(Debug)]
struct Reaction<'a> {
    prod_count: i32,
    reqs: Vec<Resource<'a>>,
}
fn parse_resource(string: &str) -> Resource {
    let mut strIt = string.trim().split(' ');
    let amount = strIt.next().unwrap().trim().parse().unwrap();
    let name = strIt.next().unwrap().trim();
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
                name.clone(),
                Reaction {
                    prod_count,
                    reqs: resources.split(',').map(parse_resource).collect(),
                },
            )
        })
        .collect()
}
fn required_ore(reaction_map: &HashMap<String, Reaction>, target: &str) -> i32 {
    if let Some(Reaction { prod_count, reqs }) = reaction_map.get(target) {
        1
    } else {
        0
    }
}
fn reaction_reps_and_leftover(
    target: &str,
    requested_amount: i32,
    prod_count: i32,
    available_resources: &HashMap<&str, i32>,
) -> (i32, i32) {
    let required_amount = requested_amount - available_resources.get(target).unwrap();
    if required_amount <= 0 {
        (0, -required_amount)
    } else {
        let (div, rem) = (required_amount / prod_count, required_amount % prod_count);
        if rem == 0 {
            // probably could be simplified with negative division and rem. E.g., 15, 8 ->
            // -15 `quotrem` 8 = (-2, 1) -> (2,1)
            //
            (div, 0)
        } else {
            (div + 1, prod_count - rem)
        }
    }
}

fn minimum_req<'a>(
    reaction_map: &HashMap<&str, Reaction<'a>>,
    target: &str,
    requested_amount: i32,
    available_resources: &mut HashMap<&'a str, i32>,
    produced_resoucers: &mut HashMap<&'a str, i32>,
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
        let Reaction { prod_count, reqs } = reaction_map.get(target).unwrap();
        let (reps, leftover) =
            reaction_reps_and_leftover(target, requested_amount, *prod_count, available_resources);
        for (reactant, reactant_amount) in reqs {
            dbg!(&reactant);
            let available_amount = available_resources.get_mut(reactant).unwrap();
            let required_amount = std::cmp::max(0, reactant_amount * reps - *available_amount);
            *available_amount -= required_amount;
            produced_resoucers
                .entry(reactant)
                .and_modify(|a| {
                    *a += required_amount;
                })
                .or_insert(0);
            minimum_req(
                reaction_map,
                reactant,
                required_amount,
                available_resources,
                produced_resoucers,
            );
            dbg!(&produced_resoucers);
        }
        *available_resources.get_mut(target).unwrap() += leftover;
        *produced_resoucers.get_mut(target).unwrap() += requested_amount + leftover;
    };
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/14.txt").unwrap();
    let reacts = parse_file(&file);
    let mut available_resoures: HashMap<&str, i32> = reacts.keys().map(|k| (*k, 0)).collect();
    available_resoures.insert("ORE", 0);
    dbg!(&reacts);
    let mut produced_resources = available_resoures.clone();

    minimum_req(
        &reacts,
        "FUEL",
        1,
        &mut available_resoures,
        &mut produced_resources,
    );
    dbg!(reacts);
    dbg!(&produced_resources);
    dbg!(produced_resources.get("ORE").unwrap());
    let sol1: u64 = 0;
    let sol2: u64 = 0;

    (Solution::from(sol1), Solution::from(sol2))
}
