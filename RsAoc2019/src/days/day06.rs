use crate::{Solution, SolutionPair};
use std::collections::HashMap;
use std::fs::read_to_string;
use std::hash::Hash;
///////////////////////////////////////////////////////////////////////////////
fn parse_file(file: &str) -> HashMap<String, Vec<String>> {
    let mut hash_map = HashMap::new();
    for line in file.lines() {
        let mut planets_it = line.split(')');
        let key = planets_it.next().unwrap();
        let val = planets_it.next().unwrap();
        let edges = hash_map.entry(String::from(key)).or_insert(Vec::new());
        edges.push(String::from(val));
    }
    hash_map
}
fn graph_distances(
    graph: &HashMap<String, Vec<String>>,
    initial_planet: &str,
) -> HashMap<String, u64> {
    struct SearchState<'a>(usize, &'a str, u64);
    let mut stack: Vec<SearchState> = vec![SearchState(0, initial_planet, 0)];
    let mut dist_map: HashMap<String, u64> = HashMap::new();
    while let Some(SearchState(mut pos, key, dist)) = stack.pop() {
        if pos == 0 {
            dist_map.insert(String::from(key), dist);
            pos += 1;
        }
        if let Some(edges) = (*graph).get(key) {
            if pos <= edges.len() {
                stack.push(SearchState(pos + 1, key, dist));
                stack.push(SearchState(0, &edges[pos - 1], dist + 1));
            }
        }
    }
    dist_map
}
fn reverse_graph(graph: &HashMap<String, Vec<String>>) -> HashMap<String, Vec<String>> {
    let mut reversed_graph = HashMap::new();
    for (key, vec) in graph.iter() {
        for val in vec.iter() {
            let rev_edges = reversed_graph
                .entry(String::from(val))
                .or_insert(Vec::new());
            rev_edges.push(String::from(key)).clone();
        }
    }
    reversed_graph
}
fn common_ancestor_distance(
    reversed_graph: &HashMap<String, Vec<String>>,
    el1: &str,
    el2: &str,
) -> u64 {
    let mut walker1 = String::from(el1);
    let mut walker2 = String::from(el2);
    let mut path1 = vec![walker1.clone()];
    let mut path2 = vec![walker2.clone()];
    while walker1 != walker2 {
        if let Some(prev_edges) = reversed_graph.get(&walker1) {
            for (idx, node) in path2.iter().enumerate() {
                if walker1 == *node {
                    println!(
                        "Found common ancestor! Paths = {:?} \nand\n{:?}",
                        path1,
                        &path2[0..=idx]
                    );
                    return (path1.len() - 1 + idx) as u64;
                }
            }
            walker1 = prev_edges[0].clone();
            path1.push(walker1.clone());
        }
        if let Some(prev_edges) = reversed_graph.get(&walker2) {
            for (idx, node) in path1.iter().enumerate() {
                if walker2 == *node {
                    println!(
                        "Found common ancestor! Paths = {:?} \nand\n{:?}",
                        &path1[0..=idx],
                        path2
                    );
                    return (path2.len() - 1 + idx) as u64;
                }
            }
            walker2 = prev_edges[0].clone();
            path2.push(walker2.clone())
        }
    }
    0
}
fn solution1(graph: &HashMap<String, Vec<String>>) -> u64 {
    let dists = graph_distances(graph, "COM");

    println!("{:?}", dists["COM"]);
    let total_dist: u64 = dists.values().sum();
    total_dist
}
fn solution2(graph: &HashMap<String, Vec<String>>) -> u64 {
    let reversed_graph = reverse_graph(graph); // graph, "COM");
    let source_planet = reversed_graph["YOU"][0].clone();
    let target_planet = reversed_graph["SAN"][0].clone();
    common_ancestor_distance(&reversed_graph, &source_planet, &target_planet)
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/6.txt").unwrap();
    let graph = parse_file(&file);
    let sol1: u64 = solution1(&graph);
    let sol2: u64 = solution2(&graph);
    (Solution::from(sol1), Solution::from(sol2))
}
