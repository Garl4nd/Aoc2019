use itertools::{Itertools, MinMaxResult};
use libm::atan2;

use crate::{Solution, SolutionPair};
use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
};

///////////////////////////////////////////////////////////////////////////////
fn parse_file(file: &str) -> Vec<Vec<char>> {
    file.lines().map(|line| line.chars().collect()).collect()
}
fn get_asteroids(char_grid: &[Vec<char>]) -> Vec<(i32, i32)> {
    char_grid
        .iter()
        .enumerate()
        .flat_map(|(y, row_vec)| {
            row_vec.iter().enumerate().filter_map(move |(x, val)| {
                if *val == '#' {
                    Some((y as i32, x as i32))
                } else {
                    None
                }
            })
        })
        .collect()
}
fn test_los((ys, xs): (i32, i32), (yt, xt): (i32, i32), char_grid: &[Vec<char>]) -> bool {
    true
}
type AstPos = (i32, i32);
fn gcd(a: i32, b: i32) -> i32 {
    if a == 0 {
        b
    } else if b == 0 {
        a
    } else {
        let (mut a, mut b) = if a < b { (a, b) } else { (b, a) };
        while b != 0 {
            let r = a % b;
            a = b;
            b = r;
        }
        a
    }
}
fn get_line((y1, x1): &AstPos, (y2, x2): &AstPos) -> (i32, i32) {
    let dy = y2 - y1;
    let dx = x2 - x1;
    let d = gcd(dy.abs(), dx.abs());
    print!("{},{},{}", dy, dx, d);
    (dy / d, dx / d)
}
fn calc_los_counts(asteroids: &[AstPos]) -> HashMap<AstPos, HashSet<AstPos>> {
    let mut los_counter: HashMap<AstPos, HashSet<AstPos>> = HashMap::new();
    for source_ast in asteroids.iter() {
        let mut visited_lines: HashSet<AstPos> = HashSet::new();
        for target_ast in asteroids.iter().filter(|el| *el != source_ast) {
            let line: AstPos = get_line(source_ast, target_ast);
            if !visited_lines.contains(&line) {
                visited_lines.insert(line);
            }
        }
        los_counter.insert(*source_ast, visited_lines); //.insert(*source_ast, visited_lines.len());
    }
    los_counter
}
fn solution1(char_grid: &[Vec<char>]) -> u64 {
    let asteroids = get_asteroids(char_grid);
    calc_los_counts(&asteroids)
        .values()
        .map(|s| s.len() as u64)
        .max()
        .unwrap()
}
fn solution2(char_grid: &[Vec<char>]) -> u64 {
    let asteroids = get_asteroids(char_grid);
    let ((y, x), lines) = calc_los_counts(&asteroids)
        .into_iter()
        .max_by_key(|(_, v)| v.len())
        .unwrap();
    let clockwise_lines: Vec<&(i32, i32)> = lines
        .iter()
        .sorted_by(|(a, b), (a2, b2)| {
            f64::total_cmp(
                &atan2((*b2) as f64, (*a2) as f64),
                &atan2((*b) as f64, (*a) as f64),
            )
        })
        .collect();

    let (dy200, dx200) = clockwise_lines[199];
    for k in 1..char_grid.len() as i32 {
        let test_point = (y + k * (*dy200), x + k * (*dx200));
        if asteroids.contains(&test_point) {
            let (y_t, x_t) = test_point;
            return (100 * x_t + y_t) as u64;
        };
    }
    0
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/10.txt").unwrap();
    let char_grid = parse_file(&file);
    let sol1: u64 = solution1(&char_grid);
    let sol2: u64 = solution2(&char_grid);

    (Solution::from(sol1), Solution::from(sol2))
}
