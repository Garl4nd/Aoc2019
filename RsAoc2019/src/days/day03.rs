use itertools::iproduct;

use crate::{Solution, SolutionPair};
use core::fmt;
use std::{
    fmt::Error,
    fs::{self, read_to_string},
    iter::zip,
    str::FromStr,
    string::ParseError,
};
///////////////////////////////////////////////////////////////////////////////
type GridPos = (i32, i32);
type PathSegment = (GridPos, GridPos);
#[derive(Debug)]
enum Dir {
    L,
    R,
    U,
    D,
}
#[derive(Debug)]
struct Instruction(Dir, i32);
#[derive(Debug)]
enum InstParseError {
    EmptyString,
    NoDirection,
    ParseIntError,
}

impl fmt::Display for InstParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let description = match *self {
            InstParseError::NoDirection => "Couldn't parse direction!",
            InstParseError::EmptyString => "Empty string!",
            InstParseError::ParseIntError => "Couldn't parse number!",
        };
        f.write_str(description)
    }
}
impl FromStr for Instruction {
    type Err = InstParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();
        let dir_char = chars.next().ok_or(InstParseError::EmptyString)?;
        // println!("{dir_char}");
        let dir = match dir_char {
            'L' => Ok(Dir::L),
            'R' => Ok(Dir::R),
            'U' => Ok(Dir::U),
            'D' => Ok(Dir::D),
            _ => Err(InstParseError::NoDirection),
        }?;
        let steps: i32 = chars
            .collect::<String>()
            .parse()
            .map_err(|_| InstParseError::ParseIntError)?;
        Ok(Instruction(dir, steps))
    }
}

type Wire = Vec<Instruction>;
fn parse_file(file: &str) -> (Wire, Wire) {
    let mut wire_strs = file.lines().map(get_wires);
    let wire1 = wire_strs.next().unwrap();
    let wire2 = wire_strs.next().unwrap();
    fn get_wires(wire_str: &str) -> Wire {
        wire_str
            .split(',')
            .map(|inst| inst.parse().unwrap())
            .collect()
    }
    (wire1, wire2)
}
fn move_dir((y, x): &GridPos, Instruction(dir, steps): &Instruction) -> GridPos {
    match dir {
        Dir::L => (*y, *x - steps),
        Dir::R => (*y, *x + steps),
        Dir::U => (*y - steps, *x),
        Dir::D => (*y + steps, *x),
    }
}
fn path_segments_w_dists(wire: &Wire) -> Vec<(PathSegment, i32)> {
    let ps = wire.iter().scan((0, 0), |pos, inst| {
        let init_pos = *pos;
        *pos = move_dir(pos, inst);
        Some((init_pos, *pos))
    });
    let dists = wire.iter().scan(0, |dist, Instruction(_, steps)| {
        let init_dist = *dist;
        *dist += steps;
        Some(init_dist)
    });
    zip(ps, dists).collect()
}
fn between(n: i32, a: i32, b: i32) -> bool {
    std::cmp::min(a, b) <= n && n <= std::cmp::max(a, b)
}
fn manhattan((y1, x1): GridPos, (y2, x2): GridPos) -> i32 {
    (y1 - y2).abs() + (x2 - x1).abs()
}
fn cross_points(ps1: &PathSegment, ps2: &PathSegment) -> Option<GridPos> {
    let ((y1s, x1s), (y1e, x1e)) = *ps1;
    let ((y2s, x2s), (y2e, x2e)) = *ps2;
    if y2s == y2e && between(y2s, y1s, y1e) && between(x1s, x2s, x2e) {
        Some((y2s, x1s))
    } else if between(x2s, x1s, x1e) && between(y1s, y2s, y2e) {
        Some((y1s, x2s))
    } else {
        None
    }
}
fn getCrossingDists((ps1, d1): &(PathSegment, i32), (ps2, d2): &(PathSegment, i32)) -> Option<i32> {
    let crossing = cross_points(ps1, ps2)?;
    if crossing == (0, 0) {
        None
    } else {
        let (p1, _) = ps1;
        let (p2, _) = ps2;
        Some(*d1 + *d2 + manhattan(*p1, crossing) + manhattan(*p2, crossing))
    }
}
fn solution1((wire1, wire2): &(Wire, Wire)) -> i32 {
    let ps1s = path_segments_w_dists(wire1);
    let ps2s = path_segments_w_dists(wire2);
    iproduct!(ps1s, ps2s)
        .filter_map(|(ps1, ps2)| cross_points(&ps1.0, &ps2.0))
        .map(|pos| manhattan(pos, (0, 0)))
        .filter(|d| *d != 0)
        .min()
        .unwrap()
}

fn solution2((wire1, wire2): &(Wire, Wire)) -> i32 {
    let ps1s = path_segments_w_dists(wire1);
    let ps2s = path_segments_w_dists(wire2);
    iproduct!(ps1s, ps2s)
        .filter_map(|(ps1, ps2)| getCrossingDists(&ps1, &ps2))
        .min()
        .unwrap()
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/3.txt").unwrap();
    let wires = parse_file(&file);
    let sol1: u64 = solution1(&wires) as u64;
    let sol2: u64 = solution2(&wires) as u64;

    (Solution::from(sol1), Solution::from(sol2))
}
