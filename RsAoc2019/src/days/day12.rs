use itertools::{process_results, Itertools};

use crate::{Solution, SolutionPair};
use std::{convert::identity, fs::read_to_string};

///////////////////////////////////////////////////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq)]
struct Vec3D {
    x: i32,
    y: i32,
    z: i32,
}
impl Vec3D {
    fn new() -> Self {
        Self { x: 0, y: 0, z: 0 }
    }
    fn add(&self, other: &Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
    fn neg(&self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
            z: -self.z,
        }
    }
    fn sub(&self, other: &Self) -> Self {
        self.add(&other.neg())
    }
    fn transform(&self, mapping: impl Fn(i32) -> i32) -> Self {
        Self {
            x: mapping(self.x),
            y: mapping(self.y),
            z: mapping(self.z),
        }
    }
    fn compwise_binop(&self, other: &Self, mapping: impl Fn(i32, i32) -> i32) -> Self {
        Self {
            x: mapping(self.x, other.x),
            y: mapping(self.y, other.y),
            z: mapping(self.z, other.z),
        }
    }
    fn energy(&self) -> u64 {
        self.x.abs() as u64 + self.y.abs() as u64 + self.z.abs() as u64
    }
}
type Moon = (Vec3D, Vec3D);
macro_rules! fTwice {
    ($fn:ident, &mut $arg:ident) => {
        $fn(&mut $arg);
        $fn(&mut $arg);
    };
}
fn floyd_mut<T: Clone, S: Eq + std::fmt::Debug>(
    f: impl Fn(&mut T),
    init_val: &T,
    eq_proxy: impl Fn(&T) -> S,
) -> (i32, i32) {
    let mut hare = init_val.clone();
    let mut tortoise = init_val.clone();
    f(&mut tortoise);
    fTwice!(f, &mut hare);
    while eq_proxy(&hare) != eq_proxy(&tortoise) {
        f(&mut tortoise);
        fTwice!(f, &mut hare);
    }
    dbg!(eq_proxy(&tortoise), eq_proxy(&hare));
    tortoise = init_val.clone();
    let mut mu = 0;
    while eq_proxy(&tortoise) != eq_proxy(&hare) {
        f(&mut tortoise);
        f(&mut hare);
        mu += 1;
    }
    dbg!(eq_proxy(&tortoise), eq_proxy(&hare));
    let mut lam = 1;
    f(&mut tortoise);
    while eq_proxy(&tortoise) != eq_proxy(&hare) {
        f(&mut tortoise);
        lam += 1;
    }
    dbg!(eq_proxy(&tortoise), eq_proxy(&hare));
    f(&mut tortoise);
    f(&mut hare);
    dbg!(eq_proxy(&tortoise), eq_proxy(&hare));
    (mu, lam)
}

fn parse_position(posStr: &str) -> Moon {
    let coords: Vec<i32> = posStr
        .split(',')
        .map(|s| s.split('=').dropping(1).next().unwrap().parse().unwrap())
        .collect();
    (
        Vec3D {
            x: coords[0],
            y: coords[1],
            z: coords[2],
        },
        Vec3D::new(),
    )
}
fn parse_file(file: &str) -> Vec<Moon> {
    file.lines()
        .map(|line| parse_position(line.chars().dropping_back(1).dropping(1).as_str()))
        .collect()
}
fn modify_velocities((pos1, ref mut vel1): &mut Moon, (pos2, ref mut vel2): &mut Moon) {
    let dif_vec = Vec3D::compwise_binop(pos1, pos2, |c1, c2| match c2 - c1 {
        0 => 0,
        dif if dif > 0 => 1,
        _ => -1,
    });
    *vel1 = vel1.add(&dif_vec);
    *vel2 = vel2.sub(&dif_vec);
}
fn modify_position((ref mut pos, vel): &mut Moon) {
    *pos = pos.add(vel);
}
fn move_moons(moons: &mut Vec<Moon>) {
    for i in 0..moons.len() {
        for j in i + 1..moons.len() {
            let (moons1, moons2) = moons.split_at_mut(i + 1);
            modify_velocities(&mut (moons1)[i], &mut (moons2)[j - i - 1]);
        }
    }
    for moon in moons {
        modify_position(moon);
    }
}
fn total_energy(moons: &Vec<Moon>) -> u64 {
    moons
        .iter()
        .map(|(pos, vel)| (*pos).energy() * (*vel).energy())
        .sum()
}
fn kinetic_energy(moons: &Vec<Moon>) -> u64 {
    moons.iter().map(|(pos, vel)| (*vel).energy()).sum()
}

fn tot_kin_energy(moons: &Vec<Moon>) -> (u64, u64, i32) {
    (total_energy(moons), kinetic_energy(moons), moons[0].0.x)
}
fn simulate_moons(moons: &mut Vec<Moon>) {
    for t in 0..1001 {
        dbg!(t, total_energy(&moons), &moons);
        move_moons(moons);
    }
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/12.txt").unwrap();
    let moons = parse_file(&file);
    let mut moon_copy = moons.clone();
    println!("{:?}", floyd_mut(move_moons, &moons, tot_kin_energy));
    //println!("{:?}", moons);
    //simulate_moons(&mut moonCopy);
    let sol1: u64 = 0;
    let sol2: u64 = 0;

    (Solution::from(sol1), Solution::from(sol2))
}
