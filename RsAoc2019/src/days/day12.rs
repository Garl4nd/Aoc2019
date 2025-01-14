use itertools::Itertools;

use crate::{Solution, SolutionPair};
use std::{fs::read_to_string};

///////////////////////////////////////////////////////////////////////////////
#[derive(Debug)]
struct Vec3D {
    x : i32,
    y : i32, 
    z : i32
}
impl Vec3D {
    fn new() -> Self 
    {
        Self{x:0, y:0, z:0}
    }
    fn add(&self, other: &Self) -> Self 
    {
        Self{x: self.x + other.x, y: self.y + other.y, z: self.z + other.z}
    }
    fn neg(&self) -> Self 
    {
        Self{x: -self.x, y: -self.y, z: -self.z}
    }
    fn sub(&self, other: &Self) -> Self 
    {
        self.add(&other.neg())
    }
    fn transform(&self, mapping : impl Fn(i32) -> i32) -> Self
    {
        Self{x: mapping(self.x), y: mapping(self.y), z: mapping(self.z)}
    }
    fn compwise_binop(&self, other: &Self, mapping : impl Fn(i32, i32) -> i32) -> Self
    {
        Self{x: mapping(self.x, other.x), y: mapping(self.y, other.y), z: mapping(self.z, other.z)}
    }

}
type Moon = (Vec3D, Vec3D);

fn parse_position(posStr: &str) -> Moon {
    let coords : Vec<i32> = posStr.split(',').map(|s| s.split('=').dropping(1).next().unwrap().parse().unwrap()).collect();
    (Vec3D {x: coords[0], y: coords[1], z: coords[2]}, Vec3D::new()) 
}
fn parse_file(file : &str) -> Vec<Moon> 
{
    file.lines().map(|line| parse_position(line.chars().dropping_back(1).dropping(1).as_str())).collect() 
}
fn modify_velocities((pos1, ref mut vel1) : &mut Moon, (pos2, ref mut vel2) : &mut Moon) 
{
    let dif_vec = Vec3D::compwise_binop(pos1, pos2, |c1, c2| match c2-c1 {0 => {0}, dif if dif>0 => {1}, _ => {-1}});
    *vel1 = pos1.add(&dif_vec); 
    *vel2 = pos2.sub(&dif_vec);
}
fn modify_position((ref mut pos, vel) : &mut Moon)
{
    *pos = pos.add(vel);
}
fn move_moons(moons: &Vec<&mut Moon>)
{
   for (moon1, moon2)  in itertools::iproduct!(moons, moons) {
        modify_velocities(moon1, moon2);
    }

}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/12.txt").unwrap();
    let moons = parse_file(&file);
    println!("{:?}", moons);
    let sol1: u64 = 0;
    let sol2: u64 = 0;
    
    (Solution::from(sol1), Solution::from(sol2))
}
