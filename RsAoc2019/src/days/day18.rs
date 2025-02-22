use console::Key;
use itertools::Itertools;

use crate::{
    etc::gridvec::{GridPos, GridVec},
    Solution, SolutionPair,
};
use std::{fs::read_to_string, io::empty};

// trait Encode {
//     fn empty() -> Self;
//     fn encode(symbol: char) -> Self
//     where
//         Self: Sized,
//     {
//         Self::empty().add(symbol)
//     }
//     fn get_encoding(&self) -> i32;
//     fn from_encoding(enc: i32) -> Self;
//     fn add(&self, new_symbol: char) -> Self
//     where
//         Self: Sized,
//     {
//         let code = self.get_encoding();
//         let new_code = Self::encode(new_symbol).get_encoding();
//         Self::from_encoding(code | new_code)
//     }
// }
// enum EncodingType {
//     Door,
//     Key,
// }
// struct Keys(i32);
// struct Doors(i32);
// impl Encode for Keys {
struct Keys(i32);
#[derive(Clone)]
struct Doors(i32);
trait ListEncoding {
    fn empty() -> Self;
    fn add(&self, symbol: char) -> Self;
    fn encode(symbol: char) -> Self
    where
        Self: Sized,
    {
        Self::empty().add(symbol)
    }
}
impl ListEncoding for Keys {
    fn empty() -> Keys {
        Keys(0)
    }
    fn add(&self, symbol: char) -> Self {
        let offset = 'a' as i32;
        let Keys(keys) = self;
        Keys(keys | (1 << (symbol as i32 - offset)))
    }
}
impl ListEncoding for Doors {
    fn empty() -> Doors {
        Doors(0)
    }
    fn add(&self, symbol: char) -> Self {
        let offset = 'A' as i32;
        let Doors(doors) = self;
        Doors(doors | (1 << (symbol as i32 - offset)))
    }
}

fn can_open(Keys(keys): &Keys, Doors(doors): &Doors) -> bool {
    keys & doors != 0
}

struct KeyPath {
    key: Keys,
    path_length: usize,
    door_list: Doors,
}
struct Explorer {
    pos: GridPos,
    previous_pos: Option<GridPos>,
    door_list: Doors,
}
///////////////////////////////////////////////////////////////////////////////
fn key_paths(maze: &GridVec<char>, init_pos: GridPos, init_key: char) -> Vec<KeyPath> {
    let mut explorers = vec![Explorer {
        pos: init_pos,
        previous_pos: None,
        door_list: Doors::empty(),
    }];
    let mut found_paths = Vec::<KeyPath>::new();
    let mut distance: usize = 0;

    while !explorers.is_empty() {
        let mut new_explorers = Vec::<Explorer>::new();
        for explorer in &mut explorers {
            let symbol = maze[explorer.pos];
            if symbol.is_ascii_lowercase() && symbol != init_key {
                found_paths.push(KeyPath {
                    key: Keys::encode(symbol),
                    path_length: distance,
                    door_list: explorer.door_list.clone(),
                });
            }
            if symbol.is_ascii_uppercase() {
                explorer.door_list.add(symbol);
            }
            let neighbors = maze
                .neighbors4(explorer.pos)
                .into_iter()
                .filter(|nei| Some(*nei) != explorer.previous_pos);
            for neighbor in neighbors {
                new_explorers.push(Explorer {
                    pos: neighbor,
                    previous_pos: Some(explorer.pos),
                    door_list: explorer.door_list.clone(),
                });
            }
        }
        explorers = new_explorers;
    }
    Vec::new()
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let sol1: u64 = 0;
    let sol2: u64 = 0;
    let key = Keys::encode('f');
    dbg!(
        can_open(&key, &Doors::encode('F')),
        can_open(&key, &Doors::encode('Q'))
    );
    let merged_key = key.add('q');

    dbg!(
        can_open(&merged_key, &Doors::encode('F')),
        can_open(&merged_key, &Doors::encode('Q')),
        can_open(&merged_key, &Doors::encode('T'))
    );
    (Solution::from(sol1), Solution::from(sol2))
}
