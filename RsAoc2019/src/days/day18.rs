use console::Key;

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
    current_pos: GridPos,
    previous_pos: Option<GridPos>,
    path_length: usize,
    door_list: Doors,
}
///////////////////////////////////////////////////////////////////////////////
fn key_paths(maze: &GridVec<char>, pos: GridPos) -> Vec<KeyPath> {
    let mut explorers = vec![Explorer {
        current_pos: pos,
        previous_pos: None,
        path_length: 0,
        door_list: Doors::empty(),
    }];
    while !explorers.is_empty() {
        for explorer in &explorers {
            let possible_paths = maze.neighbors4(explorer.current_pos);
        }
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
