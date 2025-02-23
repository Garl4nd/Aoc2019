use console::Key;
use itertools::{concat, Itertools};

use crate::{
    etc::gridvec::{string_to_chargrid, GridPos, GridVec},
    Solution, SolutionPair,
};
use std::{
    collections::{hash_set, HashMap, HashSet},
    fs::read_to_string,
    iter::once,
    thread::sleep,
    time::Duration,
};

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
#[derive(Debug)]
struct Keys(i32);
#[derive(Debug, Clone)]
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

impl Keys {
    fn contains(&self, key: char) -> bool {
        self.0 & Self::encode(key).0 != 0
    }
    fn can_open(&self, Doors(doors): &Doors) -> bool {
        self.0 & doors == *doors
    }
}

#[derive(Debug)]
struct KeyPath {
    key: char,
    path_length: usize,
    door_list: Doors,
}
#[derive(Debug)]
struct Explorer {
    pos: GridPos,
    door_list: Doors,
}
///////////////////////////////////////////////////////////////////////////////
fn key_paths(maze: &GridVec<char>, init_pos: GridPos, init_key: char) -> Vec<KeyPath> {
    let mut explorers = vec![Explorer {
        pos: init_pos,
        door_list: Doors::empty(),
    }];
    let mut visited_pos = HashSet::<GridPos>::new();
    // let mut found_keys = HashSet::<char>::new();
    let mut found_paths = Vec::<KeyPath>::new();
    let mut distance: usize = 0;

    while !explorers.is_empty() {
        let mut new_explorers = Vec::<Explorer>::new();
        for explorer in &mut explorers {
            visited_pos.insert(explorer.pos);
            let symbol = maze[explorer.pos];
            if symbol.is_ascii_lowercase() && symbol != init_key {
                //&& !found_keys.contains(&symbol) {

                found_paths.push(KeyPath {
                    key: symbol,
                    path_length: distance,
                    door_list: explorer.door_list.clone(),
                });
                // found_keys.insert(symbol);
            }
            if symbol.is_ascii_uppercase() {
                // dbg!("Found door", symbol, &explorer.door_list);
                explorer.door_list = explorer.door_list.add(symbol);
                // dbg!("New door",&explorer.door_list);
            }
            let neighbors = maze
                .neighbors4(explorer.pos)
                .into_iter()
                .filter(|nei| maze[*nei] != '#' && !visited_pos.contains(nei));
            //dbg!(explorer.pos);
            for neighbor in neighbors {
                //      dbg!(neighbor);
                new_explorers.push(Explorer {
                    pos: neighbor,
                    door_list: explorer.door_list.clone(),
                });
            }
        }

        //dbg!(&explorers, &new_explorers);
        //sleep(Duration::from_millis(500)
        //);
        explorers = new_explorers;
        distance += 1;
    }
    found_paths
}
fn gen_key_path_map(maze: &GridVec<char>) -> HashMap<char, Vec<KeyPath>> {
    let mut keypath_map = HashMap::new();
    for key in once('@').chain('a'..='z') {
        let key_pos = maze.find(key);
        if let Some(key_pos) = key_pos {
            keypath_map.insert(key, key_paths(maze, key_pos, key));
        }
    }
    keypath_map
}
fn shortest_path(maze: &GridVec<char>) -> usize {
    let keypath_map = gen_key_path_map(maze);
    let all_key_chars = keypath_map.keys().filter(|c| **c != '@');
    let all_keys = all_key_chars.fold(Keys::empty(), |keys, c| keys.add(*c));
    dbg!(&keypath_map);
    let memo_map = HashMap::<(i32, char), usize>::new();
    fn find_shortest(
        keypath_map: &HashMap<char, Vec<KeyPath>>,
        current_key: char,
        keys: Keys,
        keys_dbg: Vec<char>,
        all_keys: &Keys,
        memo: &mut HashMap<(i32, char), usize>,
    ) -> usize {
        // dbg!(&keys, &all_keys);
        // println!("Visiting {current_key}");
        // dbg!(&keys, &keys_dbg);
        // sleep(Duration::from_millis(500));
        let map_idx = (keys.0, current_key);
        if !memo.contains_key(&map_idx) {
            if keys.0 == all_keys.0 {
                // println!("Match!");
                dbg!(&keys_dbg);
                memo.insert(map_idx, 0);
            } else {
                let key_paths = &keypath_map[&current_key];
                let walkable_paths = key_paths
                    .iter()
                    .filter(|key_path| {
                        !keys.contains(key_path.key) && keys.can_open(&key_path.door_list)
                    })
                    .collect_vec();
                // dbg!(&walkable_paths);
                let dist = walkable_paths
                    .into_iter()
                    .map(|key_path| {
                        key_path.path_length
                            + find_shortest(
                                keypath_map,
                                key_path.key,
                                keys.add(key_path.key),
                                concat([keys_dbg.clone(), vec![key_path.key]]),
                                all_keys,
                                memo,
                            )
                    })
                    .min()
                    .unwrap_or(2000000000);
                memo.insert(map_idx, dist);
            };
        }
        memo[&map_idx]
    }

    find_shortest(
        &keypath_map,
        '@',
        Keys::empty(),
        Vec::new(),
        &all_keys,
        &mut HashMap::new(),
    )
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let sol1: u64 = 0;
    let sol2: u64 = 0;
    let key = Keys::encode('f');
    dbg!(
        key.can_open(&Doors::encode('F')),
        key.can_open(&Doors::encode('Q')),
    );
    let merged_key = key.add('q');

    dbg!(
        merged_key.can_open(&Doors::encode('F')),
        merged_key.can_open(&Doors::encode('Q')),
        merged_key.can_open(&Doors::encode('A')),
    );
    dbg!(
        merged_key.contains('f'),
        merged_key.contains('a'),
        merged_key.contains('q')
    );
    let file = read_to_string("input/18.txt").unwrap();
    let grid = string_to_chargrid(&file);
    dbg!(grid.width(), grid.height());
    print!("{grid}");
    // dbg!(key_paths(&grid, (0,0), 'a'));
    dbg!(grid.find('@'));
    let init_pos = grid.find('@').unwrap();
    dbg!(key_paths(&grid, init_pos, '@'));
    dbg!(shortest_path(&grid));
    (Solution::from(sol1), Solution::from(sol2))
}
