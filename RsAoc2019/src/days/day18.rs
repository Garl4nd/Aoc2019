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
    fn intersection(&self, other: &Self) -> Self;
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
    fn intersection(&self, other: &Self) -> Self {
        Keys(self.0 & other.0)
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
    fn intersection(&self, other: &Self) -> Self {
        Doors(self.0 & other.0)
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
    let openable_doors = Doors(all_keys.0);
    dbg!(&keypath_map);
    struct Context<'a>(&'a HashMap<char, Vec<KeyPath>>, &'a Keys, &'a Doors);
    let ctx = Context(&keypath_map, &all_keys, &openable_doors);
    fn find_shortest(
        memo: &mut HashMap<(i32, char), usize>,
        ctx: &Context<'_>,
        current_key: char,
        keys: Keys,
    ) -> usize {
        let Context(keypath_map, all_keys, openable_doors) = ctx;
        let map_idx = (keys.0, current_key);
        if let Some(dist) = memo.get(&map_idx) {
            *dist
        } else {
            let dist = if keys.0 == all_keys.0 {
                0
            } else {
                let key_paths = &keypath_map[&current_key];
                let walkable_paths = key_paths
                    .iter()
                    .filter(|key_path| {
                        !keys.contains(key_path.key)
                            && keys.can_open(&key_path.door_list.intersection(openable_doors))
                    })
                    .collect_vec();
                walkable_paths
                    .into_iter()
                    .map(|key_path| {
                        key_path.path_length
                            + find_shortest(
                                memo,
                                ctx,
                                key_path.key,
                                keys.add(key_path.key),
                                //                 concat([keys_dbg.clone(), vec![key_path.key]]),
                            )
                    })
                    .min()
                    .unwrap_or(2000000000)
            };
            memo.insert(map_idx, dist);
            dist
        }
    }

    find_shortest(&mut HashMap::new(), &ctx, '@', Keys::empty())
}
fn split_maze(maze: &GridVec<char>) -> Vec<GridVec<char>> {
    let (halfHeight, halfWidth): (i64, i64) =
        ((maze.height() / 2) as i64, (maze.width() / 2) as i64);
    let mut mod_pos = Vec::<(i64, i64)>::new();
    for dy in [-1, 1] {
        for dx in [-1, 1] {
            mod_pos.push((halfHeight + dy, halfWidth + dx));
        }
    }
    let mut res = Vec::new();
    for y_offset in [0, halfHeight + 1] {
        for x_offset in [0, halfWidth + 1] {
            let ylb = y_offset as i64;
            let xlb = x_offset as i64;
            let yub = (y_offset + halfHeight) as i64 - 1;
            let xub = (x_offset + halfWidth) as i64 - 1;
            dbg!(halfHeight, y_offset, x_offset, ylb, xlb, yub, xub);
            let mut new_grid = GridVec::new(' ', ylb, xlb, yub, xub);
            for y in ylb..=yub {
                for x in xlb..=xub {
                    if mod_pos.contains(&(y, x)) {
                        new_grid[(y, x)] = '@';
                    } else {
                        new_grid[(y, x)] = maze[(y, x)];
                    }
                }
            }
            res.push(new_grid);
        }
    }
    res
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
    let subgrids = split_maze(&grid);
    let dists: usize = subgrids.iter().map(|sg| shortest_path(sg)).sum();
    dbg!(dists);
    // println!("{}", &subgrids[1]);
    (Solution::from(sol1), Solution::from(sol2))
}
