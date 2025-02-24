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

#[derive(Debug)]
struct Keys(i32);
#[derive(Debug, Clone)]
struct Doors(i32);
trait ListEncoding {
    fn get_enc(&self) -> i32;
    fn from_enc(enc: i32) -> Self;
    fn empty() -> Self;
    fn add(&self, symbol: char) -> Self;
    fn intersection(&self, other: &Self) -> Self
    where
        Self: std::marker::Sized,
    {
        Self::from_enc(self.get_enc() & other.get_enc())
    }
    fn encode(symbol: char) -> Self
    where
        Self: Sized,
    {
        Self::empty().add(symbol)
    }
    fn contains(&self, c: char) -> bool
    where
        Self: std::marker::Sized,
    {
        self.get_enc() & Self::encode(c).get_enc() != 0
    }
}
impl ListEncoding for Keys {
    fn get_enc(&self) -> i32 {
        self.0
    }
    fn from_enc(enc: i32) -> Self {
        Keys(enc)
    }
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
    fn get_enc(&self) -> i32 {
        self.0
    }
    fn from_enc(enc: i32) -> Self {
        Doors(enc)
    }
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
///////////////////////////////////////////////////////////////////////////////
fn key_paths(maze: &GridVec<char>, init_pos: GridPos, init_key: char) -> Vec<KeyPath> {
    struct Explorer {
        pos: GridPos,
        door_list: Doors,
        dist: usize,
    }
    let mut explorers = vec![Explorer {
        pos: init_pos,
        door_list: Doors::empty(),
        dist: 0,
    }];
    let mut visited_pos = HashSet::<GridPos>::new();
    // let mut found_keys = HashSet::<char>::new();
    let mut found_paths = Vec::<KeyPath>::new();
    // let mut distance: usize = 0;

    while let Some(mut explorer) = explorers.pop() {
        visited_pos.insert(explorer.pos);
        let symbol = maze[explorer.pos];
        if symbol.is_ascii_lowercase() && symbol != init_key {
            found_paths.push(KeyPath {
                key: symbol,
                path_length: explorer.dist,
                door_list: explorer.door_list.clone(),
            });
        }
        if symbol.is_ascii_uppercase() {
            explorer.door_list = explorer.door_list.add(symbol);
        }
        let neighbors = maze
            .neighbors4(explorer.pos)
            .into_iter()
            .filter(|nei| maze[*nei] != '#' && !visited_pos.contains(nei));
        for neighbor in neighbors {
            explorers.push(Explorer {
                pos: neighbor,
                door_list: explorer.door_list.clone(),
                dist: explorer.dist + 1,
            });
        }
        // distance += 1;
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
                let walkable_paths = key_paths.iter().filter(|key_path| {
                    !keys.contains(key_path.key)
                        && keys.can_open(&key_path.door_list.intersection(openable_doors))
                });
                walkable_paths
                    .map(|key_path| {
                        key_path.path_length
                            + find_shortest(memo, ctx, key_path.key, keys.add(key_path.key))
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
    let (half_height, half_width): (i64, i64) =
        ((maze.height() / 2) as i64, (maze.width() / 2) as i64);
    let mut mod_pos = Vec::<(i64, i64)>::new();
    for dy in [-1, 1] {
        for dx in [-1, 1] {
            mod_pos.push((half_height + dy, half_width + dx));
        }
    }
    let mut res = Vec::new();
    for y_offset in [0, half_height + 1] {
        for x_offset in [0, half_width + 1] {
            let (ylb, xlb) = (y_offset, x_offset);
            let (yub, xub) = (y_offset + half_height - 1, x_offset + half_width - 1);
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
fn solution1(maze: &GridVec<char>) -> usize {
    shortest_path(maze)
}
fn solution2(maze: &GridVec<char>) -> usize {
    let submazes = split_maze(maze);
    submazes.iter().map(shortest_path).sum()
}

pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/18.txt").unwrap();
    let maze = string_to_chargrid(&file); // println!("{}", &subgrids[1]);
    let sol1: usize = solution1(&maze);
    let sol2: usize = solution2(&maze);
    (Solution::from(sol1), Solution::from(sol2))
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn encoding_test() {
        let key = Keys::encode('f');
        debug_assert!(key.can_open(&Doors::encode('F')));
        assert!(!key.can_open(&Doors::encode('Q')));
    }
    #[test]
    fn merge_test() {
        let key = Keys::encode('f');
        let merged_key = key.add('q');

        assert!(merged_key.can_open(&Doors::encode('F')));
        assert!(merged_key.can_open(&Doors::encode('Q')));
        assert!(!merged_key.can_open(&Doors::encode('A')));
    }
}
