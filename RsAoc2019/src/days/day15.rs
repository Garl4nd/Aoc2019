use itertools::Itertools;

use crate::{
    etc::{
        gridvec::{turn, Direction, GridPos, GridVec},
        intcode::{codeParser, IntMachine},
    },
    Solution, SolutionPair,
};

///////////////////////////////////////////////////////////////////////////////

pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = codeParser("input/15.txt");
    let (sol1, sol2) = solution(&code);

    (Solution::from(sol1), Solution::from(sol2))
}
#[derive(Clone, Copy, Debug)]
enum Distance {
    Dist(i64),
    Inf,
    Unknown,
}
fn direction_to_input(direction: Direction) -> i64 {
    use Direction::*;
    match direction {
        U => 1,
        D => 2,
        L => 3,
        R => 4,
    }
}
fn opposite(direction: Direction) -> Direction {
    use Direction::*;
    match direction {
        D => U,
        U => D,
        L => R,
        R => L,
    }
}
fn add_dist(distance: &Distance, val: i64) -> Distance {
    if let Distance::Dist(dist) = distance {
        Distance::Dist(dist + val)
    } else {
        *distance
    }
}
fn auto_explore_low(
    robot: &mut IntMachine,
    dist_map: &mut GridVec<Distance>,
    oxygen_pos: &mut Option<GridPos>,
    current_pos: GridPos,
    current_dist: Distance,
    incoming_direction: Option<Direction>,
) {
    use Direction::*;
    for direction in [U, D, L, R] {
        let pos = turn(&current_pos, &direction);
        let val = &dist_map[pos];
        if let Distance::Unknown = val {
            let output = robot.run_machine(&[direction_to_input(direction)]);

            if let Some(0) = output.first() {
                dist_map[pos] = Distance::Inf;
            } else {
                if let Some(2) = output.first() {
                    *oxygen_pos = Some(pos);
                }
                let updated_dist = add_dist(&current_dist, 1);
                dist_map[pos] = updated_dist;
                auto_explore_low(
                    robot,
                    dist_map,
                    oxygen_pos,
                    pos,
                    updated_dist,
                    Some(direction),
                );
            }
        }
    }
    if let Some(incoming_direction) = incoming_direction {
        robot.run_machine(&[direction_to_input(opposite(incoming_direction))]);
    }
}

fn auto_explore(code: &[i64], init_pos: GridPos) -> (GridVec<Distance>, Option<GridPos>) {
    use Distance::*;
    let mut dist_map = GridVec::<Distance>::new(Unknown, -25, -25, 25, 25);
    dist_map[init_pos] = Dist(0);
    let mut oxygen_pos = None;
    let mut robot = IntMachine::new(code);
    auto_explore_low(
        &mut robot,
        &mut dist_map,
        &mut oxygen_pos,
        init_pos,
        Dist(0),
        None,
    );

    (dist_map, oxygen_pos)
}

fn auto_map(code: &[i64]) -> (GridVec<Distance>, Option<GridPos>) {
    let (dist_map, oxygen_pos) = auto_explore(code, (0, 0));
    let mut land_map = dist_map.remap(|dist| match dist {
        Distance::Dist(_) => '.',
        Distance::Inf => '#',
        _ => ' ',
    });
    land_map[(0, 0)] = '@';
    if let Some(oxygen_pos) = oxygen_pos {
        land_map[oxygen_pos] = '!';
    }
    print!("{land_map}");
    dbg!(oxygen_pos);
    (dist_map, oxygen_pos)
}
fn find_max_dist(init_pos: GridPos, walkable_map: &GridVec<bool>) -> i64 {
    use Direction::*;
    fn go(
        pos: &GridPos,
        current_dist: i64,
        forbidden: &[&GridPos],
        walkable_map: &GridVec<bool>,
    ) -> i64 {
        let possible_positions = [U, D, L, R]
            .iter()
            .filter_map(|dir| {
                let new_pos = turn(pos, dir);
                if !forbidden.contains(&&new_pos) && walkable_map[new_pos] {
                    Some(new_pos)
                } else {
                    None
                }
            })
            .collect_vec();
        if possible_positions.is_empty() {
            current_dist
        } else {
            possible_positions
                .iter()
                .map(|new_pos| go(new_pos, current_dist + 1, &[pos], walkable_map))
                .max()
                .unwrap()
        }
    }
    go(&init_pos, 0, &[], walkable_map)
}

fn solution(code: &[i64]) -> (i64, i64) {
    use Distance::*;
    let (dist_map, oxygen_pos) = auto_map(code);
    let oxygen_pos = oxygen_pos.unwrap();
    let walkable_map = dist_map.remap(|dist| matches!(dist, Dist(_)));
    let oxy_dist = match dist_map[oxygen_pos] {
        Dist(dist) => dist,
        _ => -1,
    };
    (oxy_dist, find_max_dist(oxygen_pos, &walkable_map))
}
