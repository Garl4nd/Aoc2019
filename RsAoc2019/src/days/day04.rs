use crate::{Solution, SolutionPair};
use std::{collections::HashSet, fs::read_to_string};

///////////////////////////////////////////////////////////////////////////////
// genNums1F :: Int -> Int -> [Int]
// genNums1F places fixedPlace = go 1 9
//  where
//   go :: Int -> Int -> [Int]
//   go currentPlace currentMax
//     | currentPlace > places = [0]
//     | currentPlace == places = [0 .. currentMax]
//     | currentPlace == fixedPlace = [11 * n + 100 * rest | n <- [1 .. currentMax], rest <- go (currentPlace + 2) n]
//     | otherwise = [val + 10 * rest | val <- [1 .. currentMax], rest <- go (currentPlace + 1) val]
//
fn fib(n: i32) -> i32 {
    let mut stack: Vec<(i32, i32, i32)> = vec![(n, 0, 1)];
    let mut res: Option<i32> = None;
    while let Some((n, mut val, position)) = stack.pop() {
        // println!("{:?}", (n, val, position));
        if n < 2 {
            res = Some(n);
        } else if position == 4 {
            res = Some(val);
        } else {
            if let Some(res_val) = res {
                val += res_val;
                res = None;
            }
            // println!("Below: {:?}", (n, val, position));
            stack.push((n, val, position + 1));
            stack.push((n - position, 0, 1));
        }
    }

    res.unwrap()
}
fn get_nums1_stack(places: i32, fixed_place: i32) -> Vec<i32> {
    // for currentPlace in 1..=places {
    let mut stack: Vec<(i32, i32, i32, Vec<i32>)> = vec![(1, 1, 9, Vec::new())];
    let mut res: Option<Vec<i32>> = None; //Vec::new();
    while let Some((current_place, mut val, max, mut current_list)) = stack.pop() {
        if current_place == places {
            res = Some((1..=max).collect());
        } else {
            if let Some(res_vec) = res {
                current_list.extend(res_vec.iter().map(|r| val + 10 * r)); //push(val + 10 *r)
                res = None;
                val += 1;
            }
            if val <= max {
                stack.push((current_place, val, max, current_list));
                stack.push((current_place + 1, 0, val, Vec::new()))
            } else {
                res = Some(current_list);
            }
        }
        // while current_place <= places {
        //         nums_and_mins = nums_and_mins
        //             .iter()
        //             .flat_map(|(num, min)| (*min..=9).map(|num_| (10 * (*num) + num_, num_)))
        //             .collect();
        //         current_place += 1;
        //     };
        //
        //
    }
    res.unwrap()
}
fn get_nums1_f(places: i32, fixed_place: i32) -> Vec<i32> {
    let mut nums_and_mins: Vec<(i32, i32)> = vec![(0, 1)];
    // for currentPlace in 1..=places {
    let mut current_place = 1;
    while current_place <= places {
        if current_place == fixed_place {
            nums_and_mins = nums_and_mins
                .iter()
                .flat_map(|(num, min)| (*min..=9).map(|num_| (100 * (*num) + 11 * num_, num_)))
                .collect();
            current_place += 2;
        } else {
            nums_and_mins = nums_and_mins
                .iter()
                .flat_map(|(num, min)| (*min..=9).map(|num_| (10 * (*num) + num_, num_)))
                .collect();
            current_place += 1;
        };
    }

    nums_and_mins.iter().map(|(n, _)| *n).collect()
}
// genNums2F :: Int -> Int -> [Int]
// genNums2F places fixedPlace = if places == 0 then [] else go 1 9
//  where
//   go :: Int -> Int -> [Int]
//   go currentPlace currentMax
//     | currentPlace > places = [0]
//     | currentPlace == places = [0 .. currentMax]
//     | currentPlace == fixedPlace = [11 * n + 100 * rest | n <- [1 .. if currentPlace == 1 then currentMax else currentMax - 1], rest <- go (currentPlace + 2) (n - 1)]
//     | otherwise = [val + 10 * rest | val <- [1 .. currentMax], rest <- go (currentPlace + 1) val]
//
fn get_nums2_f(places: i32, fixed_place: i32) -> Vec<i32> {
    let mut nums_and_mins: Vec<(i32, i32)> = vec![(0, 1)];
    // for currentPlace in 1..=places {
    let mut current_place = 1;
    while current_place <= places {
        if current_place == fixed_place {
            nums_and_mins = nums_and_mins
                .iter()
                .flat_map(|(num, min)| {
                    ((if current_place == 0 { *min } else { *min + 1 })..=9)
                        .map(|num_| (100 * (*num) + 11 * num_, num_ + 1))
                })
                .collect();
            current_place += 2;
        } else {
            nums_and_mins = nums_and_mins
                .iter()
                .flat_map(|(num, min)| (*min..=9).map(|num_| (10 * (*num) + num_, num_)))
                .collect();
            current_place += 1;
        };
    }

    nums_and_mins.iter().map(|(n, _)| *n).collect()
}

fn get_nums(places: i32, single_fixed_func: impl Fn(i32, i32) -> Vec<i32>) -> Vec<i32> {
    (1..=(places - 1))
        .flat_map(|fixed_place| single_fixed_func(places, fixed_place))
        .filter(|n| (125730..579381).contains(n))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect()
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    // println!("{:?}", get_nums1(3));
    println!("{:?}", get_nums1_stack(3, 0));
    for i in 0..=10 {
        println!("fib {} = {} ", i, fib(i));
    }
    let nums = get_nums(6, get_nums1_f);
    let nums2 = get_nums(6, get_nums2_f);
    let sol1: u64 = nums.len() as u64;
    let sol2: u64 = nums2.len() as u64;

    (Solution::from(sol1), Solution::from(sol2))
}
