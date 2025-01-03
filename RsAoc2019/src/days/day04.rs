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
//
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

fn get_nums1(places: i32) -> Vec<i32> {
    (1..=(places - 1))
        .flat_map(|fixed_place| get_nums1_f(places, fixed_place))
        .filter(|n| (125730..579381).contains(n))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect()
}
fn get_nums2(places: i32) -> Vec<i32> {
    (1..=(places - 1))
        .flat_map(|fixed_place| get_nums2_f(places, fixed_place))
        .filter(|n| (125730..579381).contains(n))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect()
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    println!("{:?}", get_nums1(3));
    let nums = get_nums1(6);
    let nums2 = get_nums2(6);
    let sol1: u64 = nums.len() as u64;
    let sol2: u64 = nums2.len() as u64;

    (Solution::from(sol1), Solution::from(sol2))
}
