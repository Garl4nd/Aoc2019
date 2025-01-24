use crate::{Solution, SolutionPair};
use itertools::{iterate, Itertools};
use nalgebra::{
    self, ArrayStorage, DVector, Dyn, Matrix, OMatrix, OVector, RowOVector, RowVector, SMatrix,
    VecStorage, U2, U3,
};
use std::{
    fs::read_to_string,
    iter::{repeat, repeat_n},
};
///////////////////////////////////////////////////////////////////////////////
type DMatrix<T> = OMatrix<T, Dyn, Dyn>;
fn build_matrix(n: usize) -> DMatrix<i32> {
    let base_pattern = [0, 1, 0, -1];
    let row_it = (0..n).flat_map(|i| {
        let basic_pattern = base_pattern.iter().flat_map(move |el| repeat_n(*el, i + 1));
        repeat(basic_pattern).flatten().take(n + 1).dropping(1)
    });
    DMatrix::<i32>::from_row_iterator(n, n, row_it)
    //    Matrix
    //  DMatrix::from_rows(&rows)
}
fn parse_file(file: &str) -> Vec<i32> {
    file.lines()
        .flat_map(|line| line.chars().map(|c| c.to_digit(10).unwrap() as i32))
        .collect()
}
fn multiPhaseCalc(phases: usize, mat: &DMatrix<i32>, vec: &DVector<i32>) -> DVector<i32> {
    iterate(vec.clone(), |vec| {
        let mul = mat * (vec);
        mul.map(|r| if r > 0 { r % 10 } else { (-r) % 10 })
    })
    .nth(phases)
    .unwrap()
}
fn build_row_i(i: usize, n: usize) -> DVector<i32> {
    let base_pattern = [0, 1, 0, -1];
    let basic_pattern = base_pattern.iter().flat_map(move |el| repeat_n(*el, i + 1));
    DVector::<i32>::from_iterator(n, repeat(basic_pattern).flatten().take(n + 1).dropping(1))
}
//DMatrix::<i32>::from_row_iterator(n, n, row_it)
//    Matrix
//  DMatrix::from_rows(&rows)
fn val_at_simple(vec: &[i32]) -> Vec<i32> {
    //    let vec = nvec.iter().collect_vec();
    vec.iter()
        .scan(0, |accum, &n| {
            *accum = (*accum + n) % 10;
            Some(*accum)
        })
        .collect()
}
fn calc_iterate(vec: &[i32], iters: i32, i0: usize) -> Vec<i32> {
    let vec_slice = &vec[i0..];
    // let p = vec_slice.iter().rev().copied().collect_vec();
    iterate(vec_slice.iter().rev().copied().collect_vec(), |v| {
        val_at_simple(v)
    })
    .nth(iters as usize)
    .unwrap()
    .iter()
    .rev()
    .copied()
    .collect()
}
fn val_at_simple_periodic(i: i32, reps: i32, nvec: &DVector<i32>) -> DVector<i32> {
    let vec = nvec.iter().collect_vec();
    let pattern_length = vec.len() as i32;
    let vec_sum = nvec.sum() % 10;
    let dist_to_end = (reps * pattern_length) - (i + 1);
    let reps = dist_to_end / pattern_length;
    let pattern_position = i % pattern_length;
    let mut accum = reps * vec_sum;
    dbg!(reps, dist_to_end, vec_sum, accum);
    let mut solution_vec: Vec<i32> = Vec::new();
    for k in (pattern_position..pattern_length).rev() {
        accum = (accum + vec[k as usize]) % 10;
        solution_vec.push(accum);
    }
    solution_vec.reverse();
    DVector::<i32>::from_vec(solution_vec)
}

fn decimal_num_from_vec(vec: &[i32], n: usize) -> i32 {
    vec.iter().take(n).fold(0, |acc, x| 10 * acc + *x)
}

fn solution1(nums: &Vec<i32>) -> u64 {
    let matrix = build_matrix(nums.len());
    let dNums = DVector::<i32>::from_vec(nums.clone());
    let res = multiPhaseCalc(100, &matrix, &dNums);
    let res_vec = res.iter().copied().collect_vec();
    decimal_num_from_vec(&res_vec, 8) as u64
}
fn solution2(nums: &Vec<i32>) -> u64 {
    let reps = 10_000;
    let nums_rep: Vec<i32> = repeat_n(nums.iter().copied(), reps).flatten().collect();
    let skip_num = decimal_num_from_vec(&nums, 7);
    let res_vec = calc_iterate(
        &nums_rep.iter().copied().collect_vec(),
        100,
        skip_num as usize,
    );
    decimal_num_from_vec(&res_vec, 8) as u64
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let file = read_to_string("input/16.txt").unwrap();
    dbg!(&file);
    let nums = parse_file(&file);
    let sol1: u64 = solution1(&nums);
    let sol2: u64 = solution2(&nums);

    (Solution::from(sol1), Solution::from(sol2))
}
