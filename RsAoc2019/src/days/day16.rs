use crate::{Solution, SolutionPair};
use itertools::{iterate, Itertools};
use nalgebra::{
    self, ArrayStorage, DVector, Dyn, Matrix, OMatrix, OVector, RowOVector, RowVector, VecStorage,
    U2, U3,
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
fn parse_file(file: &str) -> DVector<i32> {
    let nums = file
        .lines()
        .flat_map(|line| line.chars().map(|c| c.to_digit(10).unwrap() as i32))
        .collect();
    DVector::from_vec(nums)
}
fn multiPhaseCalc(phases: usize, mat: &DMatrix<i32>, vec: &DVector<i32>) -> DVector<i32> {
    iterate(vec.clone(), |vec| {
        let mul = mat * (vec);
        mul.map(|r| if r > 0 { r % 10 } else { (-r) % 10 })
    })
    .nth(phases)
    .unwrap()
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let sol1: u64 = 0;
    let sol2: u64 = 0;
    let file = read_to_string("input/16_test.txt").unwrap();
    dbg!(&file);
    let nums = parse_file(&file);
    let matrix = build_matrix(nums.len());
    print!("vec= {}", nums);
    print!("{}", matrix);
    for phase in 0..10 {
        let res = multiPhaseCalc(phase, &matrix, &nums);

        println!("res = {}", res);
    }
    (Solution::from(sol1), Solution::from(sol2))
}
