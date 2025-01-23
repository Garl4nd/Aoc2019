use crate::{Solution, SolutionPair};
use itertools::Itertools;
use nalgebra::{
    self, ArrayStorage, Dyn, Matrix, OMatrix, RowOVector, RowVector, VecStorage, U2, U3,
};
use std::{
    fs::read_to_string,
    iter::{repeat, repeat_n},
};
///////////////////////////////////////////////////////////////////////////////
type DMatrix<T> = OMatrix<T, Dyn, Dyn>;
fn buildMatrix(n: usize) -> DMatrix<i32> {
    let rows: Vec<RowOVector<i32, Dyn>> = Vec::new();
    let base_pattern = [0, 1, 0, -1];
    let row_it = (0..n).into_iter().flat_map(|i| {
        let basic_pattern = base_pattern.iter().flat_map(move |el| repeat_n(*el, i + 1));
        repeat(basic_pattern).flatten().take(n + 1).dropping(1)
    });
    DMatrix::<i32>::from_row_iterator(n, n, row_it)
    //    Matrix
    //  DMatrix::from_rows(&rows)
}

pub fn solve() -> SolutionPair {
    // Your solution here...
    let sol1: u64 = 0;
    let sol2: u64 = 0;

    let matrix = buildMatrix(12);
    print!("{}", matrix);
    (Solution::from(sol1), Solution::from(sol2))
}
