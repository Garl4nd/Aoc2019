use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

use itertools::Itertools;

pub struct GridVec<T> {
    pub vec: Vec<T>,
    y_min: i64,
    x_min: i64,
    y_max: i64,
    x_max: i64,
}
fn string_to_chargrid(str: &str) -> GridVec<char> {
    let height = str.lines().count() as i64;
    let width = str.lines().next().iter().count() as i64;
    GridVec {
        vec: str.lines().flat_map(|line| line.chars()).collect(),
        y_min: 0,
        x_min: 0,
        y_max: height - 1,
        x_max: width - 1,
    }
}
pub type GridPos = (i64, i64);
impl<T> GridVec<T> {
    fn height(&self) -> usize {
        (self.y_max - self.y_min + 1) as usize
    }

    fn width(&self) -> usize {
        (self.x_max - self.x_min + 1) as usize
    }
    fn index1D(&self, (y, x): GridPos) -> usize {
        (y - self.y_min) as usize * self.width() + (x - self.x_min) as usize
    }
    fn row_slice(&self, y: i64) -> &[T] {
        &self.vec[self.index1D((y, self.x_min))..=self.index1D((y, self.x_max))]
    }
    fn in_range(&self, (y, x): &GridPos) -> bool {
        (self.y_min..=self.y_max).contains(y) && (self.x_min..=self.x_max).contains(x)
    }
    pub fn neighbors4(&self, (y, x): GridPos) -> Vec<GridPos> {
        [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]
            .into_iter()
            .filter(|nei| self.in_range(nei))
            .collect()
    }

    pub fn remap<S>(&self, f: impl Fn(&T) -> S) -> GridVec<S> {
        let remapped_vec: Vec<S> = self.vec.iter().map(f).collect();
        GridVec {
            vec: remapped_vec,
            y_min: self.y_min,
            x_min: self.x_min,
            y_max: self.y_max,
            x_max: self.x_max,
        }
    }
}
impl<T: Clone> Index<(i64, i64)> for GridVec<T> {
    type Output = T;
    fn index(&self, pos: GridPos) -> &Self::Output {
        &self.vec[self.index1D(pos)]
    }
}
impl<T: Clone> IndexMut<(i64, i64)> for GridVec<T> {
    fn index_mut(&mut self, pos: GridPos) -> &mut Self::Output {
        let index = self.index1D(pos);
        &mut self.vec[index]
    }
}

impl<T: Clone> GridVec<T> {
    pub fn new(init_val: T, y_min: i64, x_min: i64, y_max: i64, x_max: i64) -> Self {
        let height = y_max - y_min + 1;
        let width = x_max - x_min + 1;
        Self {
            vec: vec![init_val; (width * height) as usize],
            y_min,
            x_min,
            y_max,
            x_max,
        }
    }
}
impl Display for GridVec<char> {
    fn fmt(&self, ftr: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        {
            let mut res_str = String::from("");
            for row in self.y_min..=self.y_max {
                let row_str: String = self.row_slice(row).iter().collect();
                res_str.push_str(&row_str);
                res_str.push('\n');
            }
            write!(ftr, "\n{}", res_str)
        }
    }
}
#[derive(Clone, Copy)]
pub enum Direction {
    U,
    R,
    D,
    L,
}

pub fn change_direction(dir: &Direction, turn_code: i64) -> Direction {
    use Direction::*;
    if turn_code == 0 {
        match dir {
            U => L,
            D => R,
            L => D,
            R => U,
        }
    } else {
        match dir {
            U => R,
            D => L,
            L => U,
            R => D,
        }
    }
}
pub fn turn((y, x): &GridPos, dir: &Direction) -> GridPos {
    use Direction::*;
    match *dir {
        U => (*y - 1, *x),
        D => (*y + 1, *x),
        L => (*y, *x - 1),
        R => (*y, *x + 1),
    }
}
