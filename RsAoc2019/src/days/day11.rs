use itertools::Itertools;

use crate::{
    etc::intcode::{self, codeParser, IntMachine, MachineState},
    etc::gridvec::GridVec, etc::gridvec::GridPos,
    Solution, SolutionPair,
};
use std::{fmt::Display, fs::read_to_string, ops::{Index, IndexMut, RangeBounds}};
enum Direction {
    U, 
    R,
    D, 
    L 
}

fn change_direction(dir: &Direction, turn_code: i64) -> Direction{
    use Direction::*;
    if turn_code == 0 {
        match dir{
            U => {L},
            D => {R},
            L => {D},
            R => {U}
        }
    } 
    else {
        match dir {
            U => {R},
            D => {L},
            L => {U},
            R => {D}
        }
    }
}
fn turn((y, x): &GridPos, dir : &Direction) -> GridPos 
{
    use Direction::*; 
    match *dir{
        U => {(*y-1, *x)}
        D => {(*y+1, *x)}
        L => {(*y, *x -1)}
        R => {(*y, *x +1)}
    }
}
// newDirection :: Int -> Direction -> Direction
// newDirection 0 = \case
//   U -> L
//   D -> R
//   L -> D
//   R -> U
// newDirection _ = \case
//   U -> R
//   D -> L
//   L -> U
//   R -> D

// turn :: GridPos -> Direction -> GridPos
// turn (y, x) U = (y - 1, x)
// turn (y, x) D = (y + 1, x)
// turn (y, x) L = (y, x - 1)
// turn (y, x) R = (y, x + 1)
// ///////////////////////////////////////////////////////////////////////////////
// paintShip :: Int -> [Int] -> (A.Array GridPos Int, A.UArray GridPos Bool)
// paintShip initTile code = runST calc
//  where
//   calc :: forall s. ST s (A.Array GridPos Int, A.UArray GridPos Bool)
//   calc = do
//     shipAr <- newArray @(STUArray s) ((-height, -width), (height, width)) 0
//     writeArray shipAr (0,0) initTile
//     paintedPanels <- newArray @(STUArray s) ((-height, -width), (height, width)) False
//     computer <- createMachine @(STUArray s) code
//     flip fix ((0*, 0), U) $ \loop (currentPos, currentDir) -> do
//       currentColor <- readArray shipAr currentPos
//       MachineResult{machineState, machineOutputs} <- getMachineResult =<< runMachine [currentColor] computer
//       unless (machineState == Halted) $ do
//         let [newColor, turnCode] = machineOutputs
//         writeArray shipAr currentPos newColor
//         when (newColor == 1) $ writeArray paintedPanels currentPos True
//         let newDir = newDirection turnCode currentDir
//         let newPos = turn currentPos newDir
//         loop (newPos, newDir)
//     shipArFr <- freeze shipAr
//     paintedPanelsFr <- freeze paintedPanels
//     return (shipArFr, paintedPanelsFr)
//
//width, height :: Int

// impl Display for GridVec<char>
// {
//     fn fmt(&self, ftr: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//     {
//         let mut res_str = String::from("");
//         for row in self.y_min..=self.y_max
//         {
//             let row_str : String = self.row_slice(row).iter().collect();
//             res_str.push_str(&row_str);
//             res_str.push('\n');
//         }
//     write!(ftr, "\n{}", res_str)
//     }
// }
// }
fn paint_ship(init_tile: bool, code: &[i64]) -> (GridVec<bool>, GridVec<bool>){
    let mut ship_grid = GridVec::<bool>::new(false, -45, -45, 45, 45); 
    ship_grid[(0,0)] = init_tile;
    let mut painted_panels = GridVec::<bool>::new(false, -45, -45, 45, 45); 
    let mut computer = IntMachine::new(code);
    let mut current_pos = (0,0);
    let mut current_dir = Direction::U ; 
    loop {
        let current_color = ship_grid[current_pos];
        let outputs = computer.run_machine(&[if current_color {1} else {0}]);
        if computer.state == MachineState::Halted {break}
        else {
        let (new_color, turn_code) = (outputs[0], outputs[1]);
        ship_grid[current_pos] =   new_color == 1 ;
        if new_color == 1 {painted_panels[current_pos] = true};
        current_dir = change_direction(&current_dir, turn_code);
        current_pos = turn(&current_pos, &current_dir);
        }
    }
    (ship_grid, painted_panels)
}
fn solution1(code: &[i64]) -> u64 { 
  let (_, painted_panels) = paint_ship(false, code); 
    painted_panels.vec.into_iter().filter(|el| *el).count() as u64}
fn solution2 (code: &[i64]) -> String { 
  let (final_paint_encoded, _) = paint_ship(true, code); 
  let finalPaint = final_paint_encoded.remap(|val| if *val {'O'} else {' '});/*  (\x -> if x>0 then 'O' else ' ') <$> finalPaintEncoded */
  format!("{}",finalPaint)
    //in unlines $  charGridToStr finalPaint
}


pub fn solve() -> SolutionPair {
    // Your solution here...
    let code = codeParser("input/11.txt");
    let sol1: u64 = solution1(&code);
    let sol2: String = solution2(&code);

    (Solution::from(sol1), Solution::from(sol2))
}
