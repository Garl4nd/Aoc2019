use crate::{
    etc::intcode::{self, IntMachine},
    Solution, SolutionPair,
};
use std::fs::read_to_string;

///////////////////////////////////////////////////////////////////////////////
// paintShip :: Int -> [Int] -> (A.Array GridPos Int, A.UArray GridPos Bool)
// paintShip initTile code = runST calc
//  where
//   calc :: forall s. ST s (A.Array GridPos Int, A.UArray GridPos Bool)
//   calc = do
//     shipAr <- newArray @(STUArray s) ((-height, -width), (height, width)) 0
//     writeArray shipAr (0,0) initTile
//     paintedPanels <- newArray @(STUArray s) ((-height, -width), (height, width)) False
//     computer <- createMachine @(STUArray s) code
//     flip fix ((0, 0), U) $ \loop (currentPos, currentDir) -> do
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
static WIDTH: usize = 91;
static HEIGHT: usize = 91;
static GRID_SIZE: usize = WIDTH * HEIGHT;
fn to_idx1(x: usize, y: usize) -> usize {
    y * WIDTH + x
}
fn paint_ship(init_tile: bool, code: &[i64]) -> ([bool; GRID_SIZE], [bool; GRID_SIZE]) {
    let mut ship_grid: [bool; GRID_SIZE] = [false; GRID_SIZE];
    ship_grid[to_idx1(45, 45)] = init_tile;
    let mut painted_panels: [bool; GRID_SIZE] = [false; GRID_SIZE];
    let mut computer = IntMachine::new(code);
    (ship_grid, painted_panels)
}
pub fn solve() -> SolutionPair {
    // Your solution here...
    let sol1: u64 = 0;
    let sol2: u64 = 0;

    (Solution::from(sol1), Solution::from(sol2))
}
