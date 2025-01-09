use itertools::Itertools;

use crate::{Solution, SolutionPair};
use std::fs::read_to_string;

type Image = Vec<i32>;
///////////////////////////////////////////////////////////////////////////////
fn parse_file(file: &str, w : usize, h : usize) -> Vec<Image>{
   let  allNums = file.lines().next().unwrap().chars().map(|c|c.to_digit(10).unwrap() as i32);
    let mut layers : Vec<Image> = Vec::new();
    let mut iter = allNums.peekable();
    while iter.peek().is_some(){
        layers.push(iter.by_ref().take(w*h).collect());
    }
    layers
}
fn min_zero_layer(layers: &Vec<Image>) ->  &Image 
{
    (*layers).iter().min_by_key(|img| count_el(img, 0)).unwrap()
}
fn count_el(img: &Image, el: i32) -> usize{
    (*img).iter().filter(|&num| *num == el).count()
}

fn image_product(image: &Image) -> usize 
{
    let ones = count_el(image, 1);
    let twos = count_el(image, 2); 
    ones*twos
}
fn top_visible(layers: &Vec<Image>) -> Image 
{
    (0..25*6).map(|idx| {
     (0..(*layers).len()).map(|z| layers[z][idx]).find(|val| * val != 2).unwrap()}).collect() 
}
fn print_image(img: &Image)
{    for y in 0..6 {
        for x in 0..25{
            print!("{}",if img[y*25+x] == 0 {"     "} else {"  O  "});}
    
        println!("\n");
}}

pub fn solve() -> SolutionPair {
    // Your solution here..
    let file = read_to_string("input/8.txt").unwrap();
    let imgs = parse_file(&file, 25, 6);
    //println!("{:?}", imgs);
    println!("{:?}", top_visible(&imgs));
    let sol1: u64 = image_product(min_zero_layer(&imgs)) as u64;
    print_image(&top_visible(&imgs));
    let sol2: &str = "UGCUH";

    (Solution::from(sol1), Solution::from(sol2))
}
