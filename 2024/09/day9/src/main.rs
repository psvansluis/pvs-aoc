mod disk_content;
mod disk_map;

use std::fs;

use disk_map::DiskMap;

fn main() {
    let data = fs::read_to_string("./resources/testData.txt").unwrap();
    println!("{}", data);
    let disk_map = DiskMap::new(&data);
    println!("{disk_map:?}");
}
