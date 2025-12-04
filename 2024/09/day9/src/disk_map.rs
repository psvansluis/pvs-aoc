use crate::disk_content::DiskContent;
#[derive(Debug)]
pub struct DiskMap(Vec<DiskContent>);

impl DiskMap {
    pub fn new(source: &str) -> DiskMap {
        DiskMap(
            source
                .chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .enumerate()
                .map(|(i, c)| match i % 2 {
                    0 => DiskContent::File {
                        length: c,
                        index: i / 2,
                    },
                    _ => DiskContent::Free { length: c },
                })
                .collect(),
        )
    }

    fn checksum(&self) -> u64 {
        self.0.iter().enumerate().map(|i, content| match content {
            DiskContent::Free { .. } => 0,
            DiskContent::File { index, .. }
        })
    }

    pub fn solve_part_1(&self) -> u64 {}

    pub fn solve_part_2(&self) -> u64 {}
}
