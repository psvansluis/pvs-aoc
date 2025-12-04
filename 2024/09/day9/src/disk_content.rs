use std::iter;

#[derive(Debug)]
pub enum DiskContent {
    Free { length: u8 },
    File { length: u8, index: usize },
}

impl DiskContent {
    pub fn into_iter(&self) -> impl std::iter::Iterator<Item = usize> {
        match &self {
            DiskContent::Free { length } => iter::repeat(0).take(*length as usize),
            DiskContent::File { length, index } => iter::repeat(*index).take(*length as usize),
        }
    }
}
