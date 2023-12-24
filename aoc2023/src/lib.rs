use std::{
    fs::File,
    io::{BufRead, BufReader, Lines},
    path::Path,
};

pub fn read_lines<P>(filename: P) -> Result<Lines<BufReader<File>>, std::io::Error>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    let buf = BufReader::new(file);
    Ok(buf.lines())
}
