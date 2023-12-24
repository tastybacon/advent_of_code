use std::{
    fs::File,
    io::{self, BufRead},
    path::{Path, PathBuf},
};

use clap::Parser;
use regex::Regex;

#[derive(Parser)]
struct CliOptions {
    #[arg(short, long, default_value_t = false)]
    parse_letters: bool,
    filename: PathBuf,
}

struct CalibrationDocumentParser {
    regex: Regex,
}

impl CalibrationDocumentParser {
    fn new(parse_letters: bool) -> Self {
        let regex = if parse_letters {
            Regex::new(r"(one|two|three|four|five|six|seven|eight|nine|[1-9])").unwrap()
        } else {
            Regex::new(r"([1-9])").unwrap()
        };
        Self { regex }
    }

    fn parse_line(&self, line: &str) -> Option<u32> {
        let first = self.regex.find(line)?.as_str();
        // I wonder if reversing the string+regex patterns would be better than this.
        let last = line
            .char_indices()
            .rev()
            .find_map(|(from, _)| self.regex.find_at(line, from).map(|v| v.as_str()))?;
        Some(str_to_val(first) * 10 + str_to_val(last))
    }
}

fn main() -> Result<(), Error> {
    let cli_options = CliOptions::parse();
    let result = run(&cli_options.filename, cli_options.parse_letters)?;
    println!("Result: {result}");
    Ok(())
}

pub fn run<P>(filename: P, parse_letters: bool) -> Result<u32, Error>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    let parser = CalibrationDocumentParser::new(parse_letters);
    Ok(io::BufReader::new(file)
        .lines()
        .map(|line| {
            let Ok(line) = line else {
                eprintln!("Error reading line: {}", line.unwrap_err());
                return 0;
            };
            parser.parse_line(&line).unwrap()
        })
        .sum())
}

fn str_to_val(s: &str) -> u32 {
    match s {
        "zero" | "0" => 0,
        "one" | "1" => 1,
        "two" | "2" => 2,
        "three" | "3" => 3,
        "four" | "4" => 4,
        "five" | "5" => 5,
        "six" | "6" => 6,
        "seven" | "7" => 7,
        "eight" | "8" => 8,
        "nine" | "9" => 9,
        _ => {
            eprintln!("Unrecognized str: {s}");
            0
        }
    }
}

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    ArgumentError,
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Self {
        Error::IoError(value)
    }
}

#[cfg(test)]
mod test {
    use crate::CalibrationDocumentParser;

    #[test]
    fn part_1_example() {
        let result = super::run("resources/day01/example_input.txt", false);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 142_u32);
    }

    #[test]
    fn part_2_example() {
        let result = super::run("resources/day01/part_2_example_input.txt", true);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 281_u32);
    }

    fn parser() -> CalibrationDocumentParser {
        CalibrationDocumentParser::new(true)
    }

    #[test]
    fn empty_string() {
        let result = parser().parse_line("");
        assert!(result.is_none());
    }

    #[test]
    fn no_match() {
        let result = parser().parse_line("abcdef");
        assert!(result.is_none());
    }

    #[test]
    fn single_number() {
        let result = parser().parse_line("1");
        assert_eq!(result, Some(11));
        let result = parser().parse_line("one");
        assert_eq!(result, Some(11));
    }

    #[test]
    fn parse_letters() {
        let result = parser().parse_line("1two");
        assert_eq!(result, Some(12));
        let result = CalibrationDocumentParser::new(false).parse_line("1two");
        assert_eq!(result, Some(11));
    }

    #[test]
    fn two_numbers() {
        let result = parser().parse_line("12");
        assert_eq!(result, Some(12));
        let result = parser().parse_line("onetwo");
        assert_eq!(result, Some(12));
        let result = parser().parse_line("1two");
        assert_eq!(result, Some(12));
        let result = parser().parse_line("one2");
        assert_eq!(result, Some(12));
    }

    #[test]
    fn many_numbers() {
        let result = parser().parse_line("123456789");
        assert_eq!(result, Some(19));
        let result = parser().parse_line("onetwo3fourfivesixseven89");
        assert_eq!(result, Some(19));
    }

    #[test]
    fn tricky() {
        let result = parser().parse_line("1234oneight");
        assert_eq!(result, Some(18));
    }
}
