use anyhow::Result;
#[cfg(test)]
use nom::Finish;
use nom::{combinator::map_res, error::context};
use rand::distributions::Uniform;
use rand::prelude::Distribution;

use crate::nom_support::IResult;

#[derive(Debug, PartialEq)]
pub struct Dice {
    rolls: u8,
    sides: u8,
}

fn parse_u8(input: &str) -> Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 10)
}

fn parse_u8_str(input: &str) -> IResult<u8> {
    map_res(nom::character::complete::digit1, parse_u8)(input)
}

impl Dice {
    pub fn new(rolls: u8, sides: u8) -> Self {
        Dice { rolls, sides }
    }

    pub fn parse(input: &str) -> IResult<Dice> {
        let (input, rolls) = context("parse rolls", parse_u8_str)(input)?;
        let (input, _) = nom::character::complete::char('d')(input)?;
        let (input, sides) = context("parse sides", parse_u8_str)(input)?;

        Ok((input, Dice::new(rolls, sides)))
    }

    pub fn roll(self) -> RollResult {
        let between = Uniform::from(1..=self.sides);
        let mut rolls = Vec::with_capacity(self.rolls as usize);
        let mut rng = rand::thread_rng();

        for _ in 0..self.rolls {
            rolls.push(between.sample(&mut rng));
        }

        RollResult { dice: self, rolls }
    }
}

impl ToString for Dice {
    fn to_string(&self) -> String {
        format!("{}d{}", self.rolls, self.sides)
    }
}

#[derive(Debug, PartialEq)]
pub struct RollResult {
    dice: Dice,
    rolls: Vec<u8>,
}

impl RollResult {
    pub fn sum(&self) -> u32 {
        self.rolls.iter().map(|x| *x as u32).sum()
    }
}

impl ToString for RollResult {
    fn to_string(&self) -> String {
        format!(
            "{}{{{}}} -> {}",
            self.dice.to_string(),
            self.rolls
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(", "),
            self.sum(),
        )
    }
}

#[test]
fn test_parse_dice() {
    assert_eq!(Dice::parse("1d6").unwrap().1, Dice::new(1, 6));
    assert_eq!(Dice::parse("2d9").unwrap().1, Dice::new(2, 9));
    assert_eq!(Dice::parse("255d255").unwrap().1, Dice::new(255, 255));
    assert_eq!(
        Dice::parse("300d30").finish().unwrap_err().to_string(),
        "in section \"parse rolls\" at 300d30,\nexternal error:\n  number too large to fit in target type at 300d30"
    );
    assert_eq!(
        Dice::parse("30d300").finish().unwrap_err().to_string(),
        "in section \"parse sides\" at 300,\nexternal error:\n  number too large to fit in target type at 300"
    );
}
