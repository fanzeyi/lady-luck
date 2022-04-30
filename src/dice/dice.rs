use std::iter;

#[cfg(test)]
use nom::Finish;
use nom::{character::complete, Parser};
use nom_supreme::ParserExt;
use rand::distributions::Uniform;
use rand::prelude::Distribution;

use crate::nom_support::IResult;

#[derive(Clone, Debug, PartialEq)]
pub struct Dice {
    rolls: u8,
    sides: u8,
}

impl Dice {
    pub fn new(rolls: u8, sides: u8) -> Self {
        Dice { rolls, sides }
    }

    pub fn parse(input: &str) -> IResult<Dice> {
        let (input, rolls) = complete::u8.context("parse rolls").parse(input)?;
        let (input, _) = complete::char('d')(input)?;
        let (input, sides) = complete::u8.context("parse sides").parse(input)?;

        Ok((input, Dice::new(rolls, sides)))
    }

    pub fn roll(self) -> RollResult {
        if self.sides < 1 {
            let rolls = iter::repeat(0).take(self.rolls as usize).collect();
            return RollResult { dice: self, rolls };
        }

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

#[derive(Clone, Debug, PartialEq)]
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
        self.dice.to_string()
    }
}

#[test]
fn test_parse_dice() {
    assert_eq!(Dice::parse("1d6").unwrap().1, Dice::new(1, 6));
    assert_eq!(Dice::parse("2d9").unwrap().1, Dice::new(2, 9));
    assert_eq!(Dice::parse("255d255").unwrap().1, Dice::new(255, 255));
    assert_eq!(
        Dice::parse("300d30").finish().unwrap_err().to_string(),
        "in section \"parse rolls\" at 300d30,\nexpected an ascii digit at 300d30"
    );
    assert_eq!(
        Dice::parse("30d300").finish().unwrap_err().to_string(),
        "in section \"parse sides\" at 300,\nexpected an ascii digit at 300"
    );
}

#[test]
fn test_dice_roll() {
    assert_eq!(Dice::new(6, 1).roll().sum(), 6);
    assert_eq!(Dice::new(6, 0).roll().sum(), 0);
    assert_eq!(Dice::new(6, 0).roll().rolls, vec![0, 0, 0, 0, 0, 0]);
    // assert_eq!(Dice::new(0, 0).roll().rolls, vec![]);
    // assert_eq!(Dice::new(0, 10).roll().rolls, vec![]);
}
