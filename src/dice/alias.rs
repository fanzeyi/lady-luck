use nom::character::complete;

#[cfg(test)]
use crate::dice::Dice;
use crate::nom_support::IResult;

use super::{DiceExpr, Identifier};

#[derive(Clone, Debug, PartialEq)]
pub struct Alias {
    pub name: Identifier,
    pub body: DiceExpr,
}

impl Alias {
    pub fn parse(input: &str) -> IResult<Self> {
        let (input, name) = Identifier::parse(input)?;
        let (input, _) = complete::space0(input)?;
        let (input, _) = complete::char('=')(input)?;
        let (input, _) = complete::space0(input)?;
        let (input, body) = DiceExpr::parse(input)?;

        Ok((input, Self { name, body }))
    }

    pub fn explain(&self) -> String {
        format!("{} = {}", self.name, self.body.explain())
    }
}

impl ToString for Alias {
    fn to_string(&self) -> String {
        format!("{} = {}", self.name.as_ref(), self.body.to_string())
    }
}

#[test]
fn test_alias() {
    assert_eq!(
        Alias::parse("foo = 1d20").unwrap().1,
        Alias {
            name: Identifier::new("foo".to_string()),
            body: DiceExpr::Dice(Dice::new(1, 20)),
        }
    );
}
