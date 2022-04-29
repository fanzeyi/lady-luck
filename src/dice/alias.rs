use nom::character::complete;

use crate::{dice::Dice, nom_support::IResult};

use super::{identifier, DiceExpr};

#[derive(Clone, Debug, PartialEq)]
pub struct Alias {
    name: String,
    body: DiceExpr,
}

impl Alias {
    fn parse(input: &str) -> IResult<Self> {
        let (input, name) = identifier(input)?;
        let (input, _) = complete::space0(input)?;
        let (input, _) = complete::char('=')(input)?;
        let (input, _) = complete::space0(input)?;
        let (input, body) = DiceExpr::parse(input)?;

        Ok((input, Self { name, body }))
    }
}

#[test]
fn test_alias() {
    assert_eq!(
        Alias::parse("foo = 1d20").unwrap().1,
        Alias {
            name: "foo".to_string(),
            body: DiceExpr::Dice(Dice::new(1, 20)),
        }
    );
}
