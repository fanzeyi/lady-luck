use nom::character::complete::{self};

use crate::nom_support::IResult;

pub fn identifier(input: &str) -> IResult<String> {
    let (input, first) = complete::alpha1(input)?;
    let (input, second) = complete::alphanumeric0(input)?;

    Ok((input, format!("{}{}", first, second)))
}
