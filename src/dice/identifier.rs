use std::fmt::Display;

use nom::character::complete::{self};

use crate::nom_support::IResult;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier(String);

impl Identifier {
    pub fn new(s: String) -> Self {
        Self(s)
    }

    pub fn parse(input: &str) -> IResult<Self> {
        let (input, first) = complete::alpha1(input)?;
        let (input, second) = complete::alphanumeric0(input)?;

        Ok((input, Identifier(format!("{}{}", first, second))))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
