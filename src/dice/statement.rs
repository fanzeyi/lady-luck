use anyhow::Result;
use nom::{branch::alt, Parser};
use nom_supreme::{
    error::ErrorTree,
    final_parser::{final_parser, Location},
};

use crate::nom_support::IResult;

use super::{alias::Alias, DiceExpr, EvaluationContext};

pub enum DiceStatement {
    Alias(Alias),
    Expr(DiceExpr),
}

impl DiceStatement {
    pub fn parse_input(input: &str) -> Result<Self, ErrorTree<Location>> {
        final_parser(Self::parse)(input)
    }

    pub fn parse(input: &str) -> IResult<Self> {
        alt((
            Alias::parse.map(Self::Alias),
            DiceExpr::parse.map(Self::Expr),
        ))(input)
    }

    pub fn roll(self, context: &mut EvaluationContext) -> Result<Self> {
        match self {
            Self::Alias(alias) => {
                context.add_alias(alias.clone());
                Ok(Self::Alias(alias))
            }
            Self::Expr(expr) => Ok(Self::Expr(expr.roll(context)?)),
        }
    }

    pub fn evaluate(&self) -> Option<i128> {
        match self {
            Self::Alias(_) => None,
            Self::Expr(expr) => expr.evaluate(),
        }
    }

    pub fn explain(&self) -> String {
        match self {
            Self::Alias(alias) => alias.explain(),
            Self::Expr(expr) => expr.explain(),
        }
    }
}

impl ToString for DiceStatement {
    fn to_string(&self) -> String {
        match self {
            Self::Alias(alias) => alias.to_string(),
            Self::Expr(expr) => expr.to_string(),
        }
    }
}
