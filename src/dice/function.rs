use nom::{character::complete, combinator::opt, multi::many0, Parser};
use nom_supreme::ParserExt;

use crate::nom_support::IResult;

use super::{identifier, DiceExpr};

#[derive(Debug, PartialEq)]
pub enum BuiltinFunction {
    Max,
    Min,
    // If(DiceExpr, DiceExpr, DiceExpr),
}

impl BuiltinFunction {
    fn require_args(&self) -> bool {
        true
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    func: BuiltinFunction,
    args: Vec<DiceExpr>,
}

impl Function {
    fn parse(input: &str) -> IResult<Self> {
        let (input, func) = identifier
            .map_res(|res| match res.as_str() {
                "max" => Ok(BuiltinFunction::Max),
                "min" => Ok(BuiltinFunction::Min),
                r => Err(anyhow::anyhow!("{} is not a builtin function", r)),
            })
            .parse(input)?;
        let (input, _) = complete::char('(')(input)?;
        let (input, first) = if func.require_args() {
            DiceExpr::parse.map(Option::Some).parse(input)?
        } else {
            opt(DiceExpr::parse)(input)?
        };

        if first.is_none() {
            return Ok((
                input,
                Self {
                    func,
                    args: Vec::new(),
                },
            ));
        }

        let (input, mut args) = many0(|input| {
            let (input, _) = complete::space0.complete().parse(input)?;
            let (input, _) = complete::char(',')(input)?;
            let (input, _) = complete::space0.complete().parse(input)?;
            let (input, arg) = DiceExpr::parse(input)?;
            Ok((input, arg))
        })(input)?;

        let (input, _) = complete::char(')')(input)?;

        args.insert(0, first.unwrap());

        Ok((input, Self { func, args }))
    }
}

#[test]
fn test_function_parse() {
    assert!(Function::parse("max(1d2, 2d3)").is_ok());
    assert!(Function::parse("max(1d2, 2d3").is_err());
    assert!(Function::parse("max()").is_err());
    assert!(Function::parse("max(1d2,").is_err());
    assert!(Function::parse("max(1d2,)").is_err());
    assert!(Function::parse("max").is_err());
}
