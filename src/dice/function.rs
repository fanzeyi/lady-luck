use nom::{character::complete, combinator::opt, multi::many0, Parser};
use nom_supreme::ParserExt;

use crate::nom_support::IResult;

use super::{identifier, DiceExpr};

#[derive(Clone, Debug, PartialEq)]
pub enum BuiltinFunction {
    Max,
    Min,
    // If(DiceExpr, DiceExpr, DiceExpr),
}

impl BuiltinFunction {
    fn require_args(&self) -> bool {
        true
    }

    fn evaluate(&self, args: &Vec<DiceExpr>) -> Option<i128> {
        match *self {
            Self::Max => args
                .iter()
                .map(|x| x.evaluate())
                .max()
                .expect("function arguments should not be empty"),
            Self::Min => args
                .iter()
                .map(|x| x.evaluate())
                .min()
                .expect("function arguments should not be empty"),
        }
    }
}

impl ToString for BuiltinFunction {
    fn to_string(&self) -> String {
        match self {
            Self::Max => "max".to_string(),
            Self::Min => "min".to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    func: BuiltinFunction,
    args: Vec<DiceExpr>,
}

impl Function {
    pub fn parse(input: &str) -> IResult<Self> {
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

    pub fn roll(self) -> Self {
        let Self { func, args } = self;

        Self {
            func,
            args: args.into_iter().map(|x| x.roll()).collect(),
        }
    }

    pub fn evaluate(&self) -> Option<i128> {
        self.func.evaluate(&self.args)
    }
}

impl ToString for Function {
    fn to_string(&self) -> String {
        format!(
            "{}({})",
            self.func.to_string(),
            self.args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
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
