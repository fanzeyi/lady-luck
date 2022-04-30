use nom::{
    branch::alt,
    character::{complete, streaming::one_of},
    combinator::opt,
    multi::many0,
    Finish, Parser,
};
use nom_supreme::{
    error::ErrorTree,
    final_parser::{final_parser, Location},
    ParserExt,
};

use crate::{
    dice::{Dice, RollResult},
    nom_support::IResult,
};

use super::function::Function;

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Operator {
    fn parse(input: &str) -> IResult<Self> {
        let (input, ops) = one_of("+-*/").context("operator").parse(input)?;
        Ok((
            input,
            match ops {
                '+' => Self::Add,
                '-' => Self::Sub,
                '*' => Self::Mul,
                '/' => Self::Div,
                _ => unreachable!(),
            },
        ))
    }
}

impl ToString for Operator {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "+".to_string(),
            Self::Sub => "-".to_string(),
            Self::Mul => "*".to_string(),
            Self::Div => "/".to_string(),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Term {
    Dice(Dice),
    Constant(i32),
    Function(Function),
}

impl Term {
    fn parse(input: &str) -> IResult<Self> {
        alt((
            Dice::parse.map(Term::Dice),
            complete::i32.map(Term::Constant),
            Function::parse.map(Term::Function),
        ))(input)
    }

    fn to_expr(self) -> DiceExpr {
        match self {
            Self::Dice(dice) => DiceExpr::Dice(dice),
            Self::Constant(constant) => DiceExpr::Constant(constant),
            Self::Function(function) => DiceExpr::Function(function),
        }
    }
}

#[derive(Debug, PartialEq)]
struct ExprMulDiv {
    lhs: Term,
    rhs: Vec<(Operator, Term)>,
}

impl ExprMulDiv {
    fn parse(input: &str) -> IResult<Self> {
        let (input, lhs) = Term::parse(input)?;
        let (input, rhs) = opt(many0(|input| {
            let (input, _) = nom::character::complete::space0.complete().parse(input)?;
            let (input, op) = one_of("*/").complete().parse(input)?;
            let (input, _) = nom::character::complete::space0.complete().parse(input)?;
            let (input, rhs) = Term::parse(input)?;
            let op = match op {
                '*' => Operator::Mul,
                '/' => Operator::Div,
                _ => unreachable!(),
            };

            Ok((input, (op, rhs)))
        }))(input)?;
        Ok((
            input,
            Self {
                lhs,
                rhs: rhs.unwrap_or_default(),
            },
        ))
    }

    fn to_expr(self) -> DiceExpr {
        let mut current = None;

        for (new_op, lhs) in self.rhs.into_iter().rev() {
            let lhs = lhs.to_expr();
            if let Some((op, rhs)) = current {
                let node = DiceExpr::Expression {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                };
                current = Some((new_op, node));
            } else {
                current = Some((new_op, lhs))
            }
        }

        let lhs = self.lhs.to_expr();

        match current {
            Some((op, rhs)) => DiceExpr::Expression {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            None => lhs,
        }
    }
}

#[derive(Debug, PartialEq)]
struct ExprAddSub {
    lhs: ExprMulDiv,
    rhs: Vec<(Operator, ExprMulDiv)>,
}

impl ExprAddSub {
    fn parse(input: &str) -> IResult<Self> {
        let (input, lhs) = ExprMulDiv::parse(input)?;
        let (input, rhs) = many0(|input| {
            let (input, _) = nom::character::complete::space0(input)?;
            let (input, op) = one_of("+-").complete().parse(input)?;
            let (input, _) = nom::character::complete::space0(input)?;
            let (input, rhs) = ExprMulDiv::parse(input)?;
            let op = match op {
                '+' => Operator::Add,
                '-' => Operator::Sub,
                _ => unreachable!(),
            };

            Ok((input, (op, rhs)))
        })(input)?;
        Ok((input, Self { lhs, rhs }))
    }

    fn to_expr(self) -> DiceExpr {
        let mut current = None;

        for (new_op, lhs) in self.rhs.into_iter().rev() {
            let lhs = lhs.to_expr();
            if let Some((op, rhs)) = current {
                let node = DiceExpr::Expression {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                };
                current = Some((new_op, node));
            } else {
                current = Some((new_op, lhs))
            }
        }

        let lhs = self.lhs.to_expr();

        match current {
            Some((op, rhs)) => DiceExpr::Expression {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            None => lhs,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DiceExpr {
    Constant(i32),
    Dice(Dice),
    Roll(RollResult),
    Function(Function),
    Expression {
        lhs: Box<DiceExpr>,
        op: Operator,
        rhs: Box<DiceExpr>,
    },
}

impl DiceExpr {
    pub fn parse_input(input: &str) -> Result<DiceExpr, ErrorTree<Location>> {
        final_parser(Self::parse)(input)
    }

    pub fn parse(input: &str) -> IResult<DiceExpr> {
        let (input, expr) = ExprAddSub::parse(input)?;

        Ok((input, expr.to_expr()))
    }

    pub fn roll(self) -> Self {
        match self {
            Self::Constant(x) => Self::Constant(x),
            Self::Dice(dice) => Self::Roll(dice.roll()),
            Self::Roll(_) => panic!("Cannot roll a roll result"),
            Self::Function(function) => Self::Function(function.roll()),
            Self::Expression { lhs, op, rhs } => match op {
                Operator::Add => Self::Expression {
                    lhs: Box::new(lhs.roll()),
                    op,
                    rhs: Box::new(rhs.roll()),
                },
                Operator::Sub => Self::Expression {
                    lhs: Box::new(lhs.roll()),
                    op,
                    rhs: Box::new(rhs.roll()),
                },
                Operator::Mul => Self::Expression {
                    lhs: Box::new(lhs.roll()),
                    op,
                    rhs: Box::new(rhs.roll()),
                },
                Operator::Div => Self::Expression {
                    lhs: Box::new(lhs.roll()),
                    op,
                    rhs: Box::new(rhs.roll()),
                },
            },
        }
    }

    pub fn evaluate(&self) -> Option<i128> {
        match self {
            Self::Constant(x) => Some(*x as i128),
            Self::Dice(_) => None,
            Self::Roll(roll) => Some(roll.sum() as i128),
            Self::Function(function) => function.evaluate(),
            Self::Expression { lhs, op, rhs } => match op {
                Operator::Add => Some(lhs.evaluate()? + rhs.evaluate()?),
                Operator::Sub => Some(lhs.evaluate()? - rhs.evaluate()?),
                Operator::Mul => Some(lhs.evaluate()? * rhs.evaluate()?),
                Operator::Div => Some(lhs.evaluate()? / rhs.evaluate()?),
            },
        }
    }
}

impl ToString for DiceExpr {
    fn to_string(&self) -> String {
        match self {
            Self::Constant(x) => x.to_string(),
            Self::Dice(dice) => dice.to_string(),
            Self::Roll(roll) => roll.to_string(),
            Self::Function(function) => function.to_string(),
            Self::Expression { lhs, op, rhs } => {
                format!(
                    "({} {} {})",
                    lhs.to_string(),
                    op.to_string(),
                    rhs.to_string()
                )
            }
        }
    }
}

#[test]
fn test_parse_constant() {
    assert_eq!(DiceExpr::parse("1").unwrap().1.to_string(), "1");
    assert_eq!(DiceExpr::parse("-1").unwrap().1.to_string(), "-1");
    assert_eq!(DiceExpr::parse("-100").unwrap().1.to_string(), "-100");
    assert_eq!(DiceExpr::parse("100000").unwrap().1.to_string(), "100000");
    assert!(DiceExpr::parse("10000000000000000").is_err());
    assert!(DiceExpr::parse("abdfasf").is_err());
}

#[test]
fn test_parse_dice_expr() {
    assert_eq!(
        DiceExpr::parse("1d6+2d6+3d6").unwrap().1.to_string(),
        "(1d6 + (2d6 + 3d6))",
    );

    assert_eq!(
        DiceExpr::parse("1d6*2d6+3d6").unwrap().1.to_string(),
        "((1d6 * 2d6) + 3d6)",
    );

    assert_eq!(
        DiceExpr::parse("1d6+2d6*3d6").unwrap().1.to_string(),
        "(1d6 + (2d6 * 3d6))",
    );

    assert_eq!(
        DiceExpr::parse("1d6+2d6*3d6-4d6/5d6*6d6+7d6")
            .unwrap()
            .1
            .to_string(),
        "(1d6 + ((2d6 * 3d6) - ((4d6 / (5d6 * 6d6)) + 7d6)))",
    );

    assert_eq!(
        DiceExpr::parse("1d6      *  2d6\t  + \t \t \t 3d6")
            .unwrap()
            .1
            .to_string(),
        "((1d6 * 2d6) + 3d6)",
    );

    assert_eq!(
        DiceExpr::parse("1*2+3").unwrap().1.to_string(),
        "((1 * 2) + 3)",
    );

    assert_eq!(
        DiceExpr::parse("1*2+3d5").unwrap().1.to_string(),
        "((1 * 2) + 3d5)",
    );
}

#[cfg(test)]
fn roll_expr(expr: &str) -> DiceExpr {
    DiceExpr::parse(expr).unwrap().1.roll()
}

#[test]
fn test_expr_roll() {
    assert_eq!(roll_expr("6d1 + 3d1").evaluate().unwrap(), 9);
    assert_eq!(roll_expr("6d1 - 3d1").evaluate().unwrap(), 3);
    assert_eq!(roll_expr("6d1 * 3d1").evaluate().unwrap(), 18);
    assert_eq!(roll_expr("6d1 / 3d1").evaluate().unwrap(), 2);
    assert_eq!(roll_expr("6d1 / 3").evaluate().unwrap(), 2);
    assert_eq!(roll_expr("6 + 3d1 * 2d1").evaluate().unwrap(), 12);
}

#[test]
fn test_expr_function() {
    assert_eq!(roll_expr("max(6d1, 3d1)").evaluate().unwrap(), 6);
    assert_eq!(roll_expr("min(6d1, 3d1)").evaluate().unwrap(), 3);
    assert_eq!(
        roll_expr("min(6d1, max(3d1 + 2d1 * 6d1 / 3d1 - 1, 5d1))")
            .evaluate()
            .unwrap(),
        6
    );
}
