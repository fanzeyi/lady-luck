use nom::{character::streaming::one_of, combinator::opt, multi::many0, Parser};
use nom_supreme::ParserExt;

use crate::{
    dice::{Dice, RollResult},
    nom_support::IResult,
};

#[derive(Debug, PartialEq)]
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
struct ExprMulDiv {
    lhs: Dice,
    rhs: Vec<(Operator, Dice)>,
}

impl ExprMulDiv {
    fn parse(input: &str) -> IResult<Self> {
        let (input, lhs) = Dice::parse(input)?;
        let (input, rhs) = opt(many0(|input| {
            let (input, _) = nom::character::complete::space0.complete().parse(input)?;
            let (input, op) = one_of("*/").complete().parse(input)?;
            let (input, _) = nom::character::complete::space0.complete().parse(input)?;
            let (input, rhs) = Dice::parse(input)?;
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
            let lhs = DiceExpr::Dice(lhs);
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

        let lhs = DiceExpr::Dice(self.lhs);

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

#[derive(Debug, PartialEq)]
pub enum DiceExpr {
    Dice(Dice),
    Roll(RollResult),
    Expression {
        lhs: Box<DiceExpr>,
        op: Operator,
        rhs: Box<DiceExpr>,
    },
}

impl DiceExpr {
    pub fn parse(input: &str) -> IResult<DiceExpr> {
        let (input, expr) = ExprAddSub::parse(input)?;

        Ok((input, expr.to_expr()))
    }

    pub fn roll(self) -> Self {
        match self {
            Self::Dice(dice) => DiceExpr::Roll(dice.roll()),
            Self::Roll(_) => panic!("Cannot roll a roll result"),
            Self::Expression { lhs, op, rhs } => match op {
                Operator::Add => DiceExpr::Expression {
                    lhs: Box::new(lhs.roll()),
                    op,
                    rhs: Box::new(rhs.roll()),
                },
                Operator::Sub => DiceExpr::Expression {
                    lhs: Box::new(lhs.roll()),
                    op,
                    rhs: Box::new(rhs.roll()),
                },
                Operator::Mul => DiceExpr::Expression {
                    lhs: Box::new(lhs.roll()),
                    op,
                    rhs: Box::new(rhs.roll()),
                },
                Operator::Div => DiceExpr::Expression {
                    lhs: Box::new(lhs.roll()),
                    op,
                    rhs: Box::new(rhs.roll()),
                },
            },
        }
    }

    pub fn result(&self) -> Option<i128> {
        match self {
            Self::Dice(_) => None,
            Self::Roll(roll) => Some(roll.sum() as i128),
            Self::Expression { lhs, op, rhs } => match op {
                Operator::Add => Some(lhs.result()? + rhs.result()?),
                Operator::Sub => Some(lhs.result()? - rhs.result()?),
                Operator::Mul => Some(lhs.result()? * rhs.result()?),
                Operator::Div => Some(lhs.result()? / rhs.result()?),
            },
        }
    }
}

impl ToString for DiceExpr {
    fn to_string(&self) -> String {
        match self {
            DiceExpr::Dice(dice) => dice.to_string(),
            DiceExpr::Roll(roll) => roll.to_string(),
            DiceExpr::Expression { lhs, op, rhs } => {
                let formula = format!(
                    "({} {} {})",
                    lhs.to_string(),
                    op.to_string(),
                    rhs.to_string()
                );

                if let Some(result) = self.result() {
                    format!("{} = {}", formula, result)
                } else {
                    formula
                }
            }
        }
    }
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

    dbg!(DiceExpr::parse("1d6+2d6*3d6").unwrap().1.roll().to_string());
}
