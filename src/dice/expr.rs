use anyhow::{anyhow, Result};
use nom::{
    branch::alt,
    character::{complete, streaming::one_of},
    combinator::opt,
    multi::many0,
    Parser,
};
use nom_supreme::ParserExt;

use crate::{
    dice::{Dice, RollResult},
    nom_support::IResult,
};

use super::{alias::Alias, function::Function, EvaluationContext, Identifier};

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
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
    Parenthesis(Box<ExprAddSub>),
    Alias(Identifier),
}

impl Term {
    fn parse_parenthesis(input: &str) -> IResult<Box<ExprAddSub>> {
        let (input, _) = complete::char('(')(input)?;
        let (input, expr) = ExprAddSub::parse(input)?;
        let (input, _) = complete::char(')')(input)?;

        Ok((input, Box::new(expr)))
    }

    fn parse(input: &str) -> IResult<Self> {
        alt((
            Dice::parse.map(Term::Dice),
            complete::i32.map(Term::Constant),
            Function::parse.map(Term::Function),
            Self::parse_parenthesis.map(Term::Parenthesis),
            Identifier::parse.map(Term::Alias),
        ))(input)
    }

    fn to_expr(self) -> DiceExpr {
        match self {
            Self::Dice(dice) => DiceExpr::Dice(dice),
            Self::Constant(constant) => DiceExpr::Constant(constant),
            Self::Function(function) => DiceExpr::Function(function),
            Self::Parenthesis(expr) => expr.to_expr(),
            Self::Alias(ident) => DiceExpr::Alias(ident),
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
    Alias(Identifier),
    AliasResult(Box<Alias>),
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

    pub fn roll(self, context: &mut EvaluationContext) -> Result<Self> {
        match self {
            Self::Constant(x) => Ok(Self::Constant(x)),
            Self::Dice(dice) => Ok(Self::Roll(dice.roll())),
            Self::Roll(_) => Err(anyhow!("cannot roll a roll result")),
            Self::Function(function) => Ok(Self::Function(function.roll(context)?)),
            Self::Alias(ident) => match context.get_alias(&ident) {
                Some(expr) => Ok(Self::AliasResult(Box::new(expr.clone().roll(context)?))),
                None => Err(anyhow!("alias '{}' not found", ident)),
            },
            Self::AliasResult(_) => Err(anyhow!("cannot roll an alias result")),
            Self::Expression { lhs, op, rhs } => Ok(Self::Expression {
                lhs: Box::new(lhs.roll(context)?),
                op,
                rhs: Box::new(rhs.roll(context)?),
            }),
        }
    }

    pub fn evaluate(&self) -> Option<i128> {
        match self {
            Self::Constant(x) => Some(*x as i128),
            Self::Dice(_) => None,
            Self::Roll(roll) => Some(roll.sum() as i128),
            Self::Function(function) => function.evaluate(),
            Self::Alias(_) => None,
            Self::AliasResult(alias) => alias.evaluate(),
            Self::Expression { lhs, op, rhs } => match op {
                Operator::Add => Some(lhs.evaluate()? + rhs.evaluate()?),
                Operator::Sub => Some(lhs.evaluate()? - rhs.evaluate()?),
                Operator::Mul => Some(lhs.evaluate()? * rhs.evaluate()?),
                Operator::Div => Some(lhs.evaluate()? / rhs.evaluate()?),
            },
        }
    }

    pub fn explain(&self) -> String {
        match self {
            Self::Constant(x) => x.to_string(),
            Self::Dice(_) => "".to_owned(),
            Self::Roll(roll) => roll.explain(),
            Self::Function(function) => function.explain(),
            Self::Alias(ident) => ident.to_string(),
            Self::AliasResult(alias) => alias.explain(),
            Self::Expression { lhs, op, rhs } => {
                format!("{} {} {}", lhs.explain(), op.to_string(), rhs.explain())
            }
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
            Self::Alias(alias) => alias.to_string(),
            Self::AliasResult(alias) => alias.to_string(),
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
        DiceExpr::parse("(1d6+2d6)*3d6").unwrap().1.to_string(),
        "((1d6 + 2d6) * 3d6)",
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
    DiceExpr::parse(expr)
        .unwrap()
        .1
        .roll(&mut EvaluationContext::new())
        .unwrap()
}

#[test]
fn test_expr_roll() {
    assert_eq!(roll_expr("6d1 + 3d1").evaluate().unwrap(), 9);
    assert_eq!(roll_expr("6d1 - 3d1").evaluate().unwrap(), 3);
    assert_eq!(roll_expr("6d1 * 3d1").evaluate().unwrap(), 18);
    assert_eq!(roll_expr("6d1 / 3d1").evaluate().unwrap(), 2);
    assert_eq!(roll_expr("6d1 / 3").evaluate().unwrap(), 2);
    assert_eq!(roll_expr("6 + 3d1 * 2d1").evaluate().unwrap(), 12);
    assert_eq!(roll_expr("(6 + 3d1) * 2d1").evaluate().unwrap(), 18);
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
