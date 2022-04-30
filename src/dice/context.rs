use std::collections::HashMap;

use nom::Parser;
use nom_supreme::ParserExt;

use super::{alias::Alias, DiceExpr, Identifier};

#[derive(Debug)]
pub struct EvaluationContext {
    aliases: HashMap<Identifier, DiceExpr>,
}

impl EvaluationContext {
    pub fn new() -> Self {
        let mut context = EvaluationContext {
            aliases: HashMap::new(),
        };
        context.init_builtin();
        context
    }

    fn init_builtin(&mut self) {
        for alias in [
            "bonus = (min(1d10, 1d10) - 1) * 10 + (1d10 - 1)",
            "penalty = (max(1d10, 1d10) - 1) * 10 + (1d10 - 1)",
        ] {
            let (_, alias) = Alias::parse
                .all_consuming()
                .parse(alias)
                .expect("failed to parse builtin alias");
            self.add_alias(alias);
        }
    }

    pub fn get_alias(&self, name: &Identifier) -> Option<&DiceExpr> {
        self.aliases.get(name)
    }

    pub fn add_alias(&mut self, alias: Alias) {
        let Alias { name, body } = alias;
        self.aliases.insert(name, body);
    }
}
