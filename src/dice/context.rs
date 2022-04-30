use std::collections::HashMap;

use super::{alias::Alias, DiceExpr, Identifier};

#[derive(Debug)]
pub struct EvaluationContext {
    aliases: HashMap<Identifier, DiceExpr>,
}

impl EvaluationContext {
    pub fn new() -> Self {
        EvaluationContext {
            aliases: HashMap::new(),
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
