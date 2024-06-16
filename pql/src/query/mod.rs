use crate::query::query_argument::Declaration;
use crate::query::query_builder::ResultType;
use crate::query::query_declaration::parent_declaration::ParentDeclaration;
use crate::query::query_declaration::QueryDeclaration;
use pkb::pkb_context::PkbContext;
use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

pub mod query_argument;
pub mod query_builder;
pub mod query_declaration;
pub mod query_executor;

#[derive(Debug, Clone, PartialEq)]
pub enum QueryDeclarationVariant {
    Parent(ParentDeclaration),
}

impl QueryDeclarationVariant {
    pub fn is_executable(&self) -> bool {
        match self {
            QueryDeclarationVariant::Parent(parent) => parent.is_executable(),
        }
    }

    pub fn execute(&self, context: &PkbContext) -> bool {
        match self {
            QueryDeclarationVariant::Parent(parent) => parent.execute(context),
        }
    }

    pub fn get_declarations(&self) -> Vec<Declaration> {
        match self {
            QueryDeclarationVariant::Parent(parent) => parent.get_declarations(),
        }
    }
}

#[cfg_attr(test, derive(Default))]
pub struct Query {
    declarations: HashMap<String, Declaration>,
    queries: Vec<QueryDeclarationVariant>,
    result_type: ResultType,
    pkb_context: Rc<PkbContext>,
}

impl Debug for Query {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Query")
            .field("declarations", &self.declarations)
            .field("queries", &self.queries)
            .field("result_type", &self.result_type)
            .finish()
    }
}

#[cfg(test)]
impl PartialEq for Query {
    fn eq(&self, other: &Self) -> bool {
        self.declarations == other.declarations
            && self.queries.len() == other.queries.len()
            && self.result_type == other.result_type
    }
}
