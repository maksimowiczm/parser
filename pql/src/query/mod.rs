use crate::query::query_argument::Argument;
use crate::query::query_builder::ResultType;
use crate::query::query_declaration::QueryDeclaration;
use std::collections::HashMap;

pub mod query_argument;
pub mod query_builder;
pub mod query_declaration;

#[derive(Debug, Default)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Query {
    declarations: HashMap<String, Argument>,
    queries: Vec<QueryDeclaration>,
    result_type: ResultType,
}
