use crate::query::{Query, QueryDeclarationVariant};
use pkb::pkb_context::PkbContext;
use std::collections::HashMap;
use std::rc::Rc;

use crate::query::query_argument::Declaration;
use crate::query::query_declaration::parent_declaration::ParentDeclaration;
#[cfg(test)]
use mockall::automock;

#[cfg_attr(test, automock)]
pub trait QueryBuilder {
    fn add_declaration(&mut self, declaration: (String, Vec<String>));
    fn set_result(&mut self, result: ResultType);
    fn add_follows(&mut self, predecessor: String, follower: String);
    fn add_parent(&mut self, parent: String, child: String);
    fn build(&self, context: Rc<PkbContext>) -> Query;
}

#[derive(Default, Clone, Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ResultType {
    Single(String),
    #[default]
    Boolean,
}

#[derive(Default)]
pub struct QueryBuilderImpl {
    declarations: Vec<(String, Vec<String>)>,
    result_type: ResultType,
    follows: Vec<(String, String)>,
    parent: Vec<(String, String)>,
}

impl QueryBuilder for QueryBuilderImpl {
    fn add_declaration(&mut self, declaration: (String, Vec<String>)) {
        self.declarations.push(declaration);
    }

    fn set_result(&mut self, result: ResultType) {
        self.result_type = result;
    }

    fn add_follows(&mut self, predecessor: String, follower: String) {
        self.follows.push((predecessor, follower));
    }

    fn add_parent(&mut self, parent: String, child: String) {
        self.parent.push((parent, child));
    }

    fn build(&self, context: Rc<PkbContext>) -> Query {
        let declarations = parse_declarations(&self.declarations);

        let parent = self
            .parent
            .iter()
            .map(|(parent, child)| map_parent(&declarations, parent, child))
            .map(|declaration| QueryDeclarationVariant::Parent(declaration));

        let queries = parent.collect();

        Query {
            declarations,
            queries,
            result_type: self.result_type.clone(),
            pkb_context: context.clone(),
        }
    }
}

fn parse_declarations(declarations: &Vec<(String, Vec<String>)>) -> HashMap<String, Declaration> {
    declarations
        .iter()
        .flat_map(|(entity, names)| {
            names
                .iter()
                .map(|name| match entity.as_str() {
                    "stmt" => (name.to_string(), Declaration::Statement(name.to_string())),
                    "assign" => (name.to_string(), Declaration::Assign(name.to_string())),
                    "while" => (name.to_string(), Declaration::While(name.to_string())),
                    "if" => (name.to_string(), Declaration::If(name.to_string())),
                    _ => panic!("Unknown entity type: {}", entity),
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn map_parent(
    declarations: &HashMap<String, Declaration>,
    parent: &str,
    child: &str,
) -> ParentDeclaration {
    let parent = declarations.get(parent).cloned();
    let child = declarations.get(child).cloned();

    match parent {
        Some(parent) => match child {
            Some(child) => ParentDeclaration::from_declarations(parent, child),
            None => ParentDeclaration::from_parent(parent),
        },
        None => match child {
            Some(child) => ParentDeclaration::from_child(child),
            None => ParentDeclaration::any(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::single_statement_declaration(
        vec![("stmt".to_string(), vec!["s".to_string()])],
        HashMap::from([("s".to_string(), Declaration::Statement("s".to_string()))]),
    )]
    #[case::multiple_statement_declaration(
        vec![
            ("stmt".to_string(), vec!["s".to_string()]),
            ("assign".to_string(), vec!["a".to_string(), "a1".to_string()]),
        ],
        HashMap::from([
            ("s".to_string(), Declaration::Statement("s".to_string())),
            ("a".to_string(), Declaration::Assign("a".to_string())),
            ("a1".to_string(), Declaration::Assign("a1".to_string())),
        ]),
    )]
    #[case::empty_declaration(
        vec![],
        HashMap::new(),
    )]
    fn test_parse_declarations(
        #[case] declarations: Vec<(String, Vec<String>)>,
        #[case] expected: HashMap<String, Declaration>,
    ) {
        let actual = parse_declarations(&declarations);
        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::single_declaration(
        QueryBuilderImpl {
            declarations: vec![("stmt".to_string(), vec!["s".to_string()])],
            result_type: ResultType::Boolean,
            follows: vec![],
            parent: vec![],
        },
        Query {
            declarations: HashMap::from([("s".to_string(), Declaration::Statement("s".to_string()))]),
            queries: vec![],
            result_type: ResultType::Boolean,
            pkb_context: Rc::new(Default::default()),}
    )]
    #[case::multiple_declaration(
        QueryBuilderImpl {
            declarations: vec![
                ("stmt".to_string(), vec!["s".to_string()]),
                ("assign".to_string(), vec!["a".to_string(), "a1".to_string()]),
            ],
            result_type: ResultType::Boolean,
            follows: vec![],
            parent: vec![],
        },
        Query {
            declarations: HashMap::from([
                ("s".to_string(), Declaration::Statement("s".to_string())),
                ("a".to_string(), Declaration::Assign("a".to_string())),
                ("a1".to_string(), Declaration::Assign("a1".to_string())),
            ]),
            queries: vec![],
            result_type: ResultType::Boolean,
            pkb_context: Rc::new(Default::default()),}
    )]
    #[case::parent_with_select(
        QueryBuilderImpl {
            declarations: vec![("stmt".to_string(), vec!["s".to_string(), "s1".to_string()])],
            result_type: ResultType::Single("s".to_string()),
            follows: vec![],
            parent: vec![("s".to_string(), "s1".to_string())],
        },
        Query {
            declarations: HashMap::from([("s".to_string(), Declaration::Statement("s".to_string())), ("s1".to_string(), Declaration::Statement("s1".to_string()))]),
            queries: vec![Box::new(ParentDeclaration::from_declarations(Declaration::Statement("s".to_string()), Declaration::Statement("s1".to_string())))],
            result_type: ResultType::Single("s".to_string()),
            pkb_context: Rc::new(Default::default()),}
    )]
    #[case::parent_with_underscore(
        QueryBuilderImpl {
            declarations: vec![("stmt".to_string(), vec!["s".to_string()])],
            result_type: ResultType::Single("s".to_string()),
            follows: vec![],
            parent: vec![("s".to_string(), "_".to_string())],
        },
        Query {
            declarations: HashMap::from([("s".to_string(), Declaration::Statement("s".to_string()))]),
            queries: vec![Box::new(ParentDeclaration::from_parent(Declaration::Statement("s".to_string())))],
            result_type: ResultType::Single("s".to_string()),
            pkb_context: Rc::new(Default::default()),}
    )]
    fn test_build(#[case] builder: QueryBuilderImpl, #[case] expected: Query) {
        let actual = builder.build(Rc::new(PkbContext::default()));
        assert_eq!(actual, expected);
    }
}
