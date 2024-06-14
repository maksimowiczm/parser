use crate::query::query_argument::Argument;
use crate::query::query_declaration::QueryDeclaration;
use crate::query::Query;
#[cfg(test)]
use mockall::automock;
use std::collections::HashMap;

#[cfg_attr(test, automock)]
pub trait QueryBuilder {
    fn add_declaration(&mut self, declaration: (String, Vec<String>));
    fn set_result(&mut self, result: ResultType);
    fn add_follows(&mut self, predecessor: String, follower: String);
    fn add_parent(&mut self, parent: String, child: String);
    fn build(&self) -> Query;
}

#[derive(Default, Clone)]
#[cfg_attr(test, derive(Debug, PartialEq))]
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

    fn build(&self) -> Query {
        let declarations = parse_declarations(&self.declarations);
        let mut with_any = declarations.clone();
        with_any.insert("_".to_string(), Argument::Any);

        let parent = self.parent.iter().map(|(parent, child)| {
            QueryDeclaration::Parent(
                with_any.get(parent).unwrap().clone(),
                with_any.get(child).unwrap().clone(),
            )
        });

        let follows = self.follows.iter().map(|(predecessor, follower)| {
            QueryDeclaration::Follows(
                with_any.get(predecessor).unwrap().clone(),
                with_any.get(follower).unwrap().clone(),
            )
        });

        let queries = parent.chain(follows).collect();
        Query {
            declarations,
            queries,
            result_type: self.result_type.clone(),
        }
    }
}

fn parse_declarations(declarations: &Vec<(String, Vec<String>)>) -> HashMap<String, Argument> {
    declarations
        .iter()
        .flat_map(|(entity, names)| {
            names
                .iter()
                .map(|name| match entity.as_str() {
                    "stmt" => (name.to_string(), Argument::Statement(name.to_string())),
                    "assign" => (name.to_string(), Argument::Assign(name.to_string())),
                    "while" => (name.to_string(), Argument::While(name.to_string())),
                    "if" => (name.to_string(), Argument::If(name.to_string())),
                    _ => panic!("Unknown entity type: {}", entity),
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::single_statement_declaration(
        vec![("stmt".to_string(), vec!["s".to_string()])],
        HashMap::from([("s".to_string(), Argument::Statement("s".to_string()))]),
    )]
    #[case::multiple_statement_declaration(
        vec![
            ("stmt".to_string(), vec!["s".to_string()]),
            ("assign".to_string(), vec!["a".to_string(), "a1".to_string()]),
        ],
        HashMap::from([
            ("s".to_string(), Argument::Statement("s".to_string())),
            ("a".to_string(), Argument::Assign("a".to_string())),
            ("a1".to_string(), Argument::Assign("a1".to_string())),
        ]),
    )]
    #[case::empty_declaration(
        vec![],
        HashMap::new(),
    )]
    fn test_parse_declarations(
        #[case] declarations: Vec<(String, Vec<String>)>,
        #[case] expected: HashMap<String, Argument>,
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
            declarations: HashMap::from([("s".to_string(), Argument::Statement("s".to_string()))]),
            queries: vec![],
            result_type: ResultType::Boolean,
        }
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
                ("s".to_string(), Argument::Statement("s".to_string())),
                ("a".to_string(), Argument::Assign("a".to_string())),
                ("a1".to_string(), Argument::Assign("a1".to_string())),
            ]),
            queries: vec![],
            result_type: ResultType::Boolean,
        }
    )]
    #[case::parent_with_select(
        QueryBuilderImpl {
            declarations: vec![("stmt".to_string(), vec!["s".to_string(), "s1".to_string()])],
            result_type: ResultType::Single("s".to_string()),
            follows: vec![],
            parent: vec![("s".to_string(), "s1".to_string())],
        },
        Query {
            declarations: HashMap::from([("s".to_string(), Argument::Statement("s".to_string())), ("s1".to_string(), Argument::Statement("s1".to_string()))]),
            queries: vec![QueryDeclaration::Parent(Argument::Statement("s".to_string()), Argument::Statement("s1".to_string()))],
            result_type: ResultType::Single("s".to_string()),
        }
    )]
    #[case::parent_with_underscore(
        QueryBuilderImpl {
            declarations: vec![("stmt".to_string(), vec!["s".to_string()])],
            result_type: ResultType::Single("s".to_string()),
            follows: vec![],
            parent: vec![("s".to_string(), "_".to_string())],
        },
        Query {
            declarations: HashMap::from([("s".to_string(), Argument::Statement("s".to_string()))]),
            queries: vec![QueryDeclaration::Parent(Argument::Statement("s".to_string()), Argument::Any)],
            result_type: ResultType::Single("s".to_string()),
        }
    )]
    fn test_build(#[case] builder: QueryBuilderImpl, #[case] expected: Query) {
        let actual = builder.build();
        assert_eq!(actual, expected);
    }
}
