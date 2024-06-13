use crate::AstAnalysis;
use simple::simple_parser::Node;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Follows {
    /// follows relation map from follower to predecessor
    follows: HashMap<u32, u32>,
}

impl AstAnalysis for Follows {
    fn analyze(&mut self, ast: &Node) {
        match ast {
            Node::Program { procedures } => {
                for procedure in procedures {
                    self.analyze(procedure);
                }
            }
            Node::Procedure {
                body: statements, ..
            }
            | Node::While { statements, .. } => {
                self.set_follows(statements);
            }
            Node::If {
                if_statements,
                else_statements,
                ..
            } => {
                self.set_follows(if_statements);
                self.set_follows(else_statements);
            }
            Node::StatementList { .. }
            | Node::Assign { .. }
            | Node::Call { .. }
            | Node::Expression { .. }
            | Node::Reference { .. }
            | Node::Constant { .. } => {}
        }
    }
}

impl Follows {
    pub fn new() -> Self {
        Self {
            follows: HashMap::new(),
        }
    }

    pub fn is_follows(&self, predecessor: u32, follower: u32) -> bool {
        self.follows.get(&follower) == Some(&predecessor)
    }

    pub fn is_follows_transitive(&self, predecessor: u32, follower: u32) -> bool {
        let mut current = follower;
        while let Some(&next) = self.follows.get(&current) {
            if next == predecessor {
                return true;
            }
            current = next;
        }
        false
    }

    fn set_follows(&mut self, ast: &Node) {
        if let Node::StatementList { statements } = ast {
            let mut previous_line = None;

            for statement in statements {
                match statement {
                    Node::While { line, .. } | Node::If { line, .. } => {
                        if let Some(previous_line) = previous_line {
                            self.follows.insert(*line, previous_line);
                        }
                        previous_line = Some(*line);
                        self.analyze(statement);
                    }
                    Node::Assign { line, .. } | Node::Call { line, .. } => {
                        if let Some(previous_line) = previous_line {
                            self.follows.insert(*line, previous_line);
                        }
                        previous_line = Some(*line);
                    }
                    _ => {}
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use rstest::rstest;
    use simple::simple_parser::Node;

    #[rstest]
    #[case::empty_program(Node::Program { procedures: vec![] }, &[])]
    #[case::no_containers(
        Node::Program {
            procedures: vec![Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList { statements: vec![
                    Node::Assign { line: 1, variable: "x".to_string(), expression: Box::new(Node::Constant { value: 1 }) },
                    Node::Assign { line: 2, variable: "y".to_string(), expression: Box::new(Node::Constant { value: 2 }) },
                ] }),
            }]
        },
        &[(2, 1)]
    )]
    #[case::one_container(
        Node::Program {
            procedures: vec![Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList { statements: vec![
                    Node::While {
                        line: 1,
                        variable: "x".to_string(),
                        statements: Box::new(Node::StatementList { statements: vec![
                            Node::Assign { line: 2, variable: "x".to_string(), expression: Box::new(Node::Constant { value: 1 }) },
                            Node::Assign { line: 3, variable: "y".to_string(), expression: Box::new(Node::Constant { value: 2 }) },
                        ] }),
                    },
                ] }),
            }]
        },
        &[(3,2)]
    )]
    #[case::nested_container(
        Node::Program {
            procedures: vec![Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList { statements: vec![
                    Node::While {
                        line: 1,
                        variable: "x".to_string(),
                        statements: Box::new(Node::StatementList { statements: vec![
                            Node::Assign { line: 2, variable: "x".to_string(), expression: Box::new(Node::Constant { value: 1 }) },
                            Node::Assign { line: 3, variable: "y".to_string(), expression: Box::new(Node::Constant { value: 2 }) },
                            Node::If {
                                line: 4,
                                variable: "x".to_string(),
                                if_statements: Box::new(Node::StatementList { statements: vec![
                                    Node::Assign { line: 5, variable: "x".to_string(), expression: Box::new(Node::Constant { value: 3 }) },
                                    Node::Assign { line: 6, variable: "y".to_string(), expression: Box::new(Node::Constant { value: 4 }) },
                                ] }),
                                else_statements: Box::new(Node::StatementList { statements: vec![
                                    Node::Assign { line: 7, variable: "x".to_string(), expression: Box::new(Node::Constant { value: 5 }) },
                                    Node::Assign { line: 8, variable: "y".to_string(), expression: Box::new(Node::Constant { value: 6 }) },
                                ] }),
                            },
                        ] }),
                    },
                ] }),
            }]
        },
        &[(3,2),(4,3),(6,5),(8,7)]
    )]
    fn test_follows_analysis(#[case] ast: Node, #[case] expected_follows: &[(u32, u32)]) {
        let mut follows = Follows::new();
        follows.analyze(&ast);

        let follows = follows
            .follows
            .iter()
            .map(|(k, v)| (*k, *v))
            .sorted_by_key(|(k, _)| *k)
            .collect::<Vec<(u32, u32)>>();

        assert_eq!(follows, expected_follows);
    }

    #[rstest]
    #[case::empty_follows(&[], (1, 2), false)]
    #[case::follows(&[(2, 1)], (1, 2), true)]
    #[case::not_follows(&[(2, 1)], (2, 1), false)]
    #[case::follows_multiple(&[(2, 1), (3, 2)], (1, 2), true)]
    #[case::follows_multiple(&[(2, 1), (3, 2)], (1, 3), true)]
    #[case::follows_multiple(&[(2, 1), (3, 2)], (1, 4), false)]
    fn test_follows_transitive(
        #[case] follows: &[(u32, u32)],
        #[case] relation: (u32, u32),
        #[case] expected: bool,
    ) {
        let follows = Follows {
            follows: follows.iter().cloned().collect(),
        };

        let result = follows.is_follows_transitive(relation.0, relation.1);
        assert_eq!(result, expected);
    }
}
