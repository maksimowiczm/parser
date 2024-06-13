use crate::AstAnalysis;
use simple::simple_parser::Node;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Parent {
    /// parent relation map from child to parent
    parent: HashMap<u32, u32>,
}

impl AstAnalysis for Parent {
    fn analyze(&mut self, ast: &Node) {
        match ast {
            Node::Program { procedures } => {
                for procedure in procedures {
                    self.analyze(procedure);
                }
            }
            Node::Procedure { body, .. } => {
                self.analyze(body);
            }
            Node::StatementList { statements } => {
                for statement in statements {
                    self.analyze(statement);
                }
            }
            Node::While {
                line, statements, ..
            } => {
                self.set_parent(statements, *line).unwrap();
            }
            Node::If {
                line,
                if_statements,
                else_statements,
                ..
            } => {
                self.set_parent(if_statements, *line).unwrap();
                self.set_parent(else_statements, *line).unwrap();
            }
            Node::Call { .. }
            | Node::Expression { .. }
            | Node::Reference { .. }
            | Node::Constant { .. }
            | Node::Assign { .. } => {}
        }
    }
}

#[derive(Debug)]
enum ParentError {
    InvalidNode,
}

impl Parent {
    pub fn new() -> Parent {
        Parent {
            parent: HashMap::new(),
        }
    }

    fn set_parent(&mut self, ast: &Node, parent_line: u32) -> Result<(), ParentError> {
        if let Node::StatementList { statements } = ast {
            for statement in statements {
                match statement {
                    Node::Assign { line, .. } | Node::Call { line, .. } => {
                        self.parent.insert(*line, parent_line);
                    }
                    Node::While { line, .. } | Node::If { line, .. } => {
                        self.parent.insert(*line, parent_line);
                        self.analyze(ast)
                    }
                    _ => return Err(ParentError::InvalidNode),
                }
            }

            Ok(())
        } else {
            Err(ParentError::InvalidNode)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::AstAnalysis;
    use itertools::Itertools;
    use rstest::rstest;
    use simple::simple_parser::Node;

    #[rstest]
    #[case::no_parent_without_container(
        Node::Program {
            procedures: vec![Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList {
                    statements: vec![Node::Assign {
                        line: 1,
                        variable: "x".to_string(),
                        expression: Box::new(Node::Constant { value: 1 })
                    }]
                })
            }]
        },
        vec![]
    )]
    #[case::single_parent(
        Node::Program {
            procedures: vec![Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList {
                    statements: vec![Node::While {
                        line: 1,
                        variable: "x".to_string(),
                        statements: Box::new(Node::StatementList {
                            statements: vec![Node::Assign {
                                line: 2,
                                variable: "x".to_string(),
                                expression: Box::new(Node::Constant { value: 1 })
                            }]
                        })
                    }]
                })
            }]
        },
        vec![(2, 1)]
    )]
    #[case::nested_containers_1(
        Node::Program {
            procedures: vec![Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList {
                    statements: vec![Node::While {
                        line: 1,
                        variable: "x".to_string(),
                        statements: Box::new(Node::StatementList {
                            statements: vec![Node::If {
                                line: 2,
                                variable: "x".to_string(),
                                if_statements: Box::new(Node::StatementList {
                                    statements: vec![Node::Assign {
                                        line: 3,
                                        variable: "x".to_string(),
                                        expression: Box::new(Node::Constant { value: 1 })
                                    }]
                                }),
                                else_statements: Box::new(Node::StatementList {
                                    statements: vec![Node::Assign {
                                        line: 4,
                                        variable: "x".to_string(),
                                        expression: Box::new(Node::Constant { value: 1 })
                                    }]
                                })
                            }]
                        })
                    }]
                })
            }]
        },
        vec![(2, 1), (3, 2), (4, 2)]
    )]
    #[case::nested_containers_2(
        Node::Program {
            procedures: vec![Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList {
                    statements: vec![Node::If {
                        line: 1,
                        variable: "x".to_string(),
                        if_statements: Box::new(Node::StatementList {
                            statements: vec![Node::While {
                                line: 2,
                                variable: "x".to_string(),
                                statements: Box::new(Node::StatementList {
                                    statements: vec![Node::Assign {
                                        line: 3,
                                        variable: "x".to_string(),
                                        expression: Box::new(Node::Constant { value: 1 })
                                    }, Node::Assign {
                                        line: 4,
                                        variable: "x".to_string(),
                                        expression: Box::new(Node::Constant { value: 2 })
                                    }]
                                })
                            }, Node::Assign {
                                line: 5,
                                variable: "x".to_string(),
                                expression: Box::new(Node::Constant { value: 3 })
                            }]
                        }),
                        else_statements: Box::new(Node::StatementList {
                            statements: vec![Node::Assign {
                                line: 6,
                                variable: "x".to_string(),
                                expression: Box::new(Node::Constant { value: 4 })
                            }, Node::Assign {
                                line: 7,
                                variable: "x".to_string(),
                                expression: Box::new(Node::Constant { value: 5 })
                            }]
                        })
                    }, Node::While {
                        line: 8,
                        variable: "x".to_string(),
                        statements: Box::new(Node::StatementList {
                            statements: vec![Node::Assign {
                                line: 9,
                                variable: "x".to_string(),
                                expression: Box::new(Node::Constant { value: 6 })
                            }, Node::Assign {
                                line: 10,
                                variable: "x".to_string(),
                                expression: Box::new(Node::Constant { value: 7 })
                            }]
                        })
                    }]
                })
            }]
        },
        vec![(2,1),(3,2),(4,2),(5,1),(6,1),(7,1),(9,8),(10,8)]
    )]
    fn test_parent_analysis(#[case] ast: Node, #[case] expected_parent: Vec<(u32, u32)>) {
        let mut parent = super::Parent::new();
        parent.analyze(&ast);

        let collected = parent
            .parent
            .iter()
            .map(|(k, v)| (*k, *v))
            .sorted_by_key(|(k, _)| *k)
            .collect::<Vec<(u32, u32)>>();

        assert_eq!(collected, expected_parent);
    }
}
