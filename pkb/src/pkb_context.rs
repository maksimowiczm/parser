use crate::follows::Follows;
use crate::parent::Parent;
use simple::simple_parser::Node;

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum PkbStatement {
    Assign(u32),
    Call(u32, String),
    If(u32),
    While(u32),
}

#[derive(Default, Debug)]
pub struct PkbContext {
    statements: Vec<PkbStatement>,
    pub parent: Parent,
    pub follows: Follows,
}

#[cfg(test)]
impl PartialEq for PkbContext {
    fn eq(&self, other: &Self) -> bool {
        self.statements == other.statements
    }
}

impl PkbContext {
    pub fn new(ast: Node, parent: Parent, follows: Follows) -> PkbContext {
        let statements = map_statements(&ast);
        PkbContext {
            statements,
            parent,
            follows,
        }
    }
}

macro_rules! get_lines_for {
    ($name:ident, $variant:path) => {
        pub fn $name(&self) -> Vec<PkbStatement> {
            self.statements
                .iter()
                .filter_map(|statement| match statement {
                    $variant(..) => Some(statement),
                    _ => None,
                })
                .cloned()
                .collect()
        }
    };
}

// getters
impl PkbContext {
    pub fn get_statements(&self) -> Vec<PkbStatement> {
        self.statements.iter().cloned().collect()
    }

    get_lines_for!(get_assigns, PkbStatement::Assign);
    get_lines_for!(get_calls, PkbStatement::Call);
    get_lines_for!(get_ifs, PkbStatement::If);
    get_lines_for!(get_whiles, PkbStatement::While);
}

fn map_statements(ast: &Node) -> Vec<PkbStatement> {
    match ast {
        Node::Program { procedures } => procedures
            .into_iter()
            .flat_map(|procedure| map_statements(procedure))
            .collect(),
        Node::Procedure { body, .. } => map_statements(body),
        Node::StatementList { statements } => statements
            .iter()
            .flat_map(|statement| map_statements(statement))
            .collect(),
        Node::Assign { line, .. } => vec![PkbStatement::Assign(*line)],
        Node::Call { line, name, .. } => vec![PkbStatement::Call(*line, name.clone())],
        Node::While {
            line, statements, ..
        } => {
            let mut result = vec![PkbStatement::While(*line)];
            result.extend(map_statements(statements));
            result
        }
        Node::If {
            line,
            if_statements,
            else_statements,
            ..
        } => {
            let mut result = vec![PkbStatement::If(*line)];
            result.extend(map_statements(if_statements));
            result.extend(map_statements(else_statements));
            result
        }
        _ => panic!("unexpected ast node"),
    }
}
