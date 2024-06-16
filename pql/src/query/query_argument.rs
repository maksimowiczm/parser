use pkb::pkb_context::{PkbContext, PkbStatement};

#[derive(Clone, Debug, PartialEq)]
pub enum Argument {
    Any,
    Number(u32),
    String(String),
    Both(u32, String),
    Declaration(Declaration),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Declaration {
    Statement(String),
    Assign(String),
    While(String),
    If(String),
}

impl Declaration {
    pub fn extract_from_context(&self, context: &PkbContext) -> Vec<PkbStatement> {
        match self {
            Declaration::Statement(_) => context.get_statements(),
            Declaration::Assign(_) => context.get_assigns(),
            Declaration::While(_) => context.get_whiles(),
            Declaration::If(_) => context.get_ifs(),
        }
    }
}

impl Argument {
    pub fn transform(pkb: &PkbStatement) -> Argument {
        match pkb {
            PkbStatement::Assign(line) => Argument::Number(*line),
            PkbStatement::Call(line, name) => Argument::Both(*line, name.clone()),
            PkbStatement::If(line) => Argument::Number(*line),
            PkbStatement::While(line) => Argument::Number(*line),
        }
    }
}
