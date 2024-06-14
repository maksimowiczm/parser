#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum Argument {
    Statement(String),
    Assign(String),
    While(String),
    If(String),
    Concrete(ConcreteArgument),
    Any,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum ConcreteArgument {
    Number(i32),
    String(String),
}
