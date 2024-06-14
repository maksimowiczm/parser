use crate::query::query_argument::Argument;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum QueryDeclaration {
    Parent(Argument, Argument),
    Follows(Argument, Argument),
}
