use crate::query::query_argument::{Argument, Declaration};
use crate::query::query_declaration::QueryDeclaration;
use pkb::pkb_context::PkbContext;
use std::marker::PhantomData;

#[derive(Debug, Clone, PartialEq)]
pub struct Declared;
#[derive(Debug, Clone, PartialEq)]
pub struct Executable;

#[derive(Debug, Clone, PartialEq)]
pub struct ParentDeclaration<State = Declared> {
    parent: Argument,
    child: Argument,
    state: PhantomData<State>,
}

impl ParentDeclaration<Declared> {
    pub fn from_declarations(parent: Declaration, child: Declaration) -> Self {
        ParentDeclaration {
            parent: Argument::Declaration(parent),
            child: Argument::Declaration(child),
            state: PhantomData,
        }
    }

    pub fn from_parent(parent: Declaration) -> Self {
        ParentDeclaration {
            parent: Argument::Declaration(parent),
            child: Argument::Any,
            state: PhantomData,
        }
    }

    pub fn from_child(child: Declaration) -> Self {
        ParentDeclaration {
            parent: Argument::Any,
            child: Argument::Declaration(child),
            state: PhantomData,
        }
    }

    pub fn any() -> Self {
        ParentDeclaration {
            parent: Argument::Any,
            child: Argument::Any,
            state: PhantomData,
        }
    }

    pub fn from_arguments(parent: Argument, child: Argument) -> Self {
        ParentDeclaration {
            parent,
            child,
            state: PhantomData,
        }
    }
}

#[allow(unused_variables)]
impl QueryDeclaration for ParentDeclaration<Declared> {
    fn set_argument(&mut self, argument: Declaration, value: Argument) {
        let is_match = |arg: &Argument| {
            if let Argument::Declaration(declaration) = arg {
                declaration == &argument
            } else {
                false
            }
        };

        if is_match(&self.parent) {
            self.parent = value;
        } else if is_match(&self.child) {
            self.child = value;
        }
    }

    fn is_executable(&self) -> bool {
        match (&self.parent, &self.child) {
            (Argument::Declaration(_), _) => false,
            (_, Argument::Declaration(_)) => false,
            _ => true,
        }
    }

    fn execute(&self, context: &PkbContext) -> bool {
        match (&self.parent, &self.child) {
            (Argument::Number(parent), Argument::Number(child)) => {
                context.parent.is_parent(*parent, *child)
            }
            (Argument::Number(parent), Argument::Any) => context.parent.has_child(*parent),
            (Argument::Any, Argument::Number(child)) => context.parent.has_parent(*child),
            (Argument::Any, Argument::Any) => context.parent.any(),
            _ => panic!("Invalid arguments"),
        }
    }
}
