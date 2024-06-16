pub mod parent_declaration;

use crate::query::query_argument::{Argument, Declaration};
use std::fmt::Debug;
use pkb::pkb_context::PkbContext;

pub trait QueryDeclaration
where
    Self: Debug,
{
    fn set_argument(&mut self, argument: Declaration, value: Argument);
    fn is_executable(&self) -> bool;
    fn execute(&self, context: &PkbContext) -> bool;
}

// impl QueryDeclaration {
//     pub fn set_argument(&self, argument: &String, value: &Argument) -> Self {
//         // match self {
//         //     QueryDeclaration::Parent(a, b) => {
//         //         let left = a.get_declaration();
//         //         let right = b.get_declaration();
//         //
//         //         if left == Some(argument.clone()) {
//         //             return QueryDeclaration::Parent(value.clone(), b.clone());
//         //         } else if right == Some(argument.clone()) {
//         //             return QueryDeclaration::Parent(a.clone(), value.clone());
//         //         }
//         //
//         //         panic!("Argument not found");
//         //     }
//         //     QueryDeclaration::Follows(a, b) => {
//         //         let left = a.get_declaration();
//         //         let right = b.get_declaration();
//         //
//         //         if left == Some(argument.clone()) {
//         //             return QueryDeclaration::Follows(value.clone(), b.clone());
//         //         } else if right == Some(argument.clone()) {
//         //             return QueryDeclaration::Follows(a.clone(), value.clone());
//         //         }
//         //
//         //         panic!("Argument not found");
//         //     }
//         // }
//         todo!()
//     }
//
//     pub fn has_argument(&self, declaration: &str) -> bool {
//         // match self {
//         // QueryDeclaration::Parent(a, b) | QueryDeclaration::Follows(a, b) => {
//         //     let left = a.get_declaration();
//         //     let right = b.get_declaration();
//         //
//         //     left == Some(declaration.to_string()) || right == Some(declaration.to_string())
//         // }
//         // }
//
//         todo!()
//     }
//
//     pub fn is_executable(&self) -> bool {
//         // match self {
//         //     QueryDeclaration::Parent(left, right) | QueryDeclaration::Follows(left, right) => {
//         //         match (left, right) {
//         //             (Argument::Concrete(_), Argument::Concrete(_)) => true,
//         //             _ => false,
//         //         }
//         //     }
//         // }
//
//         todo!()
//     }
//     pub fn execute(&self, context: &PkbContext) -> bool {
//         // match self {
//         //     QueryDeclaration::Parent(left, right) => match (left, right) {
//         //         _ => false,
//         //     },
//         //     _ => panic!(),
//         // }
//
//         todo!()
//     }
// }
