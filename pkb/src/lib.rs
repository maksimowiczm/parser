use simple::simple_parser::Node;

pub mod follows;
pub mod parent;
pub mod pkb_context;

pub trait AstAnalysis {
    fn analyze(&mut self, ast: &Node);
}
