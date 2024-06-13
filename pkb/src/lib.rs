use simple::simple_parser::Node;

pub mod parent;
pub mod follows;

pub trait AstAnalysis {
    fn analyze(&mut self, ast: &Node);
}
