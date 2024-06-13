use simple::simple_parser::Node;

pub mod parent;

pub trait AstAnalysis {
    fn analyze(&mut self, ast: &Node);
}
