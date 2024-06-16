use pkb::follows::Follows;
use pkb::parent::Parent;
use pkb::pkb_context::PkbContext;
use pkb::AstAnalysis;
use pql::pql_lexer::PqlLexer;
use pql::pql_parser::PqlParser;
use pql::query::query_builder::QueryBuilderImpl;
use simple::simple_lexer::SimpleLexer;
use simple::simple_parser;
use simple_parser::SimpleParser;
use std::rc::Rc;
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::env::args().nth(1).expect("no file given");
    let program = std::fs::read_to_string(file)?;
    let lexer = SimpleLexer::default();
    let parser = SimpleParser::new(Box::new(lexer));
    let ast = parser.parse_program(&program)?;

    let mut follows = Follows::new();
    follows.analyze(&ast);
    let mut parent = Parent::new();
    parent.analyze(&ast);

    let pkb_context = PkbContext::new(ast, parent, follows);

    let pql_lexer = PqlLexer::default();
    let input =
        "stmt s; assign a, a1; Select a such that Parent(_, s) and Parent(_, _) and Parent(s, a) and Parent(2,3)";
    let input = "stmt s; assign a; Select a such that Parent(s,a)";
    // let input = "stmt s; assign a, a1; Select BOOLEAN such that Follows(_, s)";
    let mut parser = PqlParser::new(
        Box::new(pql_lexer),
        Box::new(QueryBuilderImpl::default()),
        Rc::new(pkb_context),
    );
    let out = parser.parse(input)?;

    out.execute();

    Ok(())
}
