use simple::simple_lexer::SimpleLexer;
use simple::simple_parser::SimpleParser;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let file = std::env::args().nth(1).expect("no file given");
    let program = std::fs::read_to_string(file)?;
    let lexer = SimpleLexer::default();
    let parser = SimpleParser::new(Box::new(lexer));
    let ast = parser.parse_program(&program)?;

    println!("{:?}", ast);

    Ok(())
}
