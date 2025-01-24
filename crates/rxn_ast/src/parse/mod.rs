pub mod error;
pub mod lexer;
pub mod parser;
pub mod token;

pub trait Parse: Sized + std::fmt::Debug {
    fn is_suggested(parser: &mut parser::Parser<'_>) -> bool;

    fn parse(parser: &mut parser::Parser) -> Result<Self, error::ParseError>;

    fn from_str(src: &str) -> Result<Self, error::ParseError> {
        let mut parser = parser::Parser::new(src);
        Self::parse(&mut parser)
    }
}
