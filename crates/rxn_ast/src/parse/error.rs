use crate::parse::token::Token;
use std::char::ParseCharError;
use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};
use std::str::ParseBoolError;

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Unexpected end of file")]
    UnexpectedEof,
    #[error("Unexpected token: {given:?}, expected {expected}")]
    Unexpected { given: Token, expected: Expected },
    #[error(transparent)]
    ParseChar(#[from] ParseCharError),
    #[error(transparent)]
    ParseFloat(#[from] ParseFloatError),
    #[error(transparent)]
    ParseBool(#[from] ParseBoolError),
    #[error(transparent)]
    ParseInt(#[from] ParseIntError),
}

#[derive(Debug)]
pub enum Expected {
    Tokens(Vec<Token>),
    Construct(Construct),
}

impl Display for Expected {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tokens(tokens) => {
                write!(f, "one of: ")?;
                for (i, token) in tokens.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", token)?;
                }
                Ok(())
            }
            Self::Construct(construct) => construct.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum Construct {
    Lit,
}

impl Display for Construct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Construct::Lit => write!(f, "literal"),
        }
    }
}

pub type ParseResult<T> = Result<T, ParseError>;
