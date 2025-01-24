use crate::parse::error::{Construct, Expected, ParseError};
use crate::parse::parser::Parser;
use crate::parse::token::Token;
use crate::parse::Parse;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Block(Box<BlockExpr>),
    Call(Box<CallExpr>),
    Field(Box<FieldExpr>),
    Ident(IdentExpr),
    Index(Box<IndexExpr>),
    List(Box<ListExpr>),
    Lit(LitExpr),
    Unary(Box<UnaryExpr>),
}

impl Parse for Expr {
    fn is_suggested(_: &mut Parser<'_>) -> bool {
        true
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        parse_expr_logical_or_phase(parser)
    }
}

fn parse_expr_logical_or_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_logical_and_phase(parser).unwrap();
    while parser.matches_next(Token::PipeSym, Token::PipeSym) {
        let rhs = parse_expr_logical_and_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr {
            lhs: expr,
            op: BinaryOp::Or,
            rhs,
        }));
    }
    Ok(expr)
}

fn parse_expr_logical_and_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_bitwise_or_phase(parser).unwrap();

    while parser.matches(Token::AndSym, Token::AndSym) {
        let rhs = parse_expr_bitwise_or_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr {
            lhs: expr,
            op: BinaryOp::And,
            rhs,
        }))
    }

    Ok(expr)
}

fn parse_expr_bitwise_or_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_bitwise_xor_phase(parser).unwrap();

    while parser.matches_next_first(Token::PipeSym) {
        let rhs = parse_expr_bitwise_xor_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr {
            lhs: expr,
            op: BinaryOp::BitOr,
            rhs,
        }))
    }

    Ok(expr)
}

fn parse_expr_bitwise_xor_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_bitwise_and_phase(parser).unwrap();

    while parser.matches_next_first(Token::CaretSym) {
        let rhs = parse_expr_equality_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr {
            lhs: expr,
            op: BinaryOp::BitXor,
            rhs,
        }))
    }

    Ok(expr)
}

fn parse_expr_bitwise_and_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_equality_phase(parser).unwrap();

    while parser.matches_next_first(Token::AndSym) {
        let rhs = parse_expr_equality_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr {
            lhs: expr,
            op: BinaryOp::BitAnd,
            rhs,
        }))
    }

    Ok(expr)
}

fn parse_expr_equality_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_relational_phase(parser).unwrap();

    loop {
        let op;
        match parser.peek() {
            Ok(((Token::EqSym, _), (Token::EqSym, _))) => op = BinaryOp::Eq,
            Ok(((Token::BangSym, _), (Token::EqSym, _))) => op = BinaryOp::Neq,
            _ => break,
        }

        parser.next().unwrap();
        let rhs = parse_expr_relational_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }));
    }

    Ok(expr)
}

fn parse_expr_relational_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_bit_shift_phase(parser).unwrap();

    loop {
        let op;
        match parser.peek() {
            Ok(((Token::LeftAngleSym, _), (Token::EqSym, _))) => op = BinaryOp::Lte,
            Ok(((Token::RightAngleSym, _), (Token::EqSym, _))) => op = BinaryOp::Gte,
            Ok(((Token::LeftAngleSym, _), (x, _))) if x != Token::LeftAngleSym => op = BinaryOp::Lt,
            Ok(((Token::RightAngleSym, _), (x, _))) if x != Token::RightAngleSym => {
                op = BinaryOp::Gt
            }
            _ => break,
        }

        parser.next().unwrap();
        let rhs = parse_expr_bit_shift_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }));
    }

    Ok(expr)
}

fn parse_expr_bit_shift_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_add_sub_phase(parser).unwrap();

    loop {
        let op;
        match parser.peek() {
            Ok(((Token::LeftAngleSym, _), (Token::LeftAngleSym, _))) => op = BinaryOp::Shl,
            Ok(((Token::RightAngleSym, _), (Token::RightAngleSym, _))) => op = BinaryOp::Shr,
            _ => break,
        }

        parser.next().unwrap();
        let rhs = parse_expr_add_sub_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }));
    }

    Ok(expr)
}

fn parse_expr_add_sub_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_mul_div_rem_phase(parser).unwrap();

    loop {
        let op;
        match parser.peek_first() {
            Ok((Token::PlusSym, _)) => op = BinaryOp::Add,
            Ok((Token::MinusSym, _)) => op = BinaryOp::Sub,
            _ => break,
        }

        parser.next().unwrap();
        let rhs = parse_expr_mul_div_rem_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }));
    }

    Ok(expr)
}

fn parse_expr_mul_div_rem_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr = parse_expr_unary_phase(parser).unwrap();

    loop {
        let op;
        match parser.peek_first() {
            Ok((Token::StarSym, _)) => op = BinaryOp::Mul,
            Ok((Token::ForwardSlashSym, _)) => op = BinaryOp::Div,
            Ok((Token::PercentSym, _)) => op = BinaryOp::Rem,
            _ => break,
        }

        parser.next().unwrap();
        let rhs = parse_expr_unary_phase(parser).unwrap();
        expr = Expr::Binary(Box::new(BinaryExpr { lhs: expr, op, rhs }));
    }

    Ok(expr)
}

fn parse_expr_unary_phase(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut ops = vec![];
    while let Some(op) = parser.next_filter(Token::into_unary).unwrap() {
        ops.push(op);
    }
    let mut expr = parse_base_expr(parser).unwrap();
    for &op in ops.iter().rev() {
        expr = Expr::Unary(Box::new(UnaryExpr { op, expr }));
    }
    Ok(expr)
}

fn parse_base_expr(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    let mut expr;
    if parser.matches_first(Token::LeftBrace) {
        let mut exprs = vec![];
        while !parser.matches_next_first(Token::RightBrace) {
            exprs.push(Expr::parse(parser).unwrap());
            parser.expect(&[Token::SemiSym]).unwrap();
        }
        expr = Expr::Block(Box::new(BlockExpr { exprs }));
    } else if parser.matches_first(Token::LeftBracket) {
        let mut exprs = vec![];
        if !parser.matches_next_first(Token::RightBracket) {
            loop {
                exprs.push(Expr::parse(parser).unwrap());

                match parser.next().unwrap().0 {
                    Token::CommaSym => continue,
                    Token::RightBracket => break,
                    x => {
                        return Err(ParseError::Unexpected {
                            given: x,
                            expected: Expected::Tokens(vec![Token::CommaSym, Token::RightBracket]),
                        });
                    }
                }
            }
        }
        expr = Expr::List(Box::new(ListExpr { vec: exprs }));
    } else if parser.matches_next_first(Token::LeftParen) {
        expr = Expr::parse(parser).unwrap();
        parser.expect(&[Token::RightParen]).unwrap();
    } else {
        expr = parse_token_expr(parser).unwrap();
    }

    loop {
        match parser.peek_first() {
            Ok((Token::DotSym, _)) => {
                parser.next().unwrap();
                let ident = parser.next_ident().unwrap();
                expr = Expr::Field(Box::new(FieldExpr {
                    expr,
                    field: ident.to_owned(),
                }));
            }
            Ok((Token::LeftBracket, _)) => {
                parser.next().unwrap();
                let index = Expr::parse(parser).unwrap();
                parser.expect(&[Token::RightBracket]).unwrap();
                expr = Expr::Index(Box::new(IndexExpr { expr, index }));
            }
            Ok((Token::LeftParen, _)) => {
                parser.next().unwrap();
                let mut args = vec![];
                if parser.matches_first(Token::LeftParen) {
                    loop {
                        args.push(Expr::parse(parser).unwrap());

                        match parser.next().unwrap().0 {
                            Token::CommaSym => continue,
                            Token::RightParen => break,
                            x => {
                                return Err(ParseError::Unexpected {
                                    given: x,
                                    expected: Expected::Tokens(vec![
                                        Token::CommaSym,
                                        Token::RightParen,
                                    ]),
                                });
                            }
                        }
                    }
                }
                expr = Expr::Call(Box::new(CallExpr { expr, args }));
            }
            _ => break,
        }
    }

    Ok(expr)
}

fn parse_token_expr(parser: &mut Parser<'_>) -> Result<Expr, ParseError> {
    if LitExpr::is_suggested(parser) {
        return LitExpr::parse(parser).map(Expr::Lit);
    }
    parser.next().unwrap();

    Ok(Expr::Ident(IdentExpr(parser.lexer.slice().to_owned())))
}

impl Parse for LitExpr {
    fn is_suggested(parser: &mut Parser<'_>) -> bool {
        parser
            .peek_first()
            .map(|(t, _)| t.is_lit())
            .unwrap_or(false)
    }

    fn parse(parser: &mut Parser) -> Result<Self, ParseError> {
        let (token, range) = parser.next().unwrap();
        let slice = parser.slice_at(range);
        match token {
            Token::U8Lit => Ok(LitExpr::U8(u8::from_str(&slice[0..slice.len() - 2]).unwrap())),
            Token::U16Lit => Ok(LitExpr::U16(u16::from_str(&slice[0..slice.len() - 3]).unwrap())),
            Token::U32Lit => Ok(LitExpr::U32(u32::from_str(&slice[0..slice.len() - 3]).unwrap())),
            Token::U64Lit => Ok(LitExpr::U64(u64::from_str(&slice[0..slice.len() - 3]).unwrap())),
            Token::U128Lit => Ok(LitExpr::U128(u128::from_str(&slice[0..slice.len() - 4]).unwrap())),
            Token::UintLit => Ok(LitExpr::Uint(usize::from_str(&slice[0..slice.len() - 1]).unwrap())),
            Token::I8Lit => Ok(LitExpr::I8(i8::from_str(&slice[0..slice.len() - 2]).unwrap())),
            Token::I16Lit => Ok(LitExpr::I16(i16::from_str(&slice[0..slice.len() - 3]).unwrap())),
            Token::I32Lit => Ok(LitExpr::I32(i32::from_str(&slice[0..slice.len() - 3]).unwrap())),
            Token::I64Lit => Ok(LitExpr::I64(i64::from_str(&slice[0..slice.len() - 3]).unwrap())),
            Token::I128Lit => Ok(LitExpr::I128(i128::from_str(&slice[0..slice.len() - 4]).unwrap())),
            Token::IntLit => Ok(LitExpr::Int(isize::from_str(&slice).unwrap())),
            Token::F32Lit => Ok(LitExpr::F32(f32::from_str(slice).unwrap())),
            Token::F64Lit => Ok(LitExpr::F64(f64::from_str(&slice[0..slice.len() - 3]).unwrap())),
            Token::BoolLit => Ok(LitExpr::Bool(bool::from_str(&slice).unwrap())),
            Token::CharLit => Ok(LitExpr::Char(char::from_str(&slice[1..slice.len() - 1]).unwrap())),
            Token::StringLit => Ok(LitExpr::Str(slice[1..slice.len() - 1].to_owned())),
            given => Err(ParseError::Unexpected {
                given,
                expected: Expected::Construct(Construct::Lit),
            }),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct IdentExpr(String);

#[derive(Debug, Clone, PartialEq)]
pub enum LitExpr {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    Uint(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    Int(isize),
    F32(f32),
    F64(f64),
    Bool(bool),
    Char(char),
    Str(String),
}

impl Hash for LitExpr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LitExpr::U8(x) => state.write_u8(*x),
            LitExpr::U16(x) => state.write_u16(*x),
            LitExpr::U32(x) => state.write_u32(*x),
            LitExpr::U64(x) => state.write_u64(*x),
            LitExpr::U128(x) => state.write_u128(*x),
            LitExpr::Uint(x) => state.write_usize(*x),
            LitExpr::I8(x) => state.write_i8(*x),
            LitExpr::I16(x) => state.write_i16(*x),
            LitExpr::I32(x) => state.write_i32(*x),
            LitExpr::I64(x) => state.write_i64(*x),
            LitExpr::I128(x) => state.write_i128(*x),
            LitExpr::Int(x) => state.write_isize(*x),
            LitExpr::F32(x) => state.write_u32(unsafe { std::mem::transmute(*x) }),
            LitExpr::F64(x) => state.write_u64(unsafe { std::mem::transmute(*x) }),
            LitExpr::Bool(x) => bool::hash(x, state),
            LitExpr::Char(x) => char::hash(x, state),
            LitExpr::Str(x) => String::hash(x, state),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MapExpr {
    map: HashMap<Expr, Expr>,
}

impl PartialEq for MapExpr {
    fn eq(&self, other: &Self) -> bool {
        self.map.iter().eq(other.map.iter())
    }
}

impl Hash for MapExpr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for (k, v) in &self.map {
            k.hash(state);
            v.hash(state);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ListExpr {
    vec: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct FieldExpr {
    pub expr: Expr,
    pub field: String,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct IndexExpr {
    pub expr: Expr,
    pub index: Expr,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct CallExpr {
    pub expr: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct LoopExpr {
    pub block: BlockExpr,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BlockExpr {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct BinaryExpr {
    pub lhs: Expr,
    pub op: BinaryOp,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Expr,
}

#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[test]
fn test() {
    let expr = Expr::from_str("(1 + 4 * (2 + 6) % 2) - 5").unwrap();
    dbg!(expr);
}
