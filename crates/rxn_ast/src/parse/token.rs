use crate::expr::UnaryOp;

/// A type  of token present within Rxn source code.
///
/// Rxn also has some soft keywords which are not present below:
/// * `ref`
/// * `async`
/// * `await`
/// * `abstract`
/// * `super`
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u16)]
pub enum Token {
    // Language features
    /// An identifier, which consists of one alphabetic character, followed by zero or more
    /// alphanumeric characters or underscores.
    ///
    /// There is also [Token::UnderSym], which is often, but not always, used in place of an
    /// identifier when it is not needed.
    Ident,

    // Literals
    /// A literal representing a 8-bit unsigned integer, e.g. "34u8", "235u8".
    U8Lit,
    /// A literal representing a 16-bit unsigned integer, e.g. "953u16", "2534u16".
    U16Lit,
    /// A literal representing a 32-bit unsigned integer.
    U32Lit,
    /// A literal representing a 64-bit unsigned integer.
    U64Lit,
    /// A literal representing a 128-bit unsigned integer.
    U128Lit,
    /// A literal representing a pointer-sized unsigned integer, e.g. "2394u", "16763u". This
    /// is the default and preferred integer type when working with ordinary unsigned integers.
    UintLit,
    /// A literal representing a 8-bit signed integer, e.g. "-105i8", "37i8".
    I8Lit,
    /// A literal representing a 16-bit signed integer, e.g. "953i16", "-2534i16".
    I16Lit,
    /// A literal representing a 32-bit signed integer.
    I32Lit,
    /// A literal representing a 64-bit signed integer.
    I64Lit,
    /// A literal representing a 128-bit signed integer.
    I128Lit,
    /// A literal representing a pointer-sized signed integer, e.g. "2394", "-16763". This
    /// is the default and preferred integer type when working with ordinary signed integers.
    IntLit,
    /// A literal representing an 32-bit floating-point, e.g. "30f32", "30.0_f32", "30.0",
    /// "-12.". This is the default and preferred floating-point for general purpose programming.
    F32Lit,
    /// A literal representing an 64-bit floating-point, e.g. "285f64", "-1.455224f64".
    F64Lit,
    /// A literal representing a boolean, either "false" or "true".
    BoolLit,
    /// A literal representing a UTF-16 character, which consists of the following surrounded by a pair of
    /// single quotes:
    /// * a UTF-16 code-point escape, e.g. "\u03C0", "\u1F642", "\u2656".
    /// * a UTF-16 character, e.g. 'a', '\', '6', '爱'.
    /// * a character escape.
    CharLit,
    /// A literal representing a UTF-8 string.
    StringLit,

    // Symbols
    /// ~
    TildeSym,
    /// `
    TickSym,
    /// !
    BangSym,
    /// @
    AtSym,
    /// \#
    HashSym,
    /// $
    DollarSym,
    /// %
    PercentSym,
    /// ^
    CaretSym,
    /// &
    AndSym,
    /// *
    StarSym,
    /// (
    LeftParen,
    /// )
    RightParen,
    /// -
    MinusSym,
    /// _
    UnderSym,
    /// +
    PlusSym,
    /// =
    EqSym,
    /// {
    LeftBrace,
    /// [
    LeftBracket,
    /// }
    RightBrace,
    /// ]
    RightBracket,
    /// |
    PipeSym,
    /// \
    BackSlashSym,
    /// :
    ColonSym,
    /// ;
    SemiSym,
    /// <
    LeftAngleSym,
    /// ,
    CommaSym,
    /// \>
    RightAngleSym,
    /// .
    DotSym,
    /// ?
    QuestSym,
    /// /
    ForwardSlashSym,

    // Keywords
    /// `as`
    AsKw,
    /// `break`
    BreakKw,
    /// `class`
    ClassKw,
    /// `const`
    ConstKw,
    /// `continue`
    ContinueKw,
    /// `else`
    ElseKw,
    /// `exit`
    ExitKw,
    /// `export`
    ExportKw,
    /// `fn`
    FnKw,
    /// `for`
    ForKw,
    /// `if`
    IfKw,
    /// `impl`
    ImpLKw,
    /// `in`
    InKw,
    /// `is`
    IsKw,
    /// `let`
    LetKw,
    /// `loc`
    LocKw,
    /// `loop`
    LoopKw,
    /// `macro`
    MacroKw,
    /// `match`
    MatchKw,
    /// `mod`
    ModKw,
    /// `mut`
    MutKw,
    /// `pub`
    PubKw,
    /// `struct`
    StructKw,
    /// `enum`
    EnumKw,
    /// `trait`
    TraitKw,
    /// `use`
    UseKw,
    /// `while`
    WhileKw,
}

impl Token {
    pub fn is_lit(self) -> bool {
        match self {
            Token::U8Lit
            | Token::U16Lit
            | Token::U32Lit
            | Token::U64Lit
            | Token::U128Lit
            | Token::UintLit
            | Token::I8Lit
            | Token::I16Lit
            | Token::I32Lit
            | Token::I64Lit
            | Token::I128Lit
            | Token::IntLit
            | Token::F32Lit
            | Token::F64Lit
            | Token::StringLit
            | Token::BoolLit
            | Token::CharLit => true,
            _ => false,
        }
    }

    pub fn is_unary(self) -> bool {
        match self {
            Token::BangSym | Token::MinusSym => true,
            _ => false,
        }
    }

    pub fn into_unary(self) -> Option<UnaryOp> {
        match self {
            Token::BangSym => Some(UnaryOp::Not),
            Token::MinusSym => Some(UnaryOp::Neg),
            _ => None,
        }
    }
}
