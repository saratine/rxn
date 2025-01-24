use crate::parse::token::Token;
use std::ops::Range;
use std::str::CharIndices;

pub struct Lexer<'src> {
    pub(crate) src: &'src str,
    chars: CharIndices<'src>,
    peeked: Option<Option<IndexedChar>>,
    range: Range<usize>,
}

type IndexedChar = (usize, char);

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            chars: src.char_indices(),
            peeked: None,
            range: 0..0,
        }
    }

    fn peek(&mut self) -> Option<char> {
        if self.peeked.is_none() {
            self.peeked = Some(self.chars.next());
        }

        self.peeked.clone().unwrap().map(|(_, c)| c)
    }

    fn advance(&mut self) -> Option<char> {
        self.peeked
            .take()
            .unwrap_or_else(|| self.chars.next())
            .map(|(i, c)| {
                self.range.end = i + 1;
                c
            })
    }

    fn clear_whitespace(&mut self) -> Option<()> {
        while let Some(c) = self.peek() {
            if !c.is_whitespace() {
                break;
            }

            self.advance()?;
        }
        Some(())
    }

    fn expect_char(&mut self) -> Option<()> {
        Some(())
    }

    fn expect_char_lit(&mut self) -> Option<(Token, Range<usize>)> {
        self.range.start = self.range.end;
        self.expect('\'')?;
        self.expect_char()?;
        self.expect('\'')?;

        Some((Token::CharLit, self.range.clone()))
    }

    fn expect_alphanumeric_lit(&mut self) -> Option<(Token, Range<usize>)> {
        self.range.start = self.range.end;
        self.expect_where(|c| c.is_alphabetic())?;

        while let Some(c) = self.peek() {
            if !c.is_alphanumeric() {
                break;
            }

            self.advance()?;
        }

        Some((
            match self.slice() {
                "as" => Token::AsKw,
                "break" => Token::BreakKw,
                "class" => Token::ClassKw,
                "const" => Token::ConstKw,
                "continue" => Token::ContinueKw,
                "else" => Token::ElseKw,
                "exit" => Token::ExitKw,
                "export" => Token::ExportKw,
                "fn" => Token::FnKw,
                "for" => Token::ForKw,
                "if" => Token::IfKw,
                "impl" => Token::ImpLKw,
                "in" => Token::InKw,
                "is" => Token::IsKw,
                "let" => Token::LetKw,
                "loc" => Token::LocKw,
                "loop" => Token::LoopKw,
                "macro" => Token::MacroKw,
                "match" => Token::MatchKw,
                "mod" => Token::ModKw,
                "mut" => Token::MutKw,
                "pub" => Token::PubKw,
                "struct" => Token::StructKw,
                "enum" => Token::EnumKw,
                "trait" => Token::TraitKw,
                "use" => Token::UseKw,
                "while" => Token::WhileKw,
                _ => Token::Ident,
            },
            self.range.clone(),
        ))
    }

    fn expect_string_lit(&mut self) -> Option<(Token, Range<usize>)> {
        self.range.start = self.range.end;
        self.expect('"')?;
        while let Some(c) = self.peek() {
            if c == '"' {
                self.advance()?;
                break;
            }
            self.expect_char()?;
        }
        Some((Token::StringLit, self.range.clone()))
    }

    fn expect_numeric_lit(&mut self) -> Option<(Token, Range<usize>)> {
        self.range.start = self.range.end;
        self.expect_where(|c| c.is_numeric())?;

        while let Some(c) = self.peek() {
            if !c.is_numeric() {
                break;
            }
            self.advance()?;
        }

        if let Some('.') = self.peek() {
            self.advance()?;

            while let Some(c) = self.peek() {
                if !c.is_numeric() {
                    break;
                }
                self.advance()?;
            }

            return Some((Token::F32Lit, self.range.clone()));
        }

        Some((Token::IntLit, self.range.clone()))
    }

    fn expect(&mut self, char: char) -> Option<char> {
        if self.peek()? == char {
            return self.advance();
        }
        None
    }

    fn expect_where(&mut self, f: impl FnOnce(char) -> bool) -> Option<char> {
        if f(self.peek()?) {
            return self.advance();
        }
        None
    }

    pub fn next_token(&mut self) -> Option<(Token, Range<usize>)> {
        self.clear_whitespace()?;
        self.range.start = self.range.end;
        let token = match self.peek()? {
            '~' => Token::TildeSym,
            '`' => Token::TickSym,
            '!' => Token::BangSym,
            '@' => Token::AtSym,
            '#' => Token::HashSym,
            '$' => Token::DollarSym,
            '%' => Token::PercentSym,
            '^' => Token::CaretSym,
            '&' => Token::AndSym,
            '*' => Token::StarSym,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '-' => Token::MinusSym,
            '_' => Token::UnderSym,
            '+' => Token::PlusSym,
            '=' => Token::EqSym,
            '{' => Token::LeftBrace,
            '[' => Token::LeftBracket,
            '}' => Token::RightBrace,
            ']' => Token::RightBracket,
            '|' => Token::PipeSym,
            '\\' => Token::BackSlashSym,
            ':' => Token::ColonSym,
            ';' => Token::SemiSym,
            '<' => Token::LeftAngleSym,
            ',' => Token::CommaSym,
            '>' => Token::RightAngleSym,
            '.' => Token::DotSym,
            '?' => Token::QuestSym,
            '/' => Token::ForwardSlashSym,
            '\'' => return self.expect_char_lit(),
            '"' => return self.expect_string_lit(),
            c if c.is_alphabetic() => return self.expect_alphanumeric_lit(),
            c if c.is_numeric() => return self.expect_numeric_lit(),
            _ => return None,
        };
        self.advance();
        Some((token, self.range.clone()))
    }

    pub fn slice(&self) -> &'src str {
        &self.src[self.range.clone()]
    }
}

#[test]
fn test() {
    let source = "fn struct hello 3.5 * (4 + 8) 432 world";
    let mut lexer = Lexer::new(source);
    while let Some((token, span)) = lexer.next_token() {
        dbg!(token, &lexer.src[span]);
    }
}
