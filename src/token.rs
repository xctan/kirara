use std::ops::Range;

use nom::{Compare, InputTake, InputLength};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    IntegerConst,
    Keyword,
    Identifier,
    Punctuation,
}

#[derive(Debug, Copy, Clone, Eq)]
pub struct Token<'a>(pub &'a str, pub TokenType);

impl PartialEq for Token<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1 && *self.0 == *other.0
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TokenSpan<'a>(pub &'a [Token<'a>]);

impl Compare<TokenType> for TokenSpan<'_> {
    fn compare(&self, t: TokenType) -> nom::CompareResult {
        if self.0.is_empty() {
            nom::CompareResult::Incomplete
        } else if self.0[0].1 == t {
            nom::CompareResult::Ok
        } else {
            nom::CompareResult::Error
        }
    }

    fn compare_no_case(&self, t: TokenType) -> nom::CompareResult {
        self.compare(t)
    }
}

impl Compare<Token<'_>> for TokenSpan<'_> {
    fn compare(&self, t: Token) -> nom::CompareResult {
        if self.0.is_empty() {
            nom::CompareResult::Incomplete
        } else if self.0[0] == t {
            nom::CompareResult::Ok
        } else {
            nom::CompareResult::Error
        }
    }

    fn compare_no_case(&self, t: Token) -> nom::CompareResult {
        self.compare(t)
    }
}

impl<'a> InputTake for TokenSpan<'a> {
    fn take(&self, count: usize) -> Self {
        TokenSpan(&self.0[..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.0.split_at(count);
        (TokenSpan(suffix), TokenSpan(prefix))
    }
}

impl InputLength for TokenType {
    fn input_len(&self) -> usize { 1 }
}

impl InputLength for Token<'_> {
    fn input_len(&self) -> usize { 1 }
}

impl InputLength for TokenSpan<'_> {
    fn input_len(&self) -> usize { self.0.len() }
}

impl<'a> From<&'a [Token<'a>]> for TokenSpan<'a> {
    fn from(tokens: &'a [Token<'a>]) -> Self {
        TokenSpan(tokens)
    }
}

impl<'a> From<&'a std::vec::Vec<Token<'_>>> for TokenSpan<'a> {
    fn from(tokens: &'a std::vec::Vec<Token<'_>>) -> Self {
        TokenSpan(tokens.as_slice())
    }
}

// useful parsers for a single token

impl TryInto<i64> for TokenSpan<'_> {
    type Error = ();

    fn try_into(self) -> Result<i64, Self::Error> {
        if self.0.len() != 1 {
            return Err(());
        }
        if !matches!(self.0[0].1, TokenType::IntegerConst) {
            return Err(())
        }
        let s = self.0[0].0;
        if s.starts_with("0x") || s.starts_with("0X") {
            i64::from_str_radix(&s[2..], 16).map_err(|_| ())
        } else if s.starts_with('0') {
            i64::from_str_radix(s, 8).map_err(|_| ())
        } else {
            s.parse().map_err(|_| ())
        }
    }
}

impl TryInto<i32> for TokenSpan<'_> {
    type Error = ();

    fn try_into(self) -> Result<i32, Self::Error> {
        if self.0.len() != 1 {
            return Err(());
        }
        if !matches!(self.0[0].1, TokenType::IntegerConst) {
            return Err(())
        }
        let s = self.0[0].0;
        if s.starts_with("0x") || s.starts_with("0X") {
            i32::from_str_radix(&s[2..], 16).map_err(|_| ())
        } else if s.starts_with('0') {
            i32::from_str_radix(s, 8).map_err(|_| ())
        } else {
            s.parse().map_err(|_| ())
        }
    }
}

pub type TokenRange = Range<usize>;

impl TokenSpan<'_> {
    pub fn as_str(&self) -> &str {
        self.0[0].0
    }

    // return the *wild* ptr range on the source string
    pub fn as_range(&self) -> TokenRange {
        let start = self.0[0].0.as_ptr() as usize;
        let end = start + self.0[0].0.len();
        start..end
    }
}

pub fn range_between(a: &TokenRange, b: &TokenRange) -> TokenRange {
    a.start..b.end
}