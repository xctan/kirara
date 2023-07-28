use std::ops::Range;

use nom::{Compare, InputTake, InputLength};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    IntegerConst,
    FloatConst,
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

macro_rules! impl_integer {
    ($int:ty) => {
        impl TryInto<$int> for TokenSpan<'_> {
            type Error = ();

            fn try_into(self) -> Result<$int, Self::Error> {
                if self.0.len() != 1 {
                    return Err(());
                }
                if !matches!(self.0[0].1, TokenType::IntegerConst) {
                    return Err(())
                }
                let s = self.0[0].0.replace("'", "");
                if s.starts_with("0x") || s.starts_with("0X") {
                    <$int>::from_str_radix(&s[2..], 16).map_err(|_| ())
                } else if s.starts_with('0') {
                    <$int>::from_str_radix(&s, 8).map_err(|_| ())
                } else {
                    s.parse().map_err(|_| ())
                }
            }
        }
    }
}

impl_integer!(i64);

impl_integer!(i32);

macro_rules! impl_float {
    ($float:ty) => {
        impl TryInto<$float> for TokenSpan<'_> {
            type Error = ();

            fn try_into(self) -> Result<$float, Self::Error> {
                if self.0.len() != 1 {
                    return Err(());
                }
                if !matches!(self.0[0].1, TokenType::FloatConst) {
                    return Err(())
                }
                let s = self.0[0].0.replace("'", "");
                s.parse().map_err(|_| ())
            }
        }
    }
}

impl_float!(f32);

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