#![allow(unused)]
#![allow(non_snake_case)]
use core::fmt;
use uuid::Uuid;

//mod section;
mod constants;
mod utils;

// TODO: Relpace with known types
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Unknown;


#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Id(Uuid);

impl Id {
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&base62::encode(self.0.as_u128()))
    }
}

impl core::str::FromStr for Id {
    type Err = base62::DecodeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(Uuid::from_u128(base62::decode(s)?)))
    }
}


pub trait SkipEmpty {
    fn skip_empty(self) -> Self;
}
impl<T> SkipEmpty for Option<T> where T: IsEmpty {
    fn skip_empty(self) -> Self {
        self.filter(|s| !s.is_empty())
    }
}

pub trait IsEmpty {
    fn is_empty(&self) -> bool;
}
impl<'a, T> IsEmpty for &'a T where T: IsEmpty {
    fn is_empty(&self) -> bool {
        <T as IsEmpty>::is_empty(self)
    }
}
impl<'a, T> IsEmpty for &'a mut T where T: IsEmpty {
    fn is_empty(&self) -> bool {
        <T as IsEmpty>::is_empty(self)
    }
}
impl IsEmpty for str {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}
impl IsEmpty for String {
    fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }
}
impl<T> IsEmpty for &[T] {
    fn is_empty(&self) -> bool {
        (*self).is_empty()
    }
}
impl<T> IsEmpty for Vec<T> {
    fn is_empty(&self) -> bool {
        self.as_slice().is_empty()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bool(pub bool);

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl core::str::FromStr for Bool {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Self> {
        Ok(Self(s.parse::<bool>()?))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InputFormat {
    BibTeX,
    BibLaTeXML,
}

impl Default for InputFormat {
    fn default() -> Self {
        Self::BibTeX
    }
}

impl fmt::Display for InputFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BibTeX => f.write_str("bibtex"),
            Self::BibLaTeXML => f.write_str("biblatexml"),
        }
    }
}

impl core::str::FromStr for InputFormat {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Self> {
        match s {
            "bibtex" => Ok(Self::BibTeX),
            "biblatexml" => Ok(Self::BibLaTeXML),
            _ => Err(anyhow::anyhow!("Incorrect input format type")),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OutputFormat {
    Dot,
    BibTeX,
    BibLaTeXML,
    Bbl,
    BblXML,
}

impl Default for OutputFormat {
    fn default() -> Self {
        Self::Bbl
    }
}

impl fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dot => f.write_str("dot"),
            Self::BibTeX => f.write_str("bibtex"),
            Self::BibLaTeXML => f.write_str("biblatexml"),
            Self::Bbl => f.write_str("bbl"),
            Self::BblXML => f.write_str("bblxml"),
        }
    }
}

impl core::str::FromStr for OutputFormat {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Self> {
        match s {
            "dot" => Ok(Self::Dot),
            "bibtex" => Ok(Self::BibTeX),
            "biblatexml" => Ok(Self::BibLaTeXML),
            "bbl" => Ok(Self::Bbl),
            "bblxml" => Ok(Self::BblXML),
            _ => Err(anyhow::anyhow!("Incorrect output format type")),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OutputFieldCase {
    Upper,
    Lower,
    Title,
}

impl Default for OutputFieldCase {
    fn default() -> Self {
        Self::Upper
    }
}

impl fmt::Display for OutputFieldCase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Upper => f.write_str("upper"),
            Self::Lower => f.write_str("lower"),
            Self::Title => f.write_str("title"),
        }
    }
}

impl core::str::FromStr for OutputFieldCase {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Self> {
        match s {
            "upper" => Ok(Self::Upper),
            "lower" => Ok(Self::Lower),
            "title" => Ok(Self::Title),
            _ => Err(anyhow::anyhow!("Incorrect output field case")),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OutputIndent {
    Spaces(u32),
    Tabs(u32),
}

impl Default for OutputIndent {
    fn default() -> Self {
        Self::Spaces(2)
    }
}

impl fmt::Display for OutputIndent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Spaces(num) => write!(f, "{}", num),
            Self::Tabs(num) => write!(f, "{}t", num),
        }
    }
}

impl core::str::FromStr for OutputIndent {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Self> {
        if s.ends_with("t") {
            Ok(Self::Tabs(s[..s.len() - 1].parse::<u32>()?))
        } else {
            Ok(Self::Spaces(s.parse::<u32>()?))
        }
    }
}
