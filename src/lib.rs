#![allow(unused)]
#![allow(non_snake_case)]
use core::fmt;
use uuid::Uuid;

mod element_builder;

mod annotation;
//mod section;
mod constants;
mod utils;

// TODO: Relpace with known types
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Unknown;

use std::borrow::Borrow;
use std::hash::Hash;
use std::collections::{hash_map, HashMap, HashSet};
use std::rc::Rc;
use std::sync::Arc;

mod mem;
use mem::{Ref, Wrapper};


pub struct BiSet<L: Eq + Hash, R: Eq + Hash> {
    lr: HashMap<Ref<L>, HashSet<Ref<R>>>,
    rl: HashMap<Ref<R>, HashSet<Ref<L>>>,
}

impl<L: Eq + Hash, R: Eq + Hash> BiSet<L, R> {
    pub fn new() -> Self {
        Self {
            lr: HashMap::new(),
            rl: HashMap::new(),
        }
    }
    pub fn insert(&mut self, left: impl Into<Arc<L>>, right: impl Into<Arc<R>>) {
        let left = Ref(left.into());
        let right = Ref(right.into());
        self.lr.entry(left.clone()).or_default().insert(right.clone());
        self.rl.entry(right).or_default().insert(left);
    }

    pub fn contains_left<Q>(&self, left: &Q) -> bool
    where
        L: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        self.lr.contains_key(Wrapper::wrap(left))
    }

    pub fn contains_right<Q>(&self, right: &Q) -> bool
    where
        R: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        self.rl.contains_key(Wrapper::wrap(right))
    }

    pub fn contains_pair<LQ, RQ>(&self, left: &LQ, right: &RQ) -> bool
    where
        L: Borrow<LQ>,
        R: Borrow<RQ>,
        LQ: Eq + Hash + ?Sized,
        RQ: Eq + Hash + ?Sized,
    {
        self.lr.get(Wrapper::wrap(left)).map(|s| s.contains(Wrapper::wrap(right))).unwrap_or(false)
    }

    pub fn iter_by_left<Q>(&self, left: &Q) -> impl Iterator<Item = &R>
    where
        L: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        OptIter::new(self.lr.get(Wrapper::wrap(left)).map(|e| e.iter().map(|l| &*l.0)))
    }

    pub fn iter_by_right<Q>(&self, right: &Q) -> impl Iterator<Item = &L>
    where
        R: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        OptIter::new(self.rl.get(Wrapper::wrap(right)).map(|e| e.iter().map(|r| &*r.0)))
    }
}

/// Iterates over optional iterator
pub struct OptIter<I>(Option<I>)
where
    I: Iterator;

impl<I> OptIter<I>
where
    I: Iterator,
{
    /// Create new optional iterator
    pub fn new(o: Option<I>) -> Self {
        Self(o)
    }
}

impl<'a, I> Iterator for OptIter<I>
where
    I: Iterator,
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.as_mut().and_then(I::next)
    }
}

/// Sugar
pub trait NestedMap<K1, K2, V> {
    fn contains_key2<Q1: ?Sized, Q2: ?Sized>(&self, k1: &Q1, k2: &Q2) -> bool where
        K1: Borrow<Q1>,
        Q1: Hash + Eq, 
        K2: Borrow<Q2>,
        Q2: Hash + Eq;
    fn get2<Q1: ?Sized, Q2: ?Sized>(&self, k1: &Q1, k2: &Q2) -> Option<&V> where
        K1: Borrow<Q1>,
        Q1: Hash + Eq, 
        K2: Borrow<Q2>,
        Q2: Hash + Eq;
    fn get_mut2<Q1: ?Sized, Q2: ?Sized>(&mut self, k1: &Q1, k2: &Q2) -> Option<&mut V> where
        K1: Borrow<Q1>,
        Q1: Hash + Eq, 
        K2: Borrow<Q2>,
        Q2: Hash + Eq;
}

impl<K1: Hash + Eq, K2: Hash + Eq, V> NestedMap<K1, K2, V> for HashMap<K1, HashMap<K2, V>> {
    fn contains_key2<Q1: ?Sized, Q2: ?Sized>(&self, k1: &Q1, k2: &Q2) -> bool where
        K1: Borrow<Q1>,
        Q1: Hash + Eq, 
        K2: Borrow<Q2>,
        Q2: Hash + Eq
    {
        self.get(k1).map(|h| h.contains_key(k2)).unwrap_or(false)
    }
    fn get2<Q1: ?Sized, Q2: ?Sized>(&self, k1: &Q1, k2: &Q2) -> Option<&V>
    where
        K1: Borrow<Q1>,
        Q1: Hash + Eq, 
        K2: Borrow<Q2>,
        Q2: Hash + Eq, 
    {
        self.get(k1).and_then(|h| h.get(k2))
    }
    fn get_mut2<Q1: ?Sized, Q2: ?Sized>(&mut self, k1: &Q1, k2: &Q2) -> Option<&mut V> where
        K1: Borrow<Q1>,
        Q1: Hash + Eq, 
        K2: Borrow<Q2>,
        Q2: Hash + Eq, 
    {
        self.get_mut(k1).and_then(|h| h.get_mut(k2))
    }
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Id(Uuid);

impl Id {
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl fmt::Debug for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&base62::encode(self.0.as_u128()))
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

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum FieldType {
    Field,
    List,
}

impl FieldType {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Field => "field",
            Self::List => "list",
        }
    }
}
impl fmt::Debug for FieldType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}
impl fmt::Display for FieldType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum DataType {
    Code,
    Date,
    Datepart,
    Entrykey,
    Integer,
    Key,
    Literal,
    Range,
    Verbatim,
    Uri,
    Keyword,
    Option,
    Name,

    Isbn,
    Issn,
    Ismn,
    Pattern,
    // ..
}

impl DataType {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Code => "code",
            Self::Date => "date",
            Self::Datepart => "datepart",
            Self::Entrykey => "entrykey",
            Self::Integer => "integer",
            Self::Key => "key",
            Self::Literal => "literal",
            Self::Range => "range",
            Self::Verbatim => "verbatim",
            Self::Uri => "uri",
            Self::Keyword => "keyword",
            Self::Option => "option",
            Self::Name => "name",
            Self::Isbn => "isbn",
            Self::Issn => "issn",
            Self::Ismn => "ismn",
            Self::Pattern => "pattern",
        }
    }
}
impl fmt::Debug for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}
impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Format {
    Xsv,
    Default,
}

impl Format {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Xsv => "xsv",
            Self::Default => "default",
        }
    }
}
impl fmt::Debug for Format {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}
impl fmt::Display for Format {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
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

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
pub enum InputFormat {
    #[default]
    BibTeX,
    BibLaTeXML,
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

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
pub enum OutputFormat {
    Dot,
    BibTeX,
    BibLaTeXML,
    #[default]
    Bbl,
    BblXML,
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

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
pub enum OutputFieldCase {
    #[default]
    Upper,
    Lower,
    Title,
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


#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    Left,
    Right,
}

impl fmt::Display for Side {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Left => f.write_str("left"),
            Self::Right => f.write_str("right"),
        }
    }
}

impl core::str::FromStr for Side {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> anyhow::Result<Self> {
        match s {
            "left" => Ok(Self::Left),
            "right" => Ok(Self::Right),
            _ => Err(anyhow::anyhow!("Incorrect side")),
        }
    }
}