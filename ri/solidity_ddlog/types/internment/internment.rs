#![allow(
    path_statements,
    unused_imports,
    non_snake_case,
    non_camel_case_types,
    non_upper_case_globals,
    unused_parens,
    non_shorthand_field_patterns,
    dead_code,
    overflowing_literals,
    unreachable_patterns,
    unused_variables,
    clippy::missing_safety_doc,
    clippy::match_single_binding,
    clippy::ptr_arg,
    clippy::redundant_closure,
    clippy::needless_lifetimes,
    clippy::borrowed_box,
    clippy::map_clone,
    clippy::toplevel_ref_arg,
    clippy::double_parens,
    clippy::collapsible_if,
    clippy::clone_on_copy,
    clippy::unused_unit,
    clippy::deref_addrof,
    clippy::clone_on_copy,
    clippy::needless_return,
    clippy::op_ref,
    clippy::match_like_matches_macro,
    clippy::comparison_chain,
    clippy::len_zero,
    clippy::extra_unused_lifetimes
)]

use ::num::One;
use ::std::ops::Deref;

use ::differential_dataflow::collection;
use ::timely::communication;
use ::timely::dataflow::scopes;
use ::timely::worker;

use ::ddlog_derive::{FromRecord, IntoRecord, Mutator};
use ::differential_datalog::ddval::DDValConvert;
use ::differential_datalog::program;
use ::differential_datalog::program::TupleTS;
use ::differential_datalog::program::XFormArrangement;
use ::differential_datalog::program::XFormCollection;
use ::differential_datalog::program::Weight;
use ::differential_datalog::record::FromRecord;
use ::differential_datalog::record::FromRecordInner;
use ::differential_datalog::record::IntoRecord;
use ::differential_datalog::record::Mutator;
use ::differential_datalog::record::MutatorInner;
use ::serde::Deserialize;
use ::serde::Serialize;


// `usize` and `isize` are builtin Rust types; we therefore declare an alias to DDlog's `usize` and
// `isize`.
pub type std_usize = u64;
pub type std_isize = i64;


/*
Copyright (c) 2021 VMware, Inc.
SPDX-License-Identifier: MIT

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use ddlog_std::{hash64, Vec as DDlogVec};
use differential_datalog::record::{self, Record};
use internment::ArcIntern;
use serde::{de::Deserializer, ser::Serializer};
use std::{
    any::{Any, TypeId},
    cell::RefCell,
    cmp::{self, Ordering},
    collections::HashMap,
    fmt::{Debug, Display, Formatter, Result as FmtResult},
    hash::{Hash, Hasher},
    thread_local,
};

/// An atomically reference counted handle to an interned value.
/// In addition to memory deduplication, this type is optimized for fast comparison.
/// To this end, we store a 64-bit hash along with the interned value and use this hash for
/// comparison, only falling back to by-value comparison in case of a hash collision.
#[derive(Eq, PartialEq)]
pub struct Intern<A>
where
    A: Eq + Send + Sync + Hash + 'static,
{
    interned: ArcIntern<(u64, A)>,
}

// Implement `Clone` by hand instead of auto-deriving to avoid
// bogus `T: Clone` trait bound.
impl<T: Eq + Send + Sync + Hash + 'static> Clone for Intern<T> {
    fn clone(&self) -> Self {
        Self {
            interned: self.interned.clone(),
        }
    }
}

impl<T: Hash + Eq + Send + Sync + 'static> Hash for Intern<T> {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.interned.as_ref().0.hash(state)
    }
}

impl<T: Any + Default + Eq + Send + Sync + Hash + 'static> Default for Intern<T> {
    // The straightforward implementation (`Self::new(T::default())`) causes performance
    // issues in workloads that call this method often.
    // First, `ArcIntern::new()` performs heap allocation even if the value already
    // exists in the interner.  Second, `default()` always hits the same shard in the
    // `dashmap` inside `internment` causing contention given a large enough number of
    // threads (in my experiments it gets pretty severe with 16 threads running on 16
    // CPU cores).
    //
    // Note that we cannot rely on the DDlog compiler optimization that statically
    // evaluates function calls with constant arguments, as `Intern::default()` is
    // typically called from Rust, not DDlog, e.g., when deserializing fields with
    // `serde(default)` annotations.
    //
    // The following implementation uses thread-local cache to make sure that we will
    // call `Self::new(T::default())` at most once per type per thread.
    fn default() -> Self {
        thread_local! {
            static INTERN_DEFAULT: RefCell<HashMap<TypeId, Box<dyn Any + Send + Sync + 'static>>> = RefCell::new(HashMap::new());
        }

        INTERN_DEFAULT.with(|m| {
            if let Some(v) = m.borrow().get(&TypeId::of::<T>()) {
                return v.as_ref().downcast_ref::<Self>().unwrap().clone();
            };

            let val = Self::new(T::default());
            m.borrow_mut()
                .insert(TypeId::of::<T>(), Box::new(val.clone()));
            val
        })
    }
}

impl<T> Intern<T>
where
    T: Eq + Hash + Send + Sync + 'static,
{
    /// Create a new interned value.
    pub fn new(value: T) -> Self {
        // Hash the value.  Note: this is technically redundant,
        // as `ArcIntern` hashes the value internally, but we
        // cannot easily access that hash value.
        let hash = hash64(&value);
        Intern {
            interned: ArcIntern::new((hash, value)),
        }
    }

    /// Get the current interned item's pointer as a `usize`
    fn as_usize(&self) -> usize {
        self.as_ref() as *const T as usize
    }
}

/// Order the interned values:
/// - Start with comparing pointers.  The two values are the same if and only if the pointers are
/// the same.
/// - Otherwise, compare their 64-bit hashes and order them based on hash values.
/// - In the extremely rare case where a hash collision occurs, compare the actual values.
impl<T> Ord for Intern<T>
where
    T: Eq + Ord + Send + Sync + Hash + 'static,
{
    fn cmp(&self, other: &Self) -> Ordering {
        if self.as_usize() == other.as_usize() {
            return Ordering::Equal;
        } else {
            match self.interned.as_ref().0.cmp(&other.interned.as_ref().0) {
                Ordering::Equal => self.as_ref().cmp(other.as_ref()),
                ord => ord,
            }
        }
    }
}

impl<T> PartialOrd for Intern<T>
where
    T: Eq + Ord + Send + Sync + Hash + 'static,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Deref for Intern<T>
where
    T: Eq + Send + Sync + Hash + 'static,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.interned.deref().1
    }
}

impl<T> AsRef<T> for Intern<T>
where
    T: Eq + Hash + Send + Sync + 'static,
{
    fn as_ref(&self) -> &T {
        &self.interned.as_ref().1
    }
}

impl<T> From<T> for Intern<T>
where
    T: Eq + Hash + Send + Sync + 'static,
{
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T> From<&[T]> for Intern<Vec<T>>
where
    T: Eq + Hash + Send + Sync + Clone + 'static,
{
    fn from(slice: &[T]) -> Self {
        Self::new(slice.to_vec())
    }
}

impl From<&str> for Intern<String> {
    fn from(string: &str) -> Self {
        Self::new(string.to_owned())
    }
}

impl<T> Display for Intern<T>
where
    T: Display + Eq + Hash + Send + Sync,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(self.as_ref(), f)
    }
}

impl<T> Debug for Intern<T>
where
    T: Debug + Eq + Hash + Send + Sync,
{
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Debug::fmt(self.as_ref(), f)
    }
}

impl<T> Serialize for Intern<T>
where
    T: Serialize + Eq + Hash + Send + Sync,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_ref().serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for Intern<T>
where
    T: Deserialize<'de> + Eq + Hash + Send + Sync + 'static,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        T::deserialize(deserializer).map(Intern::new)
    }
}

impl<T> FromRecord for Intern<T>
where
    T: FromRecord + Eq + Hash + Send + Sync + 'static,
{
    fn from_record(val: &Record) -> Result<Self, String> {
        T::from_record(val).map(Intern::new)
    }
}

impl<T> IntoRecord for Intern<T>
where
    T: IntoRecord + Eq + Hash + Send + Sync + Clone,
{
    fn into_record(self) -> Record {
        ival(&self).clone().into_record()
    }
}

impl<T> Mutator<Intern<T>> for Record
where
    T: Clone + Eq + Send + Sync + Hash,
    Record: Mutator<T>,
{
    fn mutate(&self, value: &mut Intern<T>) -> Result<(), String> {
        let mut mutated = ival(value).clone();
        self.mutate(&mut mutated)?;
        *value = intern(mutated);

        Ok(())
    }
}

/// Create a new interned value
pub fn intern<T>(value: T) -> Intern<T>
where
    T: Eq + Hash + Send + Sync + 'static,
{
    Intern::new(value)
}

/// Get the inner value of an interned value
pub fn ival<T>(value: &Intern<T>) -> &T
where
    T: Eq + Hash + Send + Sync + Clone,
{
    value.as_ref()
}

/// Join interned strings with a separator
pub fn istring_join(strings: &DDlogVec<istring>, separator: &String) -> String {
    strings
        .vec
        .iter()
        .map(|string| string.as_ref())
        .cloned()
        .collect::<Vec<String>>()
        .join(separator.as_str())
}

/// Split an interned string by a separator
pub fn istring_split(string: &istring, separator: &String) -> DDlogVec<String> {
    DDlogVec {
        vec: string
            .as_ref()
            .split(separator)
            .map(|string| string.to_owned())
            .collect(),
    }
}

/// Returns true if the interned string contains the given pattern
pub fn istring_contains(interned: &istring, pattern: &String) -> bool {
    interned.as_ref().contains(pattern.as_str())
}

pub fn istring_substr(string: &istring, start: &std_usize, end: &std_usize) -> String {
    let len = string.as_ref().len();
    let from = cmp::min(*start as usize, len);
    let to = cmp::max(from, cmp::min(*end as usize, len));

    string.as_ref()[from..to].to_string()
}

pub fn istring_replace(string: &istring, from: &String, to: &String) -> String {
    string.as_ref().replace(from, to)
}

pub fn istring_starts_with(string: &istring, prefix: &String) -> bool {
    string.as_ref().starts_with(prefix)
}

pub fn istring_ends_with(string: &istring, suffix: &String) -> bool {
    string.as_ref().ends_with(suffix)
}

pub fn istring_trim(string: &istring) -> String {
    string.as_ref().trim().to_string()
}

pub fn istring_len(string: &istring) -> std_usize {
    string.as_ref().len() as std_usize
}

pub fn istring_to_bytes(string: &istring) -> DDlogVec<u8> {
    DDlogVec::from(string.as_ref().as_bytes())
}

pub fn istring_to_lowercase(string: &istring) -> String {
    string.as_ref().to_lowercase()
}

pub fn istring_to_uppercase(string: &istring) -> String {
    string.as_ref().to_uppercase()
}

pub fn istring_reverse(string: &istring) -> String {
    string.as_ref().chars().rev().collect()
}

pub type istring = Intern<String>;
/* fn intern<A: ::ddlog_rt::Val>(s: A) -> Intern<A> */
/* fn istring_contains(s1: & istring, s2: & String) -> bool */
/* fn istring_ends_with(s: & istring, suffix: & String) -> bool */
/* fn istring_join(strings: & ddlog_std::Vec<istring>, sep: & String) -> String */
/* fn istring_len(s: & istring) -> u64 */
/* fn istring_replace(s: & istring, from: & String, to: & String) -> String */
/* fn istring_reverse(s: & istring) -> String */
/* fn istring_split(s: & istring, sep: & String) -> ddlog_std::Vec<String> */
/* fn istring_starts_with(s: & istring, prefix: & String) -> bool */
/* fn istring_substr(s: & istring, start: & u64, end: & u64) -> String */
/* fn istring_to_bytes(s: & istring) -> ddlog_std::Vec<u8> */
/* fn istring_to_lowercase(s: & istring) -> String */
/* fn istring_to_uppercase(s: & istring) -> String */
/* fn istring_trim(s: & istring) -> String */
/* fn ival<A: ::ddlog_rt::Val>(s: & Intern<A>) -> A */
pub fn contains(s1: & istring, s2: & String) -> bool
{   istring_contains(s1, s2)
}
pub fn ends_with(s: & istring, suffix: & String) -> bool
{   istring_ends_with(s, suffix)
}
pub fn join(strings: & ddlog_std::Vec<istring>, sep: & String) -> String
{   istring_join(strings, sep)
}
pub fn len(s: & istring) -> u64
{   istring_len(s)
}
pub fn parse_dec_i64(s: & istring) -> ddlog_std::Option<i64>
{   ddlog_std::parse_dec_i64(ival(s))
}
pub fn parse_dec_u64(s: & istring) -> ddlog_std::Option<u64>
{   ddlog_std::parse_dec_u64(ival(s))
}
pub fn replace(s: & istring, from: & String, to: & String) -> String
{   istring_replace(s, from, to)
}
pub fn reverse(s: & istring) -> String
{   istring_reverse(s)
}
pub fn split(s: & istring, sep: & String) -> ddlog_std::Vec<String>
{   istring_split(s, sep)
}
pub fn starts_with(s: & istring, prefix: & String) -> bool
{   istring_starts_with(s, prefix)
}
pub fn substr(s: & istring, start: & u64, end: & u64) -> String
{   istring_substr(s, start, end)
}
pub fn to_bytes(s: & istring) -> ddlog_std::Vec<u8>
{   istring_to_bytes(s)
}
pub fn to_lowercase(s: & istring) -> String
{   istring_to_lowercase(s)
}
pub fn to_string(s: & istring) -> String
{   (*ival(s)).clone()
}
pub fn to_uppercase(s: & istring) -> String
{   istring_to_uppercase(s)
}
pub fn trim(s: & istring) -> String
{   istring_trim(s)
}