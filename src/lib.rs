use std::{
    any::Any,
    fmt,
    hash::{Hash, Hasher},
    ops::{BitAnd, BitOr, Not},
};

use bevy_app::{App, Plugin};
use bevy_ecs::prelude::*;
use bevy_reflect::{
    prelude::*, utility::NonGenericTypeInfoCell, ApplyError, GetTypeRegistration, OpaqueInfo,
    ReflectMut, ReflectOwned, ReflectRef, TypeInfo, TypePath, TypeRegistration, Typed,
};
use bevy_utils::HashSet;
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};

pub mod prelude {
    pub use crate::{
        tags, GetEntityTags, GetTags, IntoTags, Tag, TagFilter, TagFilterAnd, TagFilterOr,
        TagPlugin, Tags,
    };
}

/// A [`Plugin`] required to register tag related types for reflection.
pub struct TagPlugin;

impl Plugin for TagPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<Tag>()
            .register_type::<Tags>()
            .register_type::<HashSet<Tag>>()
            .register_type_data::<HashSet<Tag>, ReflectSerialize>()
            .register_type_data::<HashSet<Tag>, ReflectDeserialize>()
            .register_type::<TagFilter>()
            .register_type::<Box<TagFilter>>()
            .register_type_data::<Box<TagFilter>, ReflectSerialize>()
            .register_type_data::<Box<TagFilter>, ReflectDeserialize>();
    }
}

/// Macro used to define new tags.
///
/// See [`Tag`] for more information.
#[macro_export]
macro_rules! tags {
    ($v0:vis $n0:ident, $($v:vis $n:ident),* $(,)?) => {
        $v0 const $n0: $crate::Tag = $crate::Tag::new(stringify!($n0));
        $crate::tags!($($v $n),*);
    };

    ($v:vis $name:ident) => {
        $v const $name: $crate::Tag = $crate::Tag::new(stringify!($name));
    };
}

/// A generic, cheap, and mostly unique identifier.
///
///
/// # Usage
///
/// A tag is a hashed representation of some string key. Two tags are equal if their hash values are equal.
///
/// Tags are defined to be very cheap to create, copy, and compare. However, they do not guarantee uniqueness.
/// In most application domains, the chance of tag collision within a single subsystem is very low.
///
/// Internally, tags are hashed using a 64-bit [FNV-1a](https://softwareengineering.stackexchange.com/a/145633) algorithm.
///
/// # Example
/// ```rust
/// use moonshine_tag::prelude::*;
///
/// tags! { A, B };
///
/// assert_eq!(A, A);
/// assert_ne!(A, B);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Reflect)]
#[reflect(Hash, PartialEq)]
pub struct Tag(u64);

impl Tag {
    /// Creates a new tag from a string key.
    ///
    /// # Example
    /// ```rust
    /// use moonshine_tag::prelude::*;
    ///
    /// tags! { A };
    ///
    /// assert_eq!(Tag::new("A"), A);
    /// ```
    pub const fn new(source: &str) -> Self {
        Self::from_hash(const_fnv1a_hash::fnv1a_hash_str_64(source))
    }

    /// Creates a new tag from a hash value.
    pub const fn from_hash(hash: u64) -> Self {
        Self(hash)
    }

    /// Returns the hash value of this tag.
    pub const fn hash(&self) -> u64 {
        self.0
    }
}

impl fmt::Debug for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tag({})", self.0)
    }
}

impl Hash for Tag {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.0)
    }
}

impl FromWorld for Tag {
    fn from_world(_: &mut World) -> Self {
        Self(u64::MAX)
    }
}

impl<T: Into<TagFilter>> BitOr<T> for Tag {
    type Output = TagFilter;

    fn bitor(self, rhs: T) -> Self::Output {
        TagFilter::Eq(self.into()) | rhs
    }
}

impl Not for Tag {
    type Output = TagFilter;

    fn not(self) -> Self::Output {
        !TagFilter::Eq(self.into())
    }
}

/// A collection of tags.
///
/// # Examples
///
/// ```rust
/// use moonshine_tag::prelude::*;
///
/// tags! { A, B };
///
/// let tags: Tags = [A, B].into_tags();
///
/// assert_eq!(tags, [B, A]);
/// ```
///
/// You may use this type on its own, or as a [`Component`]:
///
/// ```rust
/// use bevy::prelude::*;
/// use moonshine_tag::prelude::*;
///
/// tags! { A, B };
///
/// let mut world = World::new();
/// let entity = world.spawn([A, B].into_tags()).id();
/// assert_eq!(*world.tags(entity), [A, B]);
/// ```
#[derive(Component, Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Reflect)]
#[reflect(Component)]
pub struct Tags(HashSet<Tag>);

impl Tags {
    /// Creates a new empty set of tags.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the number of tags in this set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns `true` if this set contains the given tag.
    pub fn contains(&self, tag: Tag) -> bool {
        self.0.contains(&tag)
    }

    /// Inserts a tag into this set.
    pub fn insert(&mut self, tag: Tag) -> bool {
        self.0.insert(tag)
    }

    /// Removes a tag from this set.
    pub fn remove(&mut self, tag: Tag) -> bool {
        self.0.remove(&tag)
    }

    /// Returns an iterator over the tags in this set.
    pub fn iter(&self) -> impl Iterator<Item = Tag> + '_ {
        self.0.iter().copied()
    }

    /// Returns the union of this and another set of tags.
    #[must_use]
    pub fn union(mut self, other: impl IntoTags) -> Self {
        self.0.extend(other.into_tags());
        self
    }

    /// Returns `true` if this set has no tags in common with another set.
    pub fn is_disjoint(&self, tags: &Tags) -> bool {
        self.0.is_disjoint(&tags.0)
    }

    /// Returns `true` if the tags in this set are all present in another set.
    pub fn is_subset(&self, tags: &Tags) -> bool {
        self.0.is_subset(&tags.0)
    }

    /// Returns `true` if the tags in another set are all present in this set.
    pub fn is_superset(&self, tags: &Tags) -> bool {
        self.0.is_superset(&tags.0)
    }
}

impl From<Tag> for Tags {
    fn from(tag: Tag) -> Self {
        let mut tags = Tags::new();
        tags.insert(tag);
        tags
    }
}

impl<const N: usize> PartialEq<[Tag; N]> for Tags {
    fn eq(&self, other: &[Tag; N]) -> bool {
        self.0.len() == N && other.iter().all(|tag| self.0.contains(tag))
    }
}

impl<const N: usize> PartialEq<Tags> for [Tag; N] {
    fn eq(&self, other: &Tags) -> bool {
        other == self
    }
}

impl IntoIterator for Tags {
    type Item = Tag;
    type IntoIter = bevy_utils::hashbrown::hash_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// An extension trait to get [`Entity`] tags from a [`World`].
pub trait GetTags {
    /// Returns the tags associated with this entity, if any, or the empty set.
    fn tags(&self, entity: Entity) -> &Tags;
}

impl GetTags for World {
    fn tags(&self, entity: Entity) -> &Tags {
        self.get(entity).unwrap_or(&EMPTY_TAGS)
    }
}

/// An extension trait to get tags from an [`EntityRef`], [`EntityMut`], or [`EntityWorldMut`].
pub trait GetEntityTags {
    /// Returns the tags associated with this entity, if any, or the empty set.
    fn tags(&self) -> &Tags;
}

impl GetEntityTags for EntityRef<'_> {
    fn tags(&self) -> &Tags {
        self.get().unwrap_or(&EMPTY_TAGS)
    }
}

impl GetEntityTags for EntityMut<'_> {
    fn tags(&self) -> &Tags {
        self.get().unwrap_or(&EMPTY_TAGS)
    }
}

impl GetEntityTags for EntityWorldMut<'_> {
    fn tags(&self) -> &Tags {
        self.get().unwrap_or(&EMPTY_TAGS)
    }
}

/// A global empty set of tags.
///
/// # Usage
///
/// This is convenient for cases when the absence of `Tags` implies "no tags", for example:
///
/// ```
/// use bevy::prelude::*;
/// use moonshine_tag::{prelude::*, EMPTY_TAGS};
///
/// let mut world = World::new();
///
/// // Spawn an entity with no tags:
/// let entity = world.spawn_empty().id();
///
/// // Get tags, or just use the global empty set.
/// let tags: &Tags = world.get::<Tags>(entity).unwrap_or(&EMPTY_TAGS);
///
/// assert!(tags.is_empty());
/// ```
pub static EMPTY_TAGS: Lazy<Tags> = Lazy::new(Tags::new);

pub trait IntoTags {
    fn into_tags(self) -> Tags;
}

impl IntoTags for () {
    fn into_tags(self) -> Tags {
        Tags::new()
    }
}

impl IntoTags for Tag {
    fn into_tags(self) -> Tags {
        let mut tags = Tags::new();
        tags.insert(self);
        tags
    }
}

impl<const N: usize> IntoTags for [Tag; N] {
    fn into_tags(self) -> Tags {
        Tags(self.into_iter().flat_map(|t| t.into_tags()).collect())
    }
}

impl IntoTags for Tags {
    fn into_tags(self) -> Tags {
        self
    }
}

/// A filter which may be used to test a set of tags for a match.
///
/// # Usage
///
/// Filters are serializable and may be combined with each other to create complex tag matching rules.
///
/// # Examples
/// ```rust
/// use moonshine_tag::{prelude::*, all, matches};
///
/// tags! { A, B, C };
///
/// let a: Tags = A.into_tags();
/// let b: Tags = B.into_tags();
/// let c: Tags = C.into_tags();
/// let ab: Tags = [A, B].into_tags();
///
/// let a_or_b: TagFilter = A | B;
/// assert!(a_or_b.allows(&a));
/// assert!(a_or_b.allows(&b));
/// assert!(!a_or_b.allows(&ab));
/// assert!(!a_or_b.allows(&c));
///
/// let c_or_ab: TagFilter = C | [A, B];
/// assert!(!c_or_ab.allows(&a));
/// assert!(!c_or_ab.allows(&b));
/// assert!(c_or_ab.allows(&ab));
/// assert!(c_or_ab.allows(&c));
/// ```
#[derive(Clone, Serialize, Deserialize, Reflect)]
pub enum TagFilter {
    /// Matches no tags.
    None,
    /// Matches any set of tags.
    Any,
    /// Matches any set of tags which is exactly equal to the filter tags.
    Eq(Tags),
    /// Matches any set of tags which contains all of the filter tags.
    All(Tags),
    /// Matches any set of tags which contains any of the filter tags.
    Some(Tags),
    /// Matches any set of tags which matches both inner filters.
    And(TagFilterDyn, TagFilterDyn),
    /// Matches any set of tags which matches either inner filters.
    Or(TagFilterDyn, TagFilterDyn),
    /// Matches any set of tags which does not match the inner filter.
    Not(TagFilterDyn),
}

impl TagFilter {
    /// Returns `true` if this filter allows the given set of tags.
    pub fn allows(&self, tags: &Tags) -> bool {
        use TagFilter::*;
        match self {
            None => false,
            Any => true,
            Eq(a) => a == tags,
            All(a) => tags.is_subset(a),
            Some(a) => !tags.is_disjoint(a),
            And(a, b) => a.allows(tags) && b.allows(tags),
            Or(a, b) => a.allows(tags) || b.allows(tags),
            Not(a) => !a.allows(tags),
        }
    }
}

impl Default for TagFilter {
    fn default() -> Self {
        Self::Any
    }
}

impl<T: IntoTags> From<T> for TagFilter {
    fn from(tags: T) -> Self {
        Self::Eq(tags.into_tags())
    }
}

impl<T: Into<TagFilter>> BitAnd<T> for TagFilter {
    type Output = Self;

    fn bitand(self, rhs: T) -> Self::Output {
        Self::And(Box::new(self), Box::new(rhs.into()))
    }
}

impl<T: Into<TagFilter>> BitOr<T> for TagFilter {
    type Output = Self;

    fn bitor(self, rhs: T) -> Self::Output {
        Self::Or(Box::new(self), Box::new(rhs.into()))
    }
}

impl Not for TagFilter {
    type Output = TagFilter;

    fn not(self) -> Self::Output {
        Self::Not(Box::new(self))
    }
}

type TagFilterDyn = Box<TagFilter>;

impl PartialReflect for Box<TagFilter> {
    fn get_represented_type_info(&self) -> Option<&'static TypeInfo> {
        (**self).get_represented_type_info()
    }

    fn into_partial_reflect(self: Box<Self>) -> Box<dyn PartialReflect> {
        (*self).into_partial_reflect()
    }

    fn as_partial_reflect(&self) -> &dyn PartialReflect {
        (**self).as_partial_reflect()
    }

    fn as_partial_reflect_mut(&mut self) -> &mut dyn PartialReflect {
        (**self).as_partial_reflect_mut()
    }

    fn try_into_reflect(self: Box<Self>) -> Result<Box<dyn Reflect>, Box<dyn PartialReflect>> {
        (*self).try_into_reflect()
    }

    fn try_as_reflect(&self) -> Option<&dyn Reflect> {
        (**self).try_as_reflect()
    }

    fn try_as_reflect_mut(&mut self) -> Option<&mut dyn Reflect> {
        (**self).try_as_reflect_mut()
    }

    fn try_apply(&mut self, value: &dyn PartialReflect) -> Result<(), ApplyError> {
        (**self).try_apply(value)
    }

    fn reflect_ref(&self) -> ReflectRef {
        (**self).reflect_ref()
    }

    fn reflect_mut(&mut self) -> ReflectMut {
        (**self).reflect_mut()
    }

    fn reflect_owned(self: Box<Self>) -> ReflectOwned {
        (*self).reflect_owned()
    }

    fn clone_value(&self) -> Box<dyn PartialReflect> {
        (**self).clone_value()
    }
}

impl Reflect for Box<TagFilter> {
    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        (*self).into_any()
    }

    fn as_any(&self) -> &dyn Any {
        (**self).as_any()
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        (**self).as_any_mut()
    }

    fn into_reflect(self: Box<Self>) -> Box<dyn Reflect> {
        (*self).into_reflect()
    }

    fn as_reflect(&self) -> &dyn Reflect {
        (**self).as_reflect()
    }

    fn as_reflect_mut(&mut self) -> &mut dyn Reflect {
        (**self).as_reflect_mut()
    }

    fn set(&mut self, value: Box<dyn Reflect>) -> Result<(), Box<dyn Reflect>> {
        (**self).set(value)
    }
}

impl FromReflect for Box<TagFilter> {
    fn from_reflect(reflect: &dyn PartialReflect) -> Option<Self> {
        TagFilter::from_reflect(reflect).map(Box::new)
    }
}

impl TypePath for Box<TagFilter> {
    fn type_path() -> &'static str {
        TagFilter::type_path()
    }

    fn short_type_path() -> &'static str {
        TagFilter::short_type_path()
    }
}

impl Typed for Box<TagFilter> {
    fn type_info() -> &'static TypeInfo {
        static CELL: NonGenericTypeInfoCell = NonGenericTypeInfoCell::new();
        CELL.get_or_set(|| TypeInfo::Opaque(OpaqueInfo::new::<Self>()))
    }
}

impl GetTypeRegistration for Box<TagFilter> {
    fn get_type_registration() -> TypeRegistration {
        TypeRegistration::of::<Self>()
    }
}

pub fn none() -> TagFilter {
    TagFilter::Eq(().into_tags())
}

pub fn any() -> TagFilter {
    TagFilter::Any
}

pub fn all(all: impl IntoTags) -> TagFilter {
    TagFilter::All(all.into_tags())
}

pub fn some(some: impl IntoTags) -> TagFilter {
    TagFilter::Some(some.into_tags())
}

pub fn not(not: impl Into<TagFilter>) -> TagFilter {
    !not.into()
}

pub trait TagFilterOr {
    fn or(self, other: impl Into<TagFilter>) -> TagFilter;
}

impl<T: Into<TagFilter>> TagFilterOr for T {
    fn or(self, other: impl Into<TagFilter>) -> TagFilter {
        self.into() | other.into()
    }
}

pub trait TagFilterAnd {
    fn and(self, other: impl Into<TagFilter>) -> TagFilter;
}

impl<T: Into<TagFilter>> TagFilterAnd for T {
    fn and(self, other: impl Into<TagFilter>) -> TagFilter {
        self.into() & other.into()
    }
}

pub fn matches(tags: impl IntoTags, filter: impl Into<TagFilter>) -> bool {
    filter.into().allows(&tags.into_tags())
}

#[cfg(test)]
mod tests {
    use super::*;

    tags!(A, B, C);

    #[test]
    fn match_eq() {
        assert_eq!(A, A);
        assert_ne!(A, B);
    }

    #[test]
    fn match_empty() {
        assert!(matches((), ()));
        assert!(matches((), any()));
        assert!(matches((), all(())));
        assert!(matches((), none()));
        assert!(matches((), any() | ()));
        assert!(matches((), any() | none()));
        assert!(matches((), any() & ()));
        assert!(matches((), any() & none()));

        assert!(!matches((), !TagFilter::from(())));
        assert!(!matches((), !any()));
        assert!(!matches((), !none()));
        assert!(!matches((), A));
        assert!(!matches((), [A, B]));
        assert!(!matches((), some(A)));
        assert!(!matches((), some([A, B])));
    }

    #[test]
    fn match_tag() {
        assert!(matches(A, A));
        assert!(matches(A, any()));
        assert!(matches(A, A | B));
        assert!(matches(A, B | A));
        assert!(matches(A, !none()));
        assert!(matches(A, !B));
        assert!(matches(A, !C));
        assert!(matches(A, some(A)));
        assert!(matches(A, some([A, B])));
        assert!(matches(B, some([A, B])));

        assert!(!matches(A, B));
        assert!(!matches(A, none()));
        assert!(!matches(A, B | C));
        assert!(!matches(A, !A));
        assert!(!matches(A, [A, B]));
        assert!(!matches(A, some([B, C])));
    }

    #[test]
    fn match_tags() {
        assert!(matches([A, B], [A, B]));
        assert!(matches([A, B], [B, A]));
        assert!(matches([A, B], any()));
        assert!(matches([A, B], [A, B].or([B, C])));
        assert!(matches([A, B], !none()));
        assert!(matches([A, B], !A));
        assert!(matches([A, B], !C));
        assert!(matches([A, B], not([B, C])));
        assert!(matches([A, B], some([A, B])));

        assert!(!matches([A, B], [A, C]));
        assert!(!matches([A, B], none()));
        assert!(!matches([A, B], [A, C].or([B, C])));
        assert!(!matches([A, B], A));
        assert!(!matches([A, B], [A, B, C]));
        assert!(!matches([A, B], some(C)));
    }
}
