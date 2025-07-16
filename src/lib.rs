#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

pub mod prelude {
    //! Prelude module to import the most essential types and traits.

    pub use crate::{tag_filter, tags, ComponentTags, Tag, TagFilter, TagPlugin, Tags, WithTags};
}

mod filter;

pub extern crate inventory;

use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

use bevy_app::{App, Plugin};
use bevy_ecs::component::HookContext;
use bevy_ecs::prelude::*;
use bevy_ecs::world::DeferredWorld;
use bevy_platform::collections::HashSet;
use bevy_reflect::prelude::*;
use itertools::Itertools;
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};

pub use self::filter::*;

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
    ($(#[$meta:meta])* $v:vis $name:ident $(,)?) => {
        $(#[$meta])*
        $v const $name: $crate::Tag = $crate::Tag::new(stringify!($name));
        $crate::inventory::submit! {
            $crate::TagMeta { tag: $name, name: stringify!($name) }
        }
    };

    ($(#[$meta:meta])* $v0:vis $n0:ident, $($v:vis $n:ident),* $(,)?) => {
        $(#[$meta])*
        $v0 const $n0: $crate::Tag = $crate::Tag::new(stringify!($n0));
        $crate::inventory::submit! {
            $crate::TagMeta { tag: $n0, name: stringify!($n0) }
        }
        $crate::tags!($($v $n),*);
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
/// tags! { A, pub B };
///
/// assert_eq!(A, A);
/// assert_ne!(A, B);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Reflect)]
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

    /// Returns a human-friendly representation of this tag's hash.
    ///
    /// This is a [base31](https://github.com/kmanley/base31)-encoded string representation of the tag's hash value.
    ///
    /// This is, on average, faster than [`resolve_name`](Tag::resolve_name) for human-friendly identification.
    pub fn pretty_hash(&self) -> String {
        base31::encode(self.0)
    }

    /// Returns `true` if this tag matches the given [`TagFilter`].
    pub fn matches(&self, filter: &TagFilter) -> bool {
        use TagFilter::*;
        match filter {
            Equal(a) => a.iter().exactly_one().is_ok_and(|tag| tag == *self),
            AllOf(a) => a.is_empty() || a.iter().exactly_one().is_ok_and(|tag| tag == *self),
            AnyOf(a) => a.contains(*self),
            And(a, b) => self.matches(a) && self.matches(b),
            Or(a, b) => self.matches(a) || self.matches(b),
            Not(a) => !self.matches(a),
        }
    }

    /// Resolves the name of this tag, if it has been defined using the `tags!` macro.
    ///
    /// Note that this function is very slow and should not be used in performance-critical code.
    /// It performs a linear search over all registered tags to find a match and it is mainly designed for debugging and editor purposes.
    ///
    /// If performance is a concern, consider using [`pretty_hash`](Tag::pretty_hash) or cache the result of this function.
    /// See [`iter_names`](Tag::iter_names) for an iterator over all registered tag names.
    ///
    /// Tags must be defined using the [`tags!`] macro to be registered.
    pub fn resolve_name(&self) -> Option<&'static str> {
        TagMeta::iter()
            .find(|meta| meta.tag == *self)
            .map(|meta| meta.name)
    }

    /// Returns an iterator over all registered tag names.
    ///
    /// Tags must be defined using the [`tags!`] macro to be registered.
    pub fn iter_names() -> impl Iterator<Item = &'static str> {
        TagMeta::iter().map(|meta| meta.name)
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

impl IntoIterator for Tag {
    type Item = Tag;

    type IntoIter = std::iter::Once<Tag>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self)
    }
}

/// The metadata associated with a [`Tag`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Reflect)]
pub struct TagMeta {
    /// The tag itself.
    pub tag: Tag,
    /// The name of the tag.
    pub name: &'static str,
}

impl TagMeta {
    /// Iterates over all registered tag metadata.
    pub fn iter() -> impl Iterator<Item = &'static TagMeta> {
        inventory::iter::<TagMeta>()
    }
}

inventory::collect!(TagMeta);

/// A collection of tags.
///
/// # Examples
///
/// ```rust
/// use moonshine_tag::prelude::*;
///
/// tags! { A, B };
///
/// let tags: Tags = [A, B].into();
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
/// let entity = world.spawn(Tags::from([A, B])).id();
/// assert_eq!(world.get::<Tags>(entity).unwrap(), [A, B]);
/// ```
#[derive(Component, Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Reflect)]
#[reflect(Component)]
pub struct Tags(HashSet<Tag>);

impl Tags {
    /// A static empty set of tags.
    ///
    /// # Usage
    ///
    /// This is convenient for cases when the absence of `Tags` implies "no tags", for example:
    ///
    /// ```
    /// use bevy::prelude::*;
    /// use moonshine_tag::prelude::*;
    ///
    /// let mut world = World::new();
    ///
    /// // Spawn an entity with no tags:
    /// let entity = world.spawn_empty().id();
    ///
    /// // Get tags, or just use the global empty set.
    /// let tags = world.get::<Tags>(entity).unwrap_or(Tags::empty());
    ///
    /// assert!(tags.is_empty());
    /// ```
    pub fn empty() -> &'static Tags {
        static EMPTY: Lazy<Tags> = Lazy::new(Tags::new);
        &EMPTY
    }

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
    pub fn union(mut self, other: impl Into<Tags>) -> Self {
        self.extend(other.into());
        self
    }

    /// Adds all tags from another set to this one.
    pub fn extend(&mut self, other: impl Into<Tags>) {
        self.0.extend(other.into());
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

    /// Returns `true` if this set matches the given filter.
    pub fn matches(&self, filter: &TagFilter) -> bool {
        use TagFilter::*;
        match filter {
            Equal(a) => a == self,
            AllOf(a) => a.is_subset(self),
            AnyOf(a) => !a.is_disjoint(self),
            And(a, b) => self.matches(a) && self.matches(b),
            Or(a, b) => self.matches(a) || self.matches(b),
            Not(a) => !self.matches(a),
        }
    }

    /// Returns a string representation of this set of tags.
    ///
    /// Note that this function is slow and should not be used in performance-critical code.
    /// It is mainly designed for debugging and editor purposes.
    pub fn to_pretty_string(&self) -> String {
        self.0
            .iter()
            .map(|tag| {
                tag.resolve_name()
                    .map(|name| name.to_string())
                    .unwrap_or_else(|| tag.pretty_hash())
            })
            .join(", ")
    }
}

impl From<Tag> for Tags {
    fn from(tag: Tag) -> Self {
        let mut tags = Tags::new();
        tags.insert(tag);
        tags
    }
}

impl FromIterator<Tag> for Tags {
    fn from_iter<T: IntoIterator<Item = Tag>>(iter: T) -> Self {
        Self(HashSet::from_iter(iter))
    }
}

impl<const N: usize> From<[Tag; N]> for Tags {
    fn from(tags: [Tag; N]) -> Self {
        Tags::from_iter(tags)
    }
}

impl<const N: usize> PartialEq<[Tag; N]> for Tags {
    fn eq(&self, other: &[Tag; N]) -> bool {
        self.0.len() == N && other.iter().all(|tag| self.0.contains(tag))
    }
}

impl<const N: usize> PartialEq<[Tag; N]> for &Tags {
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
    type IntoIter = bevy_platform::collections::hash_set::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// A [`Component`] which merges a set of tags into single [`Tags`] component.
///
/// # Usage
///
/// This is useful for adding tags to entities which may or may not have a `Tags` component
/// already without replacing the existing tags, especially when spawning bundles.
///
/// This implementation takes advantage of the unique type signature of Rust lambda functions
/// to avoid conflicts when used in bundles. While more flexible, this usage is not possible
/// as component requirements. For that, see [`ComponentTags`].
///
/// # Examples
/// ```
/// use bevy::prelude::*;
/// use moonshine_tag::prelude::*;
///
/// tags! { A, B, C };
///
/// let mut world = World::new();
/// let entity = world.spawn((WithTags(|| [A, B]), WithTags(|| [B, C]))).id();
/// world.flush();
/// assert_eq!(world.get::<Tags>(entity).unwrap(), [A, B, C]);
/// ```
#[derive(Component)]
#[component(storage = "SparseSet")]
#[component(on_add = Self::on_add)]
pub struct WithTags<I: IntoIterator<Item = Tag>, F: FnOnce() -> I>(pub F)
where
    F: 'static + Send + Sync,
    I: 'static + Send + Sync;

impl<I: IntoIterator<Item = Tag>, F: FnOnce() -> I> WithTags<I, F>
where
    F: 'static + Send + Sync,
    I: 'static + Send + Sync,
{
    fn on_add(mut world: DeferredWorld, ctx: HookContext) {
        let entity = ctx.entity;
        world.commands().queue(move |world: &mut World| {
            let mut entity = world.entity_mut(entity);
            let this = entity.take::<Self>().unwrap();
            let new_tags = Tags::from_iter(this.0().into_iter());
            if let Some(mut tags) = entity.get_mut::<Tags>() {
                tags.extend(new_tags);
            } else {
                entity.insert(new_tags);
            }
        });
    }
}

/// A [`Component`] which merges a set of tags into single [`Tags`] component.
///
/// # Usage
///
/// This is similar to [`WithTags`], except that it may be used as a component requirement.
///
/// Note that `T` can be anything. By convention, it is best to use `Self` to avoid requirement conflicts.
///
/// # Examples
/// ```
/// # use bevy::prelude::*;
/// # use moonshine_tag::prelude::*;
///
/// tags! { A, B, C };
///
/// #[derive(Component)]
/// #[require(ComponentTags<Self> = A)]
/// struct Foo;
///
/// #[derive(Component)]
/// #[require(ComponentTags<Self> = [B, C])]
/// struct Bar;
///
/// let mut world = World::new();
/// let entity = world.spawn((Foo, Bar)).id();
/// world.flush();
/// assert_eq!(world.get::<Tags>(entity).unwrap(), [A, B, C]);
/// ```
#[derive(Component)]
#[component(storage = "SparseSet")]
#[component(on_add = Self::on_add)]
pub struct ComponentTags<T: Component>(Tags, PhantomData<T>);

impl<T: Component> ComponentTags<T> {
    fn on_add(mut world: DeferredWorld, ctx: HookContext) {
        let entity = ctx.entity;
        world.commands().queue(move |world: &mut World| {
            let mut entity = world.entity_mut(entity);
            let ComponentTags(new_tags, ..) = entity.take::<Self>().unwrap();
            if let Some(mut tags) = entity.get_mut::<Tags>() {
                tags.extend(new_tags);
            } else {
                entity.insert(new_tags);
            }
        });
    }
}

impl<T: Component, I: Into<Tags>> From<I> for ComponentTags<T> {
    fn from(tags: I) -> Self {
        Self(tags.into(), PhantomData)
    }
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

    pub fn matches(tags: impl Into<Tags>, filter: &TagFilter) -> bool {
        tags.into().matches(filter)
    }

    #[test]
    fn match_empty() {
        assert!(matches([], &tag_filter!([])));
        assert!(matches([], &tag_filter!([..])));
        assert!(matches([], &tag_filter!([..] | [])));
        assert!(matches([], &tag_filter!([..] & [])));

        assert!(!matches([], &tag_filter!(![])));
        assert!(!matches([], &tag_filter!([A])));
        assert!(!matches([], &tag_filter!([A, ..])));
        assert!(!matches([], &tag_filter!([A, B])));
        assert!(!matches([], &tag_filter!([A | B])));
    }

    #[test]
    fn match_tag() {
        assert!(matches(A, &tag_filter!([..])));
        assert!(matches(A, &tag_filter!([..] | [])));
        assert!(matches(A, &tag_filter!(![])));
        assert!(matches(A, &tag_filter!([A])));
        assert!(matches(A, &tag_filter!([A | B])));
        assert!(matches(A, &tag_filter!([B | A])));
        assert!(matches(A, &tag_filter!(![B])));
        assert!(matches(A, &tag_filter!([A, ..])));
        assert!(matches(A, &tag_filter!([A, ..] | [B])));

        assert!(A.matches(&tag_filter!([..])));
        assert!(A.matches(&tag_filter!([..] | [])));
        assert!(A.matches(&tag_filter!(![])));
        assert!(A.matches(&tag_filter!([A])));
        assert!(A.matches(&tag_filter!([A | B])));
        assert!(A.matches(&tag_filter!([B | A])));
        assert!(A.matches(&tag_filter!(![B])));
        assert!(A.matches(&tag_filter!([A, ..])));
        assert!(A.matches(&tag_filter!([A, ..] | [B])));

        assert!(!matches(A, &tag_filter!([])));
        assert!(!matches(A, &tag_filter!(![A])));
        assert!(!matches(A, &tag_filter!([B])));
        assert!(!matches(A, &tag_filter!([A, B, ..])));
        assert!(!matches(A, &tag_filter!([B, A, ..])));
        assert!(!matches(A, &tag_filter!([B | C])));
        assert!(!matches(A, &tag_filter!([B, C, ..])));

        assert!(!A.matches(&tag_filter!([])));
        assert!(!A.matches(&tag_filter!(![A])));
        assert!(!A.matches(&tag_filter!([B])));
        assert!(!A.matches(&tag_filter!([A, B, ..])));
        assert!(!A.matches(&tag_filter!([B, A, ..])));
        assert!(!A.matches(&tag_filter!([B | C])));
        assert!(!A.matches(&tag_filter!([B, C, ..])));
    }

    #[test]
    fn match_tags() {
        assert!(matches([A, B], &tag_filter!([..])));
        assert!(matches([A, B], &tag_filter!([..] | [])));
        assert!(matches([A, B], &tag_filter!(![])));
        assert!(matches([A, B], &tag_filter!([A, B])));
        assert!(matches([A, B], &tag_filter!([B, A])));
        assert!(matches([A, B], &tag_filter!([A, ..])));
        assert!(matches([A, B], &tag_filter!([A, B, ..])));
        assert!(matches([A, B], &tag_filter!([A, B, ..] | [B, C])));

        assert!(!matches([A, B], &tag_filter!([])));
        assert!(!matches([A, B], &tag_filter!([A])));
        assert!(!matches([A, B], &tag_filter!([A, B, C])));
        assert!(!matches([A, B], &tag_filter!(![A, B])));
        assert!(!matches([A, B], &tag_filter!([A, C])));
        assert!(!matches([A, B], &tag_filter!([A, C, ..])));
        assert!(!matches([A, B], &tag_filter!([A, B, C, ..])));
        assert!(!matches([A, B], &tag_filter!([A, C, ..] | [B, C])));
        assert!(!matches([A, B], &tag_filter!([C, ..])));
    }

    #[test]
    fn with_tags() {
        let mut world = World::new();
        let entity = world
            .spawn((WithTags(|| [A]), WithTags(|| [A, B]), WithTags(|| [B, C])))
            .id();
        world.flush();
        assert_eq!(
            world.get::<Tags>(entity).unwrap_or(Tags::empty()),
            [A, B, C]
        );
    }

    #[test]
    fn with_tags_existing() {
        let mut world = World::new();
        let entity = world
            .spawn((Tags::from(A), WithTags(|| [A, B]), WithTags(|| [B, C])))
            .id();
        world.flush();
        assert_eq!(
            world.get::<Tags>(entity).unwrap_or(Tags::empty()),
            [A, B, C]
        );
    }
}
