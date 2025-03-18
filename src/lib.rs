pub mod prelude {
    pub use crate::{
        tags, GetEntityTags, GetTags, IntoTags, Tag, TagFilter, TagPlugin, Tags, WithTags,
    };
}

mod filter;

use std::{
    fmt,
    hash::{Hash, Hasher},
    ops::{BitAnd, BitOr, Not},
};

use bevy_app::{App, Plugin};
use bevy_ecs::{component::ComponentId, prelude::*, world::DeferredWorld};
use bevy_reflect::prelude::*;
use bevy_utils::HashSet;
use once_cell::sync::Lazy;
use serde::{Deserialize, Serialize};

pub use filter::TagFilter;

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

impl<T: Into<TagFilter>> BitAnd<T> for Tag {
    type Output = TagFilter;

    fn bitand(self, rhs: T) -> Self::Output {
        TagFilter::Equal(self.into()) & rhs
    }
}

impl<T: Into<TagFilter>> BitOr<T> for Tag {
    type Output = TagFilter;

    fn bitor(self, rhs: T) -> Self::Output {
        TagFilter::Equal(self.into()) | rhs
    }
}

impl Not for Tag {
    type Output = TagFilter;

    fn not(self) -> Self::Output {
        !TagFilter::Equal(self.into())
    }
}

impl IntoIterator for Tag {
    type Item = Tag;

    type IntoIter = std::iter::Once<Tag>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::once(self)
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
    /// A static empty set of tags.
    ///
    /// # Usage
    ///
    /// This is convenient for cases when the absence of `Tags` implies "no tags", for example:
    ///
    /// ```
    /// use bevy::prelude::*;
    /// use moonshine_tag::{prelude::*};
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

    pub fn from_iter(iter: impl Iterator<Item = Tag>) -> Self {
        Self(HashSet::from_iter(iter))
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
        self.extend(other.into_tags());
        self
    }

    pub fn extend(&mut self, other: impl IntoTags) {
        self.0.extend(other.into_tags());
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

impl<const N: usize> From<[Tag; N]> for Tags {
    fn from(tags: [Tag; N]) -> Self {
        Tags::from_iter(tags.into_iter())
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
    type IntoIter = bevy_utils::hashbrown::hash_set::IntoIter<Self::Item>;

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
/// # Examples
/// ```
/// # use bevy::prelude::*;
/// # use moonshine_tag::prelude::*;
///
/// tags! { A, B, C };
///
/// let mut world = World::new();
/// let entity = world.spawn((WithTags(|| [A, B]), WithTags(|| [B, C]))).id();
/// world.flush();
/// assert_eq!(world.tags(entity), [A, B, C]);
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
    fn on_add(mut world: DeferredWorld, entity: Entity, _: ComponentId) {
        world
            .commands()
            .entity(entity)
            .queue(|entity: Entity, world: &mut World| {
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

/// An extension trait to get [`Entity`] tags from a [`World`].
pub trait GetTags {
    /// Returns the tags associated with this entity, if any, or the empty set.
    fn tags(&self, entity: Entity) -> &Tags;
}

impl GetTags for World {
    fn tags(&self, entity: Entity) -> &Tags {
        self.get(entity).unwrap_or(Tags::empty())
    }
}

/// An extension trait to get tags from an [`EntityRef`], [`EntityMut`], or [`EntityWorldMut`].
pub trait GetEntityTags {
    /// Returns the tags associated with this entity, if any, or the empty set.
    fn tags(&self) -> &Tags;
}

impl GetEntityTags for EntityRef<'_> {
    fn tags(&self) -> &Tags {
        self.get().unwrap_or(Tags::empty())
    }
}

impl GetEntityTags for EntityMut<'_> {
    fn tags(&self) -> &Tags {
        self.get().unwrap_or(Tags::empty())
    }
}

impl GetEntityTags for EntityWorldMut<'_> {
    fn tags(&self) -> &Tags {
        self.get().unwrap_or(Tags::empty())
    }
}

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
        assert!(matches((), TagFilter::any()));
        assert!(matches((), TagFilter::all_of(())));
        assert!(matches((), TagFilter::none()));
        assert!(matches((), TagFilter::any() | ()));
        assert!(matches((), TagFilter::any() | TagFilter::none()));
        assert!(matches((), TagFilter::any() & ()));
        assert!(matches((), TagFilter::any() & TagFilter::none()));

        assert!(!matches((), !TagFilter::from(())));
        assert!(!matches((), !TagFilter::any()));
        assert!(!matches((), !TagFilter::none()));
        assert!(!matches((), A));
        assert!(!matches((), [A, B]));
        assert!(!matches((), TagFilter::any_of(A)));
        assert!(!matches((), TagFilter::any_of([A, B])));
    }

    #[test]
    fn match_tag() {
        assert!(matches(A, A));
        assert!(matches(A, TagFilter::any()));
        assert!(matches(A, TagFilter::all_of(A)));
        assert!(matches(A, A | B));
        assert!(matches(A, B | A));
        assert!(matches(A, !TagFilter::none()));
        assert!(matches(A, !B));
        assert!(matches(A, !C));
        assert!(matches(A, TagFilter::any_of(A)));
        assert!(matches(A, TagFilter::any_of([A, B])));
        assert!(matches(B, TagFilter::any_of([A, B])));

        assert!(!matches(A, B));
        assert!(!matches(A, TagFilter::none()));
        assert!(!matches(A, TagFilter::all_of(B)));
        assert!(!matches(A, TagFilter::all_of([A, B])));
        assert!(!matches(A, B | C));
        assert!(!matches(A, !A));
        assert!(!matches(A, [A, B]));
        assert!(!matches(A, TagFilter::any_of([B, C])));
    }

    #[test]
    fn match_tags() {
        assert!(matches([A, B], [A, B]));
        assert!(matches([A, B], [B, A]));
        assert!(matches([A, B], TagFilter::any()));
        assert!(matches([A, B], TagFilter::all_of([A])));
        assert!(matches([A, B], TagFilter::all_of([A, B])));
        assert!(matches([A, B], TagFilter::all_of([A, B]) | [B, C]));
        assert!(matches([A, B], !TagFilter::none()));
        assert!(matches([A, B], !A));
        assert!(matches([A, B], !C));
        assert!(matches([A, B], !TagFilter::all_of([B, C])));
        assert!(matches([A, B], TagFilter::any_of([A, B])));

        assert!(!matches([A, B], [A, C]));
        assert!(!matches([A, B], TagFilter::none()));
        assert!(!matches([A, B], TagFilter::all_of([A, C])));
        assert!(!matches([A, B], TagFilter::all_of([A, B, C])));
        assert!(!matches([A, B], TagFilter::all_of([A, C]) | [B, C]));
        assert!(!matches([A, B], A));
        assert!(!matches([A, B], [A, B, C]));
        assert!(!matches([A, B], TagFilter::any_of(C)));
    }

    #[test]
    fn with_tags() {
        let mut world = World::new();
        let entity = world
            .spawn((WithTags(|| [A]), WithTags(|| [A, B]), WithTags(|| [B, C])))
            .id();
        world.flush();
        assert_eq!(world.tags(entity), [A, B, C]);
    }

    #[test]
    fn with_tags_existing() {
        let mut world = World::new();
        let entity = world
            .spawn((A.into_tags(), WithTags(|| [A, B]), WithTags(|| [B, C])))
            .id();
        world.flush();
        assert_eq!(world.tags(entity), [A, B, C]);
    }
}
