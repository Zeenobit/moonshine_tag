use std::{
    any::Any,
    ops::{BitAnd, BitOr, Not},
};

use bevy_reflect::{
    prelude::*, utility::NonGenericTypeInfoCell, ApplyError, GetTypeRegistration, OpaqueInfo,
    ReflectMut, ReflectOwned, ReflectRef, TypeInfo, TypePath, TypeRegistration, Typed,
};
use serde::{Deserialize, Serialize};

use crate::{IntoTags, Tags};

/// A filter which may be used to test a set of tags for a match.
///
/// # Usage
///
/// Filters are serializable and may be combined with each other to create complex tag matching rules.
///
/// # Examples
/// ```rust
/// use moonshine_tag::{prelude::*, matches};
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
    /// Matches any set of tags which is exactly equal to the filter tags.
    Equal(Tags),
    /// Matches any set of tags which contains all of the filter tags.
    AllOf(Tags),
    /// Matches any set of tags which contains any of the filter tags.
    AnyOf(Tags),
    /// Matches any set of tags which matches both inner filters.
    And(TagFilterDyn, TagFilterDyn),
    /// Matches any set of tags which matches either inner filters.
    Or(TagFilterDyn, TagFilterDyn),
    /// Matches any set of tags which does not match the inner filter.
    Not(TagFilterDyn),
}

impl TagFilter {
    pub fn none() -> TagFilter {
        TagFilter::Equal(().into_tags())
    }

    pub fn any() -> TagFilter {
        TagFilter::AllOf(().into_tags())
    }

    pub fn all_of(tags: impl IntoTags) -> TagFilter {
        TagFilter::AllOf(tags.into_tags())
    }

    pub fn any_of(tags: impl IntoTags) -> TagFilter {
        TagFilter::AnyOf(tags.into_tags())
    }

    /// Returns `true` if this filter allows the given set of tags.
    pub fn allows(&self, tags: &Tags) -> bool {
        use TagFilter::*;
        match self {
            None => false,
            Equal(a) => a == tags,
            AllOf(a) => a.is_subset(tags),
            AnyOf(a) => !a.is_disjoint(tags),
            And(a, b) => a.allows(tags) && b.allows(tags),
            Or(a, b) => a.allows(tags) || b.allows(tags),
            Not(a) => !a.allows(tags),
        }
    }
}

impl Default for TagFilter {
    fn default() -> Self {
        Self::any()
    }
}

impl<T: IntoTags> From<T> for TagFilter {
    fn from(tags: T) -> Self {
        Self::Equal(tags.into_tags())
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
