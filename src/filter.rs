use std::ops::{BitAndAssign, BitOrAssign};
use std::{
    any::Any,
    ops::{BitAnd, BitOr, Not},
};

use bevy_reflect::{
    prelude::*, utility::NonGenericTypeInfoCell, ApplyError, GetTypeRegistration, OpaqueInfo,
    ReflectCloneError, ReflectMut, ReflectOwned, ReflectRef, TypeInfo, TypePath, TypeRegistration,
    Typed,
};
use serde::{Deserialize, Serialize};

use crate::Tags;

/// A filter which may be used to test a set of tags for a match.
///
/// # Usage
///
/// Filters are serializable and may be combined with each other to create complex tag matching rules.
///
/// # Examples
/// ```rust
/// use moonshine_tag::{prelude::*};
///
/// tags! { A, B, C };
///
/// let filter: TagFilter = tag_filter!([A, B, ..]);
/// let a = Tags::from([A]);
/// let ab = Tags::from([A, B]);
/// let cb = Tags::from([C, B]);
/// let abc = Tags::from([A, B, C]);
///
/// assert!(filter.allows(&ab));
/// assert!(filter.allows(&abc));
/// assert!(!filter.allows(&a));
/// assert!(!filter.allows(&cb));
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Reflect)]
pub enum TagFilter {
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

#[doc(hidden)]
#[deprecated(since = "0.2.0", note = "use `TagFilter` instead")]
pub type Filter = TagFilter;

impl TagFilter {
    /// Creates a new filter which does not match any tags.
    pub fn none() -> TagFilter {
        Self::Equal(Tags::default())
    }

    /// Creates a new filter which matches only an exact set of tags.
    pub fn equal(tags: impl Into<Tags>) -> TagFilter {
        Self::Equal(tags.into())
    }

    /// Creates a new filter which matches any set of tags.
    pub fn any() -> TagFilter {
        Self::AllOf(Tags::default())
    }

    /// Creates a new filter which matches any set of tags which contains all of the given tags.
    pub fn all_of(tags: impl Into<Tags>) -> TagFilter {
        Self::AllOf(tags.into())
    }

    /// Creates a new filter which matches any set of tags which contains any of the given tags.
    pub fn any_of(tags: impl Into<Tags>) -> TagFilter {
        Self::AnyOf(tags.into())
    }

    /// Returns `true` if this filter allows the given set of tags.
    #[deprecated(
        since = "0.2.0",
        note = "use `Tags::matches` and `Tag::matches` instead"
    )]
    pub fn allows(&self, tags: &Tags) -> bool {
        tags.matches(self)
    }
}

impl Default for TagFilter {
    fn default() -> Self {
        Self::any()
    }
}

impl<T: Into<Tags>> From<T> for TagFilter {
    fn from(tags: T) -> Self {
        Self::Equal(tags.into())
    }
}

impl<T: Into<TagFilter>> BitAnd<T> for TagFilter {
    type Output = Self;

    fn bitand(self, rhs: T) -> Self::Output {
        Self::And(Box::new(self), Box::new(rhs.into()))
    }
}

impl<T: Into<TagFilter>> BitAndAssign<T> for TagFilter {
    fn bitand_assign(&mut self, rhs: T) {
        // TODO: Can this be optimized to avoid cloning?
        *self = self.clone() & rhs;
    }
}

impl<T: Into<TagFilter>> BitOr<T> for TagFilter {
    type Output = Self;

    fn bitor(self, rhs: T) -> Self::Output {
        Self::Or(Box::new(self), Box::new(rhs.into()))
    }
}

impl<T: Into<TagFilter>> BitOrAssign<T> for TagFilter {
    fn bitor_assign(&mut self, rhs: T) {
        // TODO: Can this be optimized to avoid cloning?
        *self = self.clone() | rhs;
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

    fn reflect_clone(&self) -> Result<Box<dyn Reflect>, ReflectCloneError> {
        (**self).reflect_clone()
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

/// A macro for creating a [`TagFilter`] from a "tag expression".
///
/// #### ⚠️ Warning
///
/// This macro is experimental and in development!
/// It is possible that the syntax might change in the future, or that some cases are not handled.
/// Please report any issues you encounter or improvements you would like to see.
///
/// # Usage
///
/// A tag expression is a sequence of tag patterns separated by `&` or `|` operators.
///
/// A tag pattern is a list of tags with some special operators:
///
/// - `[]` - Empty pattern, matches no tags.
/// - `[..]` - Wildcard pattern, matches any tags.
/// - `[A, B]` - Matches exactly the tags `A` and `B`.
/// - `[A, B, ..]` - Matches any tags that contain `A` and `B`, but may also contain other tags.
/// - `[A | B]` - Matches any tags that contain either `A` or `B`.
/// - `![A, B]` - Matches any tags that contain neither `A`, nor `B`.
///
/// These patterns may be chained with `&` and `|` operators to create complex filters, such as:
///
/// - `[A, B, ..] & [C, ..]` - Matches tags that contain both `A` and `B`, and also contain `C`.
/// - `[A, B, ..] | [C, ..]` - Matches tags that contain either `A` and `B`, or contain `C`.
///
/// # Examples
///
/// ```rust
/// use moonshine_tag::prelude::*;
///
/// tags! { A, B, C };
///
/// let _: TagFilter = tag_filter!([A, B] | [C, ..]); // Matches any tags that are exactly `A` and `B`, or any tags that contain `C`.
/// let _: TagFilter = tag_filter!(![C]);             // Matches any tags which do not contain `C`.
/// ```
#[macro_export]
macro_rules! tag_filter {
    ([$($tags:ident),* $(,)?]) => {
        $crate::TagFilter::equal([$($tags),*])
    };
    ([$($tags:ident)|* $(|)?]) => {
        $crate::TagFilter::any_of([$($tags),*])
    };
    ([$($tags:ident),* $(,)? ..]) => {
        $crate::TagFilter::all_of([$($tags),*])
    };
    ($first:tt & $($rest:tt)+) => {
        $crate::tag_filter!($first) & $crate::tag_filter!($($rest)+)
    };
    ($first:tt | $($rest:tt)+) => {
        $crate::tag_filter!($first) | $crate::tag_filter!($($rest)+)
    };
    (!$first:tt & $($rest:tt)+) => {
        !$crate::tag_filter!($first) & $crate::tag_filter!($($rest)+)
    };
    (!$first:tt | $($rest:tt)+) => {
        !$crate::tag_filter!($first) | $crate::tag_filter!($($rest)+)
    };
    (!$first:tt) => {
        !$crate::tag_filter!($first)
    };
}

#[doc(hidden)]
#[deprecated(since = "0.2.0", note = "use `tag_filter!` instead")]
#[macro_export]
macro_rules! filter {
    ($($tt:tt)*) => {
        $crate::tag_filter!($($tt)*);
    };
}

#[test]
fn test_filter_macro_expansion() {
    crate::tags!(A, B, C);

    assert_eq!(tag_filter!([]), TagFilter::none());

    assert_eq!(tag_filter!([A]), TagFilter::equal([A]));
    assert_eq!(tag_filter!([A, B]), TagFilter::equal([A, B]));
    assert_eq!(tag_filter!([A, B]), TagFilter::equal([B, A]));
    assert_eq!(tag_filter!([A, B,]), TagFilter::equal([A, B]));

    assert_eq!(tag_filter!([..]), TagFilter::any());

    assert_eq!(tag_filter!([A, ..]), TagFilter::all_of([A]));
    assert_eq!(tag_filter!([A, B, ..]), TagFilter::all_of([A, B]));
    assert_eq!(tag_filter!([A, B, ..]), TagFilter::all_of([B, A]));

    assert_eq!(tag_filter!([A | B]), TagFilter::any_of([A, B]));
    assert_eq!(tag_filter!([A | B]), TagFilter::any_of([A, B]));
    assert_eq!(tag_filter!([A | B |]), TagFilter::any_of([A, B]));

    assert_eq!(tag_filter!(![]), !TagFilter::none());
    assert_eq!(tag_filter!(![A]), !TagFilter::equal([A]));
    assert_eq!(tag_filter!(![A, ..]), !TagFilter::all_of([A]));
    assert_eq!(tag_filter!(![A | B]), !TagFilter::any_of([A, B]));

    assert_eq!(
        tag_filter!([A, B] & [C, ..]),
        TagFilter::equal([A, B]) & TagFilter::all_of([C])
    );

    assert_eq!(
        tag_filter!([A, B] | [C, ..]),
        TagFilter::equal([A, B]) | TagFilter::all_of([C])
    );

    assert_eq!(
        tag_filter!([A, B] & [C, ..] & [A, ..]),
        TagFilter::equal([A, B]) & (TagFilter::all_of([C]) & TagFilter::all_of([A]))
    );

    assert_eq!(
        tag_filter!([A, B] | [C, ..] | [A, ..]),
        TagFilter::equal([A, B]) | (TagFilter::all_of([C]) | TagFilter::all_of([A]))
    );

    assert_eq!(
        tag_filter!([A, B] & [C, ..] | [A, ..]),
        TagFilter::equal([A, B]) & (TagFilter::all_of([C]) | TagFilter::all_of([A]))
    );

    assert_eq!(
        tag_filter!([A, B] | [C, ..] & [A, ..]),
        TagFilter::equal([A, B]) | TagFilter::all_of([C]) & TagFilter::all_of([A])
    );

    assert_eq!(
        tag_filter!([A, B, ..] & [C, ..]),
        TagFilter::all_of([A, B]) & TagFilter::all_of([C])
    );

    assert_eq!(
        tag_filter!([A, B, ..] | [C]),
        TagFilter::all_of([A, B]) | TagFilter::equal([C])
    );

    assert_eq!(
        tag_filter!([A, B] & ![C, ..]),
        TagFilter::equal([A, B]) & !TagFilter::all_of([C])
    );

    assert_eq!(
        tag_filter!(![A, B] & [C, ..]),
        !TagFilter::equal([A, B]) & TagFilter::all_of([C])
    );

    assert_eq!(
        tag_filter!(![A, B] & ![C, ..]),
        !TagFilter::equal([A, B]) & !TagFilter::all_of([C])
    );
}
