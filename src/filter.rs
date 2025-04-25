use std::{
    any::Any,
    ops::{BitAnd, BitOr, Not},
};

use bevy_reflect::ReflectCloneError;
use bevy_reflect::{
    prelude::*, utility::NonGenericTypeInfoCell, ApplyError, GetTypeRegistration, OpaqueInfo,
    ReflectMut, ReflectOwned, ReflectRef, TypeInfo, TypePath, TypeRegistration, Typed,
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
/// use moonshine_tag::{prelude::*, filter, Filter};
///
/// tags! { A, B, C };
///
/// let filter: Filter = filter!([A, B, ..]);
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
pub enum Filter {
    /// Matches any set of tags which is exactly equal to the filter tags.
    Equal(Tags),
    /// Matches any set of tags which contains all of the filter tags.
    AllOf(Tags),
    /// Matches any set of tags which contains any of the filter tags.
    AnyOf(Tags),
    /// Matches any set of tags which matches both inner filters.
    And(FilterDyn, FilterDyn),
    /// Matches any set of tags which matches either inner filters.
    Or(FilterDyn, FilterDyn),
    /// Matches any set of tags which does not match the inner filter.
    Not(FilterDyn),
}

impl Filter {
    pub fn none() -> Filter {
        Self::Equal([].into())
    }

    pub fn equal(tags: impl Into<Tags>) -> Filter {
        Self::Equal(tags.into())
    }

    pub fn any() -> Filter {
        Self::AllOf([].into())
    }

    pub fn all_of(tags: impl Into<Tags>) -> Filter {
        Self::AllOf(tags.into())
    }

    pub fn any_of(tags: impl Into<Tags>) -> Filter {
        Self::AnyOf(tags.into())
    }

    /// Returns `true` if this filter allows the given set of tags.
    pub fn allows(&self, tags: &Tags) -> bool {
        use Filter::*;
        match self {
            Equal(a) => a == tags,
            AllOf(a) => a.is_subset(tags),
            AnyOf(a) => !a.is_disjoint(tags),
            And(a, b) => a.allows(tags) && b.allows(tags),
            Or(a, b) => a.allows(tags) || b.allows(tags),
            Not(a) => !a.allows(tags),
        }
    }
}

impl Default for Filter {
    fn default() -> Self {
        Self::any()
    }
}

impl<T: Into<Tags>> From<T> for Filter {
    fn from(tags: T) -> Self {
        Self::Equal(tags.into())
    }
}

impl<T: Into<Filter>> BitAnd<T> for Filter {
    type Output = Self;

    fn bitand(self, rhs: T) -> Self::Output {
        Self::And(Box::new(self), Box::new(rhs.into()))
    }
}

impl<T: Into<Filter>> BitOr<T> for Filter {
    type Output = Self;

    fn bitor(self, rhs: T) -> Self::Output {
        Self::Or(Box::new(self), Box::new(rhs.into()))
    }
}

impl Not for Filter {
    type Output = Filter;

    fn not(self) -> Self::Output {
        Self::Not(Box::new(self))
    }
}

type FilterDyn = Box<Filter>;

impl PartialReflect for Box<Filter> {
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

impl Reflect for Box<Filter> {
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

impl FromReflect for Box<Filter> {
    fn from_reflect(reflect: &dyn PartialReflect) -> Option<Self> {
        Filter::from_reflect(reflect).map(Box::new)
    }
}

impl TypePath for Box<Filter> {
    fn type_path() -> &'static str {
        Filter::type_path()
    }

    fn short_type_path() -> &'static str {
        Filter::short_type_path()
    }
}

impl Typed for Box<Filter> {
    fn type_info() -> &'static TypeInfo {
        static CELL: NonGenericTypeInfoCell = NonGenericTypeInfoCell::new();
        CELL.get_or_set(|| TypeInfo::Opaque(OpaqueInfo::new::<Self>()))
    }
}

impl GetTypeRegistration for Box<Filter> {
    fn get_type_registration() -> TypeRegistration {
        TypeRegistration::of::<Self>()
    }
}

/// A macro for creating a [`Filter`] from a "tag expression".
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
/// These patterns may be chained with `&` and `|` operators to create complex filters.
///
/// # Examples
///
/// ```rust
/// use moonshine_tag::{prelude::*, self as tag};
///
/// let _: tag::Filter = tag::filter!([A, B] | [C, ..]); // Matches any tags that are exactly `A` and `B`, or any tags that contain `C`.
/// let _: tag::Filter = tag::filter!([A, B, ..] & ![C]); // Matches any tags contain `A` and `B`, but not `C`.
/// ```
///
/// # Limitations
///
/// At the moment, it is not possible to mix `&` and `|` operators in the same expression.
#[macro_export]
macro_rules! filter {
    ($first:tt $(& $rest:tt)+) => {
        $crate::filter!($first) $(& $crate::filter!($rest))*
    };
    ($first:tt $(| $rest:tt)+) => {
        $crate::filter!($first) $(| $crate::filter!($rest))*
    };

    // TODO: Handle mixed terms? Is that possible?

    // Handle empty pattern `[]`
    ([]) => {
        $crate::Filter::none()
    };

    // Handle wildcard `[..]`
    ([..]) => {
        $crate::Filter::any()
    };

    // Handle equal patterns `[A]`,`[A, B]`, ...
    ([$($tags:path),* $(,)?]) => {
        $crate::Filter::equal([$($tags),*])
    };

    // Handle negation `![...]`
    (![$tag:path $(,)?]) => {
        !$crate::Filter::equal([$tag])
    };
    (![$($tag:path),* $(,)?]) => {
        !$crate::Filter::equal([$($tag),*])
    };
    (![$($tag:path),* , .. $(,)?]) => {
        !$crate::Filter::all_of([$($tag),*])
    };

    // Handle any_of pattern `[A | B]`
    ([$($tag:path)|+ $(,)?]) => {
        $crate::Filter::any_of([$($tag),*])
    };

    // Handle all_of pattern `[A, ..]` and `[A, B, ..]`
    ([$tag:path, .. $(,)?]) => {
        $crate::Filter::all_of([$tag])
    };
    ([$($tag:path),+ , .. $(,)?]) => {
        $crate::Filter::all_of([$($tag),*])
    };
}

#[test]
fn test_filter_macro_expansion() {
    crate::tags!(A, B, C);

    assert_eq!(filter!([]), Filter::none());

    assert_eq!(filter!([A]), Filter::equal([A]));
    assert_eq!(filter!([A, B]), Filter::equal([A, B]));
    assert_eq!(filter!([A, B]), Filter::equal([B, A]));
    assert_eq!(filter!([A, B,]), Filter::equal([A, B]));

    assert_eq!(filter!([..]), Filter::any());

    assert_eq!(filter!([A, ..]), Filter::all_of([A]));
    assert_eq!(filter!([A, B, ..]), Filter::all_of([A, B]));
    assert_eq!(filter!([A, B, ..]), Filter::all_of([B, A]));

    assert_eq!(filter!([A | B]), Filter::any_of([A, B]));

    assert_eq!(filter!([A | B]), Filter::any_of([A, B]));

    assert_eq!(
        filter!([A, B] & [C, ..]),
        Filter::equal([A, B]) & Filter::all_of([C])
    );

    assert_eq!(
        filter!([A, B] & [C, ..]),
        Filter::equal([A, B]) & Filter::all_of([C])
    );

    assert_eq!(
        filter!([A, B] & [C, ..] & [A, ..]),
        Filter::equal([A, B]) & Filter::all_of([C]) & Filter::all_of([A])
    );

    assert_eq!(
        filter!([A, B] | [C, ..]),
        Filter::equal([A, B]) | Filter::all_of([C])
    );

    assert_eq!(
        filter!([A, B] | [C, ..] | [A, ..]),
        Filter::equal([A, B]) | Filter::all_of([C]) | Filter::all_of([A])
    );

    assert_eq!(
        filter!([A, B, ..] & [C, ..]),
        Filter::all_of([A, B]) & Filter::all_of([C])
    );

    assert_eq!(
        filter!([A, B, ..] | [C]),
        Filter::all_of([A, B]) | Filter::equal([C])
    );
}
