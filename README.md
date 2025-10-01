# 🏷️ Moonshine Tag

[![crates.io](https://img.shields.io/crates/v/moonshine-tag)](https://crates.io/crates/moonshine-tag)
[![downloads](https://img.shields.io/crates/dr/moonshine-tag?label=downloads)](https://crates.io/crates/moonshine-tag)
[![docs.rs](https://docs.rs/moonshine-tag/badge.svg)](https://docs.rs/moonshine-tag)
[![license](https://img.shields.io/crates/l/moonshine-tag)](https://github.com/Zeenobit/moonshine_tag/blob/main/LICENSE)
[![stars](https://img.shields.io/github/stars/Zeenobit/moonshine_tag)](https://github.com/Zeenobit/moonshine_tag)

Cheap, fast, mostly unique identifiers designed for [Bevy](https://github.com/bevyengine/bevy).

This crate is also included as part of [🍸 Moonshine Core](https://github.com/Zeenobit/moonshine_core).

## Overview

A [`Tag`] represents a cheap, generic, somewhat unique identifier which may be used to associate "things" with each other or to dynamically flag entities.

```rust
use bevy::prelude::*;
use moonshine_tag::prelude::*;

tags! { APPLE, ORANGE, JUICY, CRUNCHY, POISONED }

let mut world = World::new();

// Define some fruits!
let fruits = [
    Tags::from([APPLE, CRUNCHY]),
    Tags::from([ORANGE, JUICY]),
    Tags::from([APPLE, CRUNCHY, POISONED])
];

// Only crunchy, edible apples, please! :)
let filter: TagFilter = tag_filter!([APPLE, CRUNCHY] & ![POISONED]);

for fruit in &fruits {
    if filter.allows(fruit) {
        world.spawn(fruit.clone());
    }
}

# assert!(filter.allows(&fruits[0]));
```

### Features

- Tags are cheap to create, cheap to copy, cheap to compare and "unique enough". It's *just* a `u64`.
- Serialization support for both tags and tag filters
- Ability to define complex tag filter expressions
- Reverse lookup of tag names without any manual regitration
- Simple implementation with no boilerplate and no procedural macros 🧘

## Usage

### Tags

You may create a [`Tag`] from any arbitrary string:

```rust
use moonshine_tag::prelude::*;

tags! { A }; // Convenient macro

const A1: Tag = Tag::new("A"); // Manual constant

let a2 = Tag::new("A"); // Runtime

assert_eq!(A, A1);
assert_eq!(A, a2);
```

Any two tags with the same name are considered equal.

[`Tags`] is a specialized collection for managing sets of tags:

```rust
use moonshine_tag::prelude::*;

tags! { A, B, C }

let a = Tags::from(A);
let c = Tags::from([C]);
let ab = Tags::from([A, B]);
let ac = a.union(c);
```

[`Tags`] may be used as a [`Component`] or on its own as a generic collection of tags.

### Tag Filters

A tag [`TagFilter`] is used to test if a given [`Tags`] set matches a certain pattern:

```rust
use moonshine_tag::prelude::*;

tags! { A, B, C }

let a = Tags::from(A);
let c = Tags::from(C);

let a_or_b: TagFilter = TagFilter::any_of([A, B]);

assert!(a_or_b.allows(&a));
assert!(!a_or_b.allows(&c));
```

Tag filters may be combined which each other to create complex expressions:

```rust
use moonshine_tag::prelude::*;

tags! { A, B, C, D }

let ab = Tags::from([A, B]);
let cd = Tags::from([C, D]);
let c = Tags::from(C);

let filter = (TagFilter::all_of([A, B]) | TagFilter::any_of([C, B])) & TagFilter::any_of(D);

assert!(!filter.allows(&ab));
assert!(!filter.allows(&c));
assert!(filter.allows(&cd));
```

There is also a convenient `filter!` macro for constructing tag filters from tag expressions:

```rust
use moonshine_tag::prelude::*;

tags! { A, B, C, D }

let _: TagFilter = tag_filter!([A, B, ..]);       // Matches any tag set containing A or B
let _: TagFilter = tag_filter!([A, B, ..] | [C]); // Matches any tag set which contains A or B, or exactly C
let _: TagFilter = tag_filter!(![C]);             // Matches any tag set not containing C
```

⚠️ This macro is still in development.

## Tag Names

When debugging or implementing tools, it is often useful to have some human-friendly representation of tags.

There are two methods provided to for human-friendly identification of tags:
- [`pretty_hash`]
    - This returns a base31-encoded string representation of the tag's hash value.
    - This is generally fast and suitable for most use cases.
- [`resolve_name`]
    - This returns the actual name of the tag.
    - This is slow as it performs a linear search through all defined tags.
        - If performance is a concern, consider using [`pretty_hash`] or cache the names in memory.
    - The tag must be defined using the [`tags!`] macro for this to work, otherwise [`pretty_hash`] is returned as fallback.

## Limitations and Guidelines

Internally, tags are just an [FNV-1a](https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function) ([Why?](https://softwareengineering.stackexchange.com/a/145633)) hash of their string representation. This makes them very cheap to use, but this means they are **NOT** guaranteed to be unique.

It is the assumption of this library that in most game application domains, this is a minor and unlikely problem.

In most applications, the chance of collision between two different tags within the same subsystem is very low, non-fatal, and easily correctable (just rename one of the tags!).

However, you should **NOT** use tags for any cryptographic purposes, or as globally unique identifiers.

Instead, prefer to use them for convenient, dynamic pattern matching or flagging "things" within your systems, especially entities.

## Changes

### Version 0.3

- Renamed `Filter` back to `TagFilter`
    - `tag::Filter` and `tag::filter!` was a cute idea, but when `moonshine_tag` is used on its own, it would force the user to either import `moonshine_tag as tag` or use `moonshine_tag::Filter`. Both are ugly.
- Flipped the `allows` functions into `matches` functions
    - Better symmetry with the rest of Rust API
    - Enables an optimization when checking against single tags
- Added methods for human-friendly tag identification:
    - [`pretty_hash`]
    - [`resolve_name`]
    - [`to_pretty_string`] for `Tags` and `TagFilter`
- Support for mixed expressions in `tag_filter!`
    - See tests for examples


## Support

Please [post an issue](https://github.com/Zeenobit/moonshine_tag/issues/new) for any bugs, questions, or suggestions.

You may also contact me on the official [Bevy Discord](https://discord.gg/bevy) server as **@Zeenobit**.


[`Tag`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/struct.Tag.html
[`resolve_name`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/struct.Tag.html#method.resoleve_name
[`pretty_hash`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/struct.Tag.html#method.pretty_hash
[`tags!`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/macro.tags.html
[`Tags`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/struct.Tags.html
[`TagFilter`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/filter/struct.TagFilter.html
[`to_pretty_string`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/struct.Tags.html#to_pretty_string
[`Component`]:https://docs.rs/bevy/latest/bevy/ecs/component/trait.Component.html