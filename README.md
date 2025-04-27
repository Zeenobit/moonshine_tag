# 🏷️ Moonshine Tag

[![crates.io](https://img.shields.io/crates/v/moonshine-tag)](https://crates.io/crates/moonshine-tag)
[![downloads](https://img.shields.io/crates/dr/moonshine-tag?label=downloads)](https://crates.io/crates/moonshine-tag)
[![docs.rs](https://docs.rs/moonshine-tag/badge.svg)](https://docs.rs/moonshine-tag)
[![license](https://img.shields.io/crates/l/moonshine-tag)](https://github.com/Zeenobit/moonshine_tag/blob/main/LICENSE)
[![stars](https://img.shields.io/github/stars/Zeenobit/moonshine_tag)](https://github.com/Zeenobit/moonshine_tag)

Cheap, fast, mostly unique identifiers designed for [Bevy](https://github.com/bevyengine/bevy).

## Overview

A [`Tag`] represents a cheap, generic, somewhat unique identifier which may be used to associate "things" with each other or to dynamically flag entities.

```rust
use bevy::prelude::*;
use moonshine_tag::{prelude::*, filter, Filter};

tags! { APPLE, ORANGE, JUICY, CRUNCHY, POISONED }

let mut world = World::new();

// Define some fruits!
let fruits = [
    Tags::from([APPLE, CRUNCHY]),
    Tags::from([ORANGE, JUICY]),
    Tags::from([APPLE, CRUNCHY, POISONED])
];

// Only crunchy, edible apples, please! :)
let filter: Filter = filter!([APPLE, CRUNCHY]) & filter!(![POISONED]);

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
- Simple implementation with no boilerplate and no procedural macros 🧘

## Usage

### Tags

You may define [`Tags`] from any arbitrary string:

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

A tag [`Filter`] is used to test if a given [`Tags`] set matches a certain pattern:

```rust
use moonshine_tag::{prelude::*, Filter, filter};

tags! { A, B, C }

let a = Tags::from(A);
let c = Tags::from(C);

let a_or_b: Filter = Filter::any_of([A, B]);

assert!(a_or_b.allows(&a));
assert!(!a_or_b.allows(&c));
```

Tag filters may be combined which each other to create complex expressions:

```rust
use moonshine_tag::{prelude::*, Filter, filter};

tags! { A, B, C, D }

let ab = Tags::from([A, B]);
let cd = Tags::from([C, D]);
let c = Tags::from(C);

let filter = (Filter::all_of([A, B]) | Filter::any_of([C, B])) & Filter::any_of(D);

assert!(!filter.allows(&ab));
assert!(!filter.allows(&c));
assert!(filter.allows(&cd));
```

There is also a convenient `filter!` macro for constructing tag filters from tag expressions:

```rust
use moonshine_tag::{prelude::*, Filter, filter};

tags! { A, B, C, D }

let _: Filter = filter!([A, B, ..]);       // Matches any tag set containing A or B
let _: Filter = filter!([A, B, ..] | [C]); // Matches any tag set which contains A or B, or exactly C
let _: Filter = filter!(![C]);             // Matches any tag set not containing C
```

⚠️ This macro is still in development.

## Limitations and Guidelines

Internally, tags are just an [FNV-1a](https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function) ([Why?](https://softwareengineering.stackexchange.com/a/145633)) hash of their string representation. This makes them very cheap to use, but this means they are **NOT** guaranteed to be unique.

It is the assumption of this library that in most game application domains, this is a minor and unlikely problem.

In most applications, the chance of collision between two different tags within the same subsystem is very low, non-fatal, and easily correctable (just rename one of the tags!).

However, you should **NOT** use tags for any cryptographic purposes, or as globally unique identifiers.

Instead, prefer to use them for convenient, dynamic pattern matching or flagging "things" within your systems, especially entities.

## Installation

Add the following to your `Cargo.toml`:

```toml
[dependencies]
moonshine-tag = "0.2.0"
```

This crate is also included as part of [🍸 Moonshine Core](https://github.com/Zeenobit/moonshine_core).

## Support

Please [post an issue](https://github.com/Zeenobit/moonshine_tag/issues/new) for any bugs, questions, or suggestions.

You may also contact me on the official [Bevy Discord](https://discord.gg/bevy) server as **@Zeenobit**.


[`Tag`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/struct.Tag.html
[`Tags`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/struct.Tags.html
[`Filter`]:https://docs.rs/moonshine-tag/latest/moonshine_tag/struct.Filter.html
[`Component`]:https://docs.rs/bevy/latest/bevy/ecs/component/trait.Component.html