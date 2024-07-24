# 🏷️ Moonshine Tag

[![crates.io](https://img.shields.io/crates/v/moonshine-tag)](https://crates.io/crates/moonshine-tag)
[![downloads](https://img.shields.io/crates/dr/moonshine-tag?label=downloads)](https://crates.io/crates/moonshine-tag)
[![docs.rs](https://docs.rs/moonshine-tag/badge.svg)](https://docs.rs/moonshine-tag)
[![license](https://img.shields.io/crates/l/moonshine-tag)](https://github.com/Zeenobit/moonshine_tag/blob/main/LICENSE)
[![stars](https://img.shields.io/github/stars/Zeenobit/tag)](https://github.com/Zeenobit/moonshine_tag)

Cheap, fast, mostly unique identifiers designed for [Bevy](https://github.com/bevyengine/bevy).

## Overview

A [`Tag`] represents a cheap, generic, somewhat unique identifier which may be used to associate "things" with each other or to dynamically flag entities.

```rust
use bevy::prelude::*;
use moonshine_tag::{prelude::*, self as tag};

tags! { APPLE, ORANGE, JUICY, CRUNCHY, POISONED }

let mut world = World::new();

// Spawn some fruits!
let a = world.spawn([APPLE, CRUNCHY].into_tags()).id();
let b = world.spawn([ORANGE, JUICY].into_tags()).id();
let c = world.spawn([APPLE, CRUNCHY, POISONED].into_tags()).id();

// Only crunchy, edible apples, please! :)
let filter: TagFilter = (APPLE & CRUNCHY) & !POISONED;

assert!(filter.allows(world.tags(a)));

assert!(!filter.allows(world.tags(b)));
assert!(!filter.allows(world.tags(c)));
```

### Features

- Tags are cheap to create, cheap to copy, cheap to compare and "unique enough". It's *just* a `u64`.
- Serialization support for both tags and tag filters
- Ability to define complex tag filter expressions
- Simple implementation with no boilerplate and no procedural macros 🧘

## Usage

### Tags

You may define tags from any arbitrary string:

```rust
use moonshine_tag::prelude::*;

tags! { A0 }; // Convenient macro

const A1: Tag = Tag::new("A"); // Manual constant

let a2 = Tag::new("A"); // Runtime

assert_eq!(A0, A1);
assert_eq!(A0, a2);
```

Any two tags with the same name are considered equal.

[`Tags`] is a specialized collection for managing sets of tags:

```rust
use moonshine_tag::prelude::*;

tags! { A, B, C }

let a: Tags = A.into_tags();
let ab = [A, B].into_tags();
let c = C.into_tags();
let ac = a.union(c);
```

### Tag Filters

A [`TagFilter`] is used to test if a given [`Tags`] set matches a certain pattern:

```rust
use moonshine_tag::prelude::*;

tags! { A, B, C }

let a = A.into_tags();
let c = C.into_tags();

let a_or_b: TagFilter = A | B;

assert!(a_or_b.allows(a));
assert!(!a_or_b.allows(c));
```

Tag filters may be combined which each other to create complex expressions:

```rust
use moonshine_tag::prelude::*;

tags! { A, B, C, D }

let ab = [A, B].into_tags();
let c = C.into_tags();
let cd = [C, D].into_tags();

let filter = ([A, B] | C) & D;

assert!(!filter.allows(ab));
assert!(!filter.allows(c));
assert!(filter.allows(cd));
```

## Limitations and Guidelines

Internally, tags are just an [FNV-1a](https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function) ([Why?](https://softwareengineering.stackexchange.com/a/145633)) hash of their string representation. This makes them very cheap to use, but this means they are **NOT** guaranteed to be unique.

It is the assumption of this library that in most game application domains, this is a minor and unlikely problem.

In most applications, the chance of collision between two different tags within the same subsystem is very low, non-fatal, and easily correctable (just rename one of the tags!).

However, you should **NOT** use tags for any cryptographic purposes, or as globally unique identifiers.

Instead, prefer to use them for convenient, dynamic pattern matching or flagging "things" within your systems, especially entities.

[`Tag`]:https://docs.rs/moonshine-kind/latest/moonshine_tag/struct.Tag.html
[`Tags`]:https://docs.rs/moonshine-kind/latest/moonshine_tag/struct.Tags.html
[`TagFilter`]:https://docs.rs/moonshine-kind/latest/moonshine_tag/struct.Tags.html