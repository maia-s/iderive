# iderive: Inner Derive

`iderive` is a drop-in replacement for `derive` that doesn't directly depend
on generic bounds. It only checks the types of a struct's fields when deriving
a trait.

## Example
```rust
#[derive(Clone, Copy)]
struct TaggedIndex<T: ?Sized> {
    index: usize,
    _tag: PhantomData<T>,
}

let a = TaggedIndex::<String> { index: 0, _tag: PhantomData };
let b = a;
let c = a; // Error: Value used after move
```
This won't work because `derive` requires that `T` implements `Copy` for
`TaggedIndex` to be able to derive it.

In contrast, `iderive` only checks the struct's fields to determine if a
trait can be derived. Because `usize` and `PhantomData<T>` implements `Copy`
regardless of the type of `T`, `iderive(Copy)` will implement `Copy` for
`TaggedIndex`:

```rust
#[iderive(Clone, Copy)]
struct TaggedIndex<T: ?Sized> {
    index: usize,
    _tag: PhantomData<T>,
}

let a = TaggedIndex::<String> { index: 0, _tag: PhantomData };
let b = a;
let c = a; // Works!
```

## Supported traits
`iderive` is currently implemented for `Clone`, `Copy`, `Debug`,
`Default`, `PartialEq`, `Eq`, `PartialOrd`, `Ord` and `Hash`.

## Version history
- 1.2.1
  - Fix parsing of field visibility
- 1.2.0
  - Rewrite; iderive now has no dependencies
  - Don't use canonical implementations, because this breaks if the other trait fails bounds
- 1.1.2
  - Remove the non-exhaustive support added in 1.1.1 as it doesn't make sense when all
    fields are displayed anyway. This matches the output of `#[derive(Debug)]`
- 1.1.1
  - Indicate non-exhausiveness in the output of the `Debug` trait for named structs
  - Guard against redefinitions of the `bool` type
  - Don't require syn's `full` feature
  - Add more license options
- 1.1.0
  - Use canonical implementations of `Clone`/`PartialOrd` if `Copy`/`Ord` is also derived
  - Update to syn 2.0
- 1.0.0
  - Remove debug output that was left in by accident
- 0.1.0
  - First release
