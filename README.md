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
