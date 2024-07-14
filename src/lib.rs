// Copyright 2021-2024 Maia S. R. <66437537+maia-s@users.noreply.github.com>
// (See accompanying files LICENSE*.md for license options)

//! # iderive: Inner Derive
//!
//! `iderive` is a drop-in replacement for `derive` that doesn't directly depend
//! on generic bounds. It only checks the types of a struct's fields when deriving
//! a trait.
//!
//! ## Example
//! ```compile_fail
//! # use core::marker::PhantomData;
//! #[derive(Clone, Copy)]
//! struct TaggedIndex<T: ?Sized> {
//!     index: usize,
//!     _tag: PhantomData<T>,
//! }
//!
//! let a = TaggedIndex::<String> { index: 0, _tag: PhantomData };
//! let b = a;
//! let c = a; // Error: Value used after move
//! ```
//! This won't work because `derive` requires that `T` implements `Copy` for
//! `TaggedIndex` to be able to derive it.
//!
//! In contrast, `iderive` only checks the struct's fields to determine if a
//! trait can be derived. Because `usize` and `PhantomData<T>` implements `Copy`
//! regardless of the type of `T`, `iderive(Copy)` will implement `Copy` for
//! `TaggedIndex`:
//!
//! ```
//! # #[macro_use] extern crate iderive;
//! # use core::marker::PhantomData;
//! #[iderive(Clone, Copy)]
//! struct TaggedIndex<T: ?Sized> {
//!     index: usize,
//!     _tag: PhantomData<T>,
//! }
//!
//! let a = TaggedIndex::<String> { index: 0, _tag: PhantomData };
//! let b = a;
//! let c = a; // Works!
//! ```
//!
//! ## Supported traits
//! `iderive` is currently implemented for `Clone`, `Copy`, `Debug`,
//! `Default`, `PartialEq`, `Eq`, `PartialOrd`, `Ord` and `Hash`.
//!
//! ## Version history
//! See README.md for version history.

#![allow(clippy::blocks_in_conditions)]
//#![recursion_limit = "1024"]

macro_rules! Op {
    ($op:tt) => {
        $crate::parse::Op<
            { $crate::parse::into_ascii_char_first(stringify!($op)) },
            { $crate::parse::into_ascii_char_second(stringify!($op)) },
        >
    };
}

macro_rules! miniquote {
    (@@ $q:tt,) => {};

    (@@ $q:tt, # $i:ident $($rest:tt)*) => {{
        $i.into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    }};

    (@@ $q:tt, $ident:ident $($rest:tt)*) => {{
        $q.extend([TokenTree::Ident(Ident::new(stringify!($ident), Span::mixed_site()))]);
        miniquote!(@@ $q, $($rest)*);
    }};

    (@@ $q:tt, ( $($tt:tt)* ) $($rest:tt)*) => {{
        $q.extend([TokenTree::Group(Group::new(Delimiter::Parenthesis, miniquote!($($tt)*)))]);
        miniquote!(@@ $q, $($rest)*);
    }};

    (@@ $q:tt, { $($tt:tt)* } $($rest:tt)*) => {{
        $q.extend([TokenTree::Group(Group::new(Delimiter::Brace, miniquote!($($tt)*)))]);
        miniquote!(@@ $q, $($rest)*);
    }};

    (@@ $q:tt, [ $($tt:tt)* ] $($rest:tt)*) => {{
        $q.extend([TokenTree::Group(Group::new(Delimiter::Bracket, miniquote!($($tt)*)))]);
        miniquote!(@@ $q, $($rest)*);
    }};

    (@@ $q:tt, # $($rest:tt)*) => {
        <Op![#]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, && $($rest:tt)*) => {
        <Op![&&]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, & $($rest:tt)*) => {
        <Op![&]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, :: $($rest:tt)*) => {
        <Op![::]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, : $($rest:tt)*) => {
        <Op![:]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, -> $($rest:tt)*) => {
        <Op![->]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, => $($rest:tt)*) => {
        <Op![=>]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, . $($rest:tt)*) => {
        <Op![.]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, , $($rest:tt)*) => {
        <Op![,]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, ; $($rest:tt)*) => {
        <Op![;]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, < $($rest:tt)*) => {
        <Op![<]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $q:tt, > $($rest:tt)*) => {
        <Op![>]>::default().into_token_trees($q);
        miniquote!(@@ $q, $($rest)*);
    };

    (@@ $($tt:tt)*) => {
        compile_error!("unhandled token")
    };

    ($($tt:tt)*) => {{
        let mut __quote = TokenStream::new();
        miniquote!(@@ {&mut __quote}, $($tt)*);
        __quote
    }};
}

mod parse;

use parse::*;
use proc_macro::{Delimiter, Group, Ident, Literal, Span, TokenStream, TokenTree};

/// Inner derive. Used like `derive`. See the crate documentation for details.
#[proc_macro_attribute]
pub fn iderive(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut output = TokenStream::new();
    let args = &mut { into_parse_input(args) };
    let input = &mut { into_parse_input(input) };

    match (|| -> Result<(), Error> {
        let traits = Punctuated::<Ident, Op![,]>::parse(args)?;
        if let Some(tt) = args.next() {
            return Err(Error::with_span(tt.span(), "unrecognized argument"));
        }

        let attrs = Attributes::parse(input)?;
        let vis = Visibility::try_parse(input)?;
        if let Some(mut s) = Struct::try_parse(input)? {
            s.set_attrs_and_vis(attrs, vis);

            if let Some(union_kw) = s.union_kw {
                return Err(Error::with_span(
                    union_kw.span(),
                    "iderive can't be used on unions",
                ));
            }

            for trait_id in traits.into_values() {
                let trait_s = trait_id.to_string();
                match trait_s.as_str() {
                    "Clone" => {
                        let is_unit_struct = s.is_unit_struct();
                        impl_for_struct(
                            &mut output,
                            &mut s,
                            Path::for_ident("::core::clone", trait_id)?,
                            move |body| {
                                if is_unit_struct {
                                    miniquote! {
                                        fn clone(&self) -> Self {
                                            Self
                                        }
                                    }
                                } else {
                                    miniquote! {
                                        fn clone(&self) -> Self {
                                            Self { #body }
                                        }
                                    }
                                }
                            },
                            |fid, _| {
                                miniquote! {
                                    #fid: self.#fid.clone(),
                                }
                            },
                        )
                    }

                    "Copy" => impl_for_struct(
                        &mut output,
                        &mut s,
                        Path::for_ident("::core::marker", trait_id)?,
                        |_| TokenStream::new(),
                        |_, _| TokenStream::new(),
                    ),

                    "Debug" => {
                        let struct_name = s.ident.to_string();
                        let is_unit_struct = s.is_unit_struct();
                        let is_tuple_struct = s.is_tuple_struct();
                        impl_for_struct(
                            &mut output,
                            &mut s,
                            Path::for_ident("::core::fmt", trait_id)?,
                            move |body| {
                                if is_unit_struct {
                                    miniquote! {
                                        fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                                            fmt.write_str(#struct_name)
                                        }
                                    }
                                } else {
                                    let method = Ident::new(
                                        if is_tuple_struct {
                                            "debug_tuple"
                                        } else {
                                            "debug_struct"
                                        },
                                        Span::call_site(),
                                    );
                                    miniquote! {
                                        fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                                            fmt.#method(#struct_name) #body .finish()
                                        }
                                    }
                                }
                            },
                            |fid, field| {
                                if let Some(name) = &field.name {
                                    let name = name.to_string();
                                    miniquote! {
                                        .field(#name, &self.#fid)
                                    }
                                } else {
                                    miniquote! {
                                        .field(&self.#fid)
                                    }
                                }
                            },
                        )
                    }

                    "Default" => {
                        let is_unit_struct = s.is_unit_struct();
                        impl_for_struct(
                            &mut output,
                            &mut s,
                            Path::for_ident("::core::default", trait_id)?,
                            |body| {
                                if is_unit_struct {
                                    miniquote! {
                                        fn default() -> Self {
                                            Self
                                        }
                                    }
                                } else {
                                    miniquote! {
                                        fn default() -> Self {
                                            Self { #body }
                                        }
                                    }
                                }
                            },
                            |fid, _| {
                                miniquote! {
                                    #fid: ::core::default::Default::default(),
                                }
                            },
                        )
                    }

                    "PartialEq" => impl_for_struct(
                        &mut output,
                        &mut s,
                        Path::for_ident("::core::cmp", trait_id)?,
                        |body| {
                            miniquote! {
                                fn eq(&self, other: &Self) -> ::core::primitive::bool {
                                    true #body
                                }
                            }
                        },
                        |fid, _| {
                            miniquote! {
                                && self.#fid.eq(&other.#fid)
                            }
                        },
                    ),

                    "Eq" => impl_for_struct(
                        &mut output,
                        &mut s,
                        Path::for_ident("::core::cmp", trait_id)?,
                        |_| TokenStream::new(),
                        |_, _| TokenStream::new(),
                    ),

                    "PartialOrd" => impl_for_struct(
                        &mut output,
                        &mut s,
                        Path::for_ident("::core::cmp", trait_id)?,
                        |body| {
                            miniquote! {
                                fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                                    #body
                                    ::core::option::Option::Some(::core::cmp::Ordering::Equal)
                                }
                            }
                        },
                        |fid, _| {
                            miniquote! {
                                match self.#fid.partial_cmp(&other.#fid) {
                                    ::core::option::Option::Some(::core::cmp::Ordering::Equal) => (),
                                    result => return result,
                                }
                            }
                        },
                    ),

                    "Ord" => impl_for_struct(
                        &mut output,
                        &mut s,
                        Path::for_ident("::core::cmp", trait_id)?,
                        |body| {
                            miniquote! {
                                fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                                    #body
                                    ::core::cmp::Ordering::Equal
                                }
                            }
                        },
                        |fid, _| {
                            miniquote! {
                                match self.#fid.cmp(&other.#fid) {
                                    ::core::cmp::Ordering::Equal => (),
                                    result => return result,
                                }
                            }
                        },
                    ),

                    "Hash" => impl_for_struct(
                        &mut output,
                        &mut s,
                        Path::for_ident("::core::hash", trait_id)?,
                        |body| {
                            miniquote! {
                                fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) {
                                    #body
                                }
                            }
                        },
                        |fid, _| {
                            miniquote! {
                                self.#fid.hash(state);
                            }
                        },
                    ),

                    _ => {
                        return Err(Error::with_span(
                            trait_id.span(),
                            format!("the trait `{trait_s}` can't be derived with iderive"),
                        ))
                    }
                }
            }

            s.into_token_trees(&mut output);
            Ok(())
        } else if let Some(tt) = input.next() {
            if let TokenTree::Ident(i) = &tt {
                if i.to_string() == "enum" {
                    return Err(Error::with_span(
                        tt.span(),
                        "enums aren't currently supported by iderive",
                    ));
                }
            }
            Err(Error::with_span(tt.span(), "expected struct"))
        } else {
            Err(Error::new("unexpected end of input; expected struct"))
        }
    })() {
        Ok(()) => (),
        Err(e) => e.into_token_trees(&mut output),
    }

    output
}

fn impl_for_struct(
    output: &mut impl Extend<TokenTree>,
    s: &mut Struct,
    trait_path: Path,
    per_impl: impl FnOnce(Vec<TokenTree>) -> TokenStream,
    per_field: impl Fn(&TokenTree, &Field) -> TokenStream,
) {
    let mut where_clauses = s.where_clauses.clone().unwrap_or_default();
    let mut body = Vec::new();

    for (i, field) in s.fields.iter().enumerate() {
        let fid = if let Some(name) = &field.name {
            TokenTree::Ident(name.clone())
        } else {
            TokenTree::Literal(Literal::usize_unsuffixed(i))
        };
        body.extend(per_field(&fid, field));

        let mut clause = Vec::new();
        field.ty.to_token_trees(&mut clause);
        <Op![:]>::default().into_token_trees(&mut clause);
        trait_path.to_token_trees(&mut clause);
        where_clauses.push(clause.into());
    }

    let ident = &s.ident;
    let generics = s.generics.clone().map(|mut g| {
        g.remove_init();
        g
    });
    let generics_sym_only = generics.as_ref().map(GenericsSymOnly::from);
    let body = per_impl(body);

    output.extend(miniquote! {
        #[automatically_derived]
        impl #generics #trait_path for #ident #generics_sym_only #where_clauses {
            #body
        }
    });
}
