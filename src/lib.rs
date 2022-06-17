// Copyright 2021 Maia <66437537+maia-s@users.noreply.github.com>
// Distributed under the Boost Software License, version 1.0
// (See accompanying file LICENSE.md)

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

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::{self, Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Error, Fields, Ident, Index, ItemStruct, Token,
};

struct Traits(Punctuated<Ident, Token![,]>);

impl Parse for Traits {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(Self(Punctuated::<Ident, Token![,]>::parse_terminated(
            input,
        )?))
    }
}

struct Input(ItemStruct);

impl Parse for Input {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let mut input = Self(ItemStruct::parse(input)?);
        input.0.generics.make_where_clause();
        if let Some(ref mut wh) = input.0.generics.where_clause {
            if !wh.predicates.empty_or_trailing() {
                wh.predicates.push_punct(Token![,](Span::call_site()));
            }
        }
        Ok(input)
    }
}

impl Input {
    fn begin_trait(
        &self,
        trait_tokens: TokenStream,
        output: &mut TokenStream,
        mut per_field: impl FnMut(TokenStream),
    ) {
        let name = &self.0.ident;
        let (imp, ty, wher) = self.0.generics.split_for_impl();
        let mut wher = wher.unwrap().to_token_stream();
        if wher.is_empty() {
            wher = Token![where](Span::call_site()).to_token_stream();
        }

        output.extend::<TokenStream>(quote! {
            impl #imp #trait_tokens for #name #ty #wher
        });

        for (i, field) in self.0.fields.iter().enumerate() {
            let ty = &field.ty;
            output.extend::<TokenStream>(quote! { #ty: #trait_tokens, });

            per_field(if let Some(name) = &field.ident {
                name.to_token_stream()
            } else {
                Index::from(i).to_token_stream()
            });
        }
    }
}

/// Inner derive. Used like `derive`. See the crate documentation for details.
#[proc_macro_attribute]
pub fn iderive(args: TokenStream1, input: TokenStream1) -> TokenStream1 {
    let mut output = TokenStream::from(input.clone());
    let input = parse_macro_input!(input as Input);
    let traits = parse_macro_input!(args as Traits);

    for id in traits.0 {
        let s = id.to_string();
        match s.as_str() {
            "Clone" => derive_clone(&input, &id, &mut output),
            "Copy" => derive_copy(&input, &id, &mut output),
            "Debug" => derive_debug(&input, &id, &mut output),
            "Default" => derive_default(&input, &id, &mut output),
            "PartialEq" => derive_partialeq(&input, &id, &mut output),
            "Eq" => derive_eq(&input, &id, &mut output),
            "PartialOrd" => derive_partialord(&input, &id, &mut output),
            "Ord" => derive_ord(&input, &id, &mut output),
            "Hash" => derive_hash(&input, &id, &mut output),
            _ => output.extend(
                Error::new(id.span(), format!("Unsupported trait: `{}`", s)).to_compile_error(),
            ),
        }
    }

    output.into()
}

fn derive_clone(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    let mut clone = TokenStream::new();
    input.begin_trait(quote! { ::core::clone::#traitid }, output, |name| {
        clone.extend(quote! { #name: self.#name.clone(), });
    });
    output.extend::<TokenStream>(quote! {{
        #[inline]
        fn clone(&self) -> Self {
            Self { #clone }
        }
    }});
}

fn derive_copy(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    input.begin_trait(quote! { ::core::marker::#traitid }, output, |_| ());
    output.extend::<TokenStream>(quote! {{}});
}

fn derive_debug(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    let mut debug = TokenStream::new();
    input.begin_trait(
        quote! { ::core::fmt::#traitid },
        output,
        |name| match input.0.fields {
            Fields::Named(_) => {
                let name_s = name.to_string();
                debug.extend(quote! { .field(#name_s, &self.#name) })
            }
            Fields::Unnamed(_) => debug.extend(quote! { .field(&self.#name) }),
            Fields::Unit => (),
        },
    );
    let id_s = &input.0.ident.to_string();
    output.extend::<TokenStream>(match input.0.fields {
        Fields::Named(_) => quote! {{
            #[inline]
            fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                fmt.debug_struct(#id_s) #debug .finish()
            }
        }},
        Fields::Unnamed(_) => quote! {{
            #[inline]
            fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                fmt.debug_tuple(#id_s) #debug .finish()
            }
        }},
        Fields::Unit => quote! {{
            #[inline]
            fn fmt(&self, fmt: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                fmt.write_str(#id_s)
            }
        }},
    });
}

fn derive_default(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    let mut default = TokenStream::new();
    input.begin_trait(quote! { ::core::default::#traitid }, output, |name| {
        default.extend(quote! { #name: ::core::default::Default::default(), });
    });
    output.extend::<TokenStream>(quote! {{
        #[inline]
        fn default() -> Self {
            Self { #default }
        }
    }});
}

fn derive_partialeq(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    let mut cmp = quote! { true };
    input.begin_trait(quote! { ::core::cmp::#traitid }, output, |name| {
        cmp.extend(quote! { && self.#name.eq(&other.#name) });
    });
    output.extend::<TokenStream>(quote! {{
        #[inline]
        fn eq(&self, other: &Self) -> bool {
            #cmp
        }
    }});
}

fn derive_eq(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    input.begin_trait(quote! { ::core::cmp::#traitid }, output, |_| ());
    output.extend::<TokenStream>(quote! {{}});
}

fn derive_partialord(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    let mut cmp = quote! { let cmp = Some(::core::cmp::Ordering::Equal); };
    input.begin_trait(quote! { ::core::cmp::#traitid }, output, |name| {
        cmp.extend(quote! {
            if cmp != Some(::core::cmp::Ordering::Equal) {
                return cmp;
            }
            let cmp = self.#name.partial_cmp(&other.#name);
        });
    });
    output.extend::<TokenStream>(quote! {{
        #[inline]
        fn partial_cmp(&self, other: &Self) -> Option<::core::cmp::Ordering> {
            #cmp cmp
        }
    }});
}

fn derive_ord(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    let mut cmp = quote! { let cmp = ::core::cmp::Ordering::Equal; };
    input.begin_trait(quote! { ::core::cmp::#traitid }, output, |name| {
        cmp.extend(quote! {
            if cmp != ::core::cmp::Ordering::Equal {
                return cmp;
            }
            let cmp = self.#name.cmp(&other.#name);
        });
    });
    output.extend::<TokenStream>(quote! {{
        #[inline]
        fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
            #cmp cmp
        }
    }});
}

fn derive_hash(input: &Input, traitid: &Ident, output: &mut TokenStream) {
    let mut hash = TokenStream::new();
    input.begin_trait(quote! { ::core::hash::#traitid }, output, |name| {
        hash.extend(quote! { self.#name.hash(state); });
    });
    output.extend::<TokenStream>(quote! {{
        #[inline]
        fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
            #hash
        }
    }});
}
