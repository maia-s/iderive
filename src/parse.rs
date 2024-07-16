use core::{iter::Peekable, str};
use proc_macro::{Delimiter, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::borrow::Cow;

macro_rules! def_keywords {
    ($($tag:ident),* $(,)?) => {
        const KEYWORDS: &[&'static str] = &[ $( strip_kw_prefix(stringify!($tag)) ),* ];
        def_keywords!(@ 0; $($tag,)*);
    };

    (@ $n:expr;) => {};

    (@ $n:expr; $tag:ident, $($rest:tt)*) => {
        #[allow(non_camel_case_types)]
        pub(crate) type $tag = Keyword<{$n}>;
        def_keywords!(@ $n + 1; $($rest)*);
    };
}

def_keywords! {
    Kw_const,
    Kw_pub,
    Kw_struct,
    Kw_union,
    Kw_where,
}

const fn strip_kw_prefix(s: &str) -> &str {
    let bytes = s.as_bytes();
    let (pfx, bytes) = bytes.split_at(3);
    match pfx {
        b"Kw_" => (),
        _ => panic!("expected keyword ident to start with `Kw_`"),
    }
    match str::from_utf8(bytes) {
        Ok(s) => s,
        _ => unreachable!(),
    }
}

pub(crate) const fn into_ascii_char_first(s: &str) -> char {
    let byte = s.as_bytes()[0];
    assert!(byte <= 0x7f);
    byte as char
}

pub(crate) const fn into_ascii_char_second(s: &str) -> char {
    assert!(s.len() <= 2);
    let byte = if s.len() == 1 { 0 } else { s.as_bytes()[1] };
    assert!(byte <= 0x7f);
    byte as char
}

// this doesn't have to worry about `<` `>` in operators bc those will be inside `{}` groups,
// except for the `->` operator
fn get_matching_generic_angle_brackets(
    input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    output: &mut Vec<TokenTree>,
) -> Result<(), Error> {
    let mut level = 0;
    while let Some(tt) = input.next() {
        if let TokenTree::Punct(p) = &tt {
            match p.as_char() {
                '<' => level += 1,
                '>' => level -= 1,
                '-' | '=' if p.spacing() == Spacing::Joint => {
                    output.push(tt);
                    if let Some(TokenTree::Punct(p)) = input.peek() {
                        if p.as_char() == '>' {
                            output.push(input.next().unwrap());
                        }
                    }
                    continue;
                }
                _ => (),
            }
        }
        output.push(tt);
        if level == 0 {
            return Ok(());
        }
    }
    Err(Error::new("unexpected end of input in angle brackets"))
}

fn get_tts_with_matching_angle_brackets_until_comma(
    input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    output: &mut Vec<TokenTree>,
) -> Result<(), Error> {
    while let Some(tt) = input.peek() {
        if let TokenTree::Punct(p) = &tt {
            match p.as_char() {
                ',' => break,
                '<' => {
                    get_matching_generic_angle_brackets(input, output)?;
                    continue;
                }
                _ => (),
            }
        }
        output.push(input.next().unwrap());
    }
    Ok(())
}

pub(crate) fn into_parse_input(input: TokenStream) -> Peekable<impl Iterator<Item = TokenTree>> {
    input
        .into_iter()
        .map(|mut tt| loop {
            if let TokenTree::Group(g) = &tt {
                if matches!(g.delimiter(), Delimiter::None) {
                    let mut it = g.stream().into_iter();
                    if let Some(first) = it.next() {
                        if it.next().is_none() {
                            tt = first;
                            continue;
                        }
                    }
                }
            }
            return tt;
        })
        .fuse()
        .peekable()
}

pub(crate) struct Error {
    span: Option<Span>,
    str: String,
}

impl Error {
    pub(crate) fn new(str: impl Into<String>) -> Self {
        Self {
            span: None,
            str: str.into(),
        }
    }

    pub(crate) fn with_span(span: Span, str: impl Into<String>) -> Self {
        Self {
            span: Some(span),
            str: str.into(),
        }
    }
}

impl IntoTokenTrees for Error {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        let tokens: TokenStream = format!("::core::compile_error!({:?});", self.str)
            .parse()
            .unwrap();
        if let Some(span) = self.span {
            output.extend(tokens.into_iter().map(|mut tt| {
                tt.set_span(span);
                tt
            }));
        } else {
            output.extend(tokens)
        };
    }
}

#[derive(Clone)]
pub(crate) struct Keyword<const KW: usize>(Ident);

impl<const KW: usize> Keyword<KW> {
    pub fn span(&self) -> Span {
        self.0.span()
    }
}

impl<const KW: usize> Default for Keyword<KW> {
    fn default() -> Self {
        Self(Ident::new(KEYWORDS[KW], Span::call_site()))
    }
}

impl<const KW: usize> TryParse for Keyword<KW> {
    fn desc() -> Cow<'static, str> {
        format!("keyword `{}`", KEYWORDS[KW]).into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        if let Some(TokenTree::Ident(i)) = input.peek() {
            if i.to_string() == KEYWORDS[KW] {
                let Some(TokenTree::Ident(i)) = input.next() else {
                    unreachable!()
                };
                return Ok(Some(Keyword(i)));
            }
        }
        Ok(None)
    }
}

impl<const KW: usize> IntoTokenTrees for Keyword<KW> {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        output.extend([TokenTree::Ident(self.0)])
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Op<const OP1: char, const OP2: char>(Span);

impl<const OP1: char, const OP2: char> Op<OP1, OP2> {
    fn new(span: Span) -> Self {
        Self(span)
    }
}

impl<const OP1: char, const OP2: char> Default for Op<OP1, OP2> {
    fn default() -> Self {
        Self::new(Span::call_site())
    }
}

impl<const OP1: char, const OP2: char> TryParse for Op<OP1, OP2> {
    fn desc() -> Cow<'static, str> {
        if OP2 == '\0' {
            format!("`{}` token", OP1).into()
        } else {
            format!("`{}{}` token", OP1, OP2).into()
        }
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        if let Some(TokenTree::Punct(p)) = input.peek() {
            if p.as_char() == OP1 && (OP2 == '\0' || matches!(p.spacing(), Spacing::Joint)) {
                let Some(TokenTree::Punct(p)) = input.next() else {
                    unreachable!()
                };
                if OP2 == '\0' {
                    return Ok(Some(Self(p.span())));
                }
                if let Some(tt) = input.next() {
                    if let TokenTree::Punct(p2) = &tt {
                        if p2.as_char() == OP2 {
                            // FIXME: join spans if/when that becomes possible on stable
                            return Ok(Some(Self(p.span())));
                        }
                    }
                    return Err(Error::with_span(tt.span(), format!("expected `{}`", OP2)));
                }
                return Err(Error::with_span(
                    p.span(),
                    format!("expected `{}` after this `{}`", OP2, OP1),
                ));
            }
        }
        Ok(None)
    }
}

impl<const OP1: char, const OP2: char> IntoTokenTrees for Op<OP1, OP2> {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        if OP2 == '\0' {
            let mut p1 = Punct::new(OP1, Spacing::Alone);
            p1.set_span(self.0);
            output.extend([TokenTree::Punct(p1)]);
        } else {
            let mut p1 = Punct::new(OP1, Spacing::Joint);
            let mut p2 = Punct::new(OP2, Spacing::Alone);
            p1.set_span(self.0);
            p2.set_span(self.0);
            output.extend([TokenTree::Punct(p1), TokenTree::Punct(p2)]);
        }
    }
}

pub(crate) trait TryParse: Sized {
    fn desc() -> Cow<'static, str>;
    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error>;
}

pub(crate) trait Parse: Sized {
    fn parse(input: &mut Peekable<impl Iterator<Item = TokenTree>>) -> Result<Self, Error>;

    fn parse_all(input: &mut Peekable<impl Iterator<Item = TokenTree>>) -> Result<Self, Error> {
        let result = Self::parse(input)?;
        if let Some(tt) = input.peek() {
            Err(Error::with_span(tt.span(), "unexpected token"))
        } else {
            Ok(result)
        }
    }
}

impl<T> Parse for T
where
    T: TryParse,
{
    fn parse(input: &mut Peekable<impl Iterator<Item = TokenTree>>) -> Result<Self, Error> {
        if let Some(parsed) = Self::try_parse(input)? {
            Ok(parsed)
        } else if let Some(tt) = input.peek() {
            Err(Error::with_span(
                tt.span(),
                format!("expected {}", Self::desc()),
            ))
        } else {
            Err(Error::new(format!(
                "unexpected end of input; expected {}",
                Self::desc(),
            )))
        }
    }
}

impl<T> Parse for Option<T>
where
    T: TryParse,
{
    fn parse(input: &mut Peekable<impl Iterator<Item = TokenTree>>) -> Result<Self, Error> {
        T::try_parse(input)
    }
}

impl<T> TryParse for Vec<T>
where
    T: TryParse,
{
    fn desc() -> Cow<'static, str> {
        T::desc()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        let mut v = Vec::new();
        while let Some(i) = T::try_parse(input)? {
            v.push(i);
        }
        Ok(Some(v))
    }
}

pub(crate) trait IntoTokenTrees: Sized {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>);
}

impl<T: IntoTokenTrees> IntoTokenTrees for Option<T> {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        if let Some(t) = self {
            t.into_token_trees(output)
        }
    }
}

impl<T: IntoTokenTrees> IntoTokenTrees for Vec<T> {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        for i in self.into_iter() {
            i.into_token_trees(output);
        }
    }
}

impl IntoTokenTrees for TokenTree {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        output.extend([self])
    }
}

impl IntoTokenTrees for TokenStream {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        output.extend(self)
    }
}

impl IntoTokenTrees for &str {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        output.extend([TokenTree::Literal(Literal::string(self))])
    }
}

impl<'a, T> IntoTokenTrees for &'a T
where
    &'a T: ToTokenTrees,
{
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.to_token_trees(output);
    }
}

pub(crate) trait ToTokenTrees {
    fn to_token_trees(self, output: &mut impl Extend<TokenTree>);
}

impl<T: IntoTokenTrees + Clone> ToTokenTrees for &T {
    fn to_token_trees(self, output: &mut impl Extend<TokenTree>) {
        (*self).clone().into_token_trees(output)
    }
}

impl<T: IntoTokenTrees + Clone> ToTokenTrees for &mut T {
    fn to_token_trees(self, output: &mut impl Extend<TokenTree>) {
        (*self).clone().into_token_trees(output)
    }
}

pub(crate) struct Attribute {
    hash_token: Op![#],
    group: Group<DELIM_BRACKET>,
}

impl TryParse for Attribute {
    fn desc() -> Cow<'static, str> {
        "attribute".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        if let Some(hash_token) = <Op![#]>::try_parse(input)? {
            let group = Group::<DELIM_BRACKET>::parse(input)?;
            Ok(Some(Attribute { hash_token, group }))
        } else {
            Ok(None)
        }
    }
}

impl IntoTokenTrees for Attribute {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.hash_token.into_token_trees(output);
        self.group.into_token_trees(output);
    }
}

pub(crate) type Attributes = Vec<Attribute>;

pub const NAMED_FIELDS: u8 = 1;
pub const UNNAMED_FIELDS: u8 = 2;
pub const NO_FIELDS: u8 = 4;

pub(crate) struct Field<const FIELD_TYPES: u8 = { NAMED_FIELDS | UNNAMED_FIELDS }> {
    pub attrs: Attributes,
    pub vis: Option<Visibility>,
    pub name: Option<Ident>,
    pub colon_token: Option<Op![:]>,
    pub ty: Type,
}

impl From<Field<NAMED_FIELDS>> for Field {
    fn from(value: Field<NAMED_FIELDS>) -> Self {
        Self {
            attrs: value.attrs,
            vis: value.vis,
            name: value.name,
            colon_token: value.colon_token,
            ty: value.ty,
        }
    }
}

impl From<Field<UNNAMED_FIELDS>> for Field {
    fn from(value: Field<UNNAMED_FIELDS>) -> Self {
        Self {
            attrs: value.attrs,
            vis: value.vis,
            name: value.name,
            colon_token: value.colon_token,
            ty: value.ty,
        }
    }
}

impl TryParse for Field<NAMED_FIELDS> {
    fn desc() -> Cow<'static, str> {
        "named field".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        let attrs = Attributes::parse(input)?;
        let vis = Visibility::try_parse(input)?;
        let name = if attrs.is_empty() {
            if let Some(name) = Ident::try_parse(input)? {
                name
            } else {
                return Ok(None);
            }
        } else {
            Ident::parse(input)?
        };
        let colon_token = Some(<Op![:]>::parse(input)?);
        let mut ty = Vec::new();
        get_tts_with_matching_angle_brackets_until_comma(input, &mut ty)?;
        Ok(Some(Self {
            attrs,
            vis,
            name: Some(name),
            colon_token,
            ty: ty.into(),
        }))
    }
}

impl TryParse for Field<UNNAMED_FIELDS> {
    fn desc() -> Cow<'static, str> {
        "unnamed field".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        let attrs = Attributes::parse(input)?;
        let vis = Visibility::try_parse(input)?;
        let mut ty = Vec::new();
        get_tts_with_matching_angle_brackets_until_comma(input, &mut ty)?;
        if ty.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Self {
                attrs,
                vis,
                name: None,
                colon_token: None,
                ty: ty.into(),
            }))
        }
    }
}

impl<const FIELD_TYPES: u8> IntoTokenTrees for Field<FIELD_TYPES> {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.attrs.into_token_trees(output);
        self.vis.into_token_trees(output);
        if FIELD_TYPES & NAMED_FIELDS != 0 {
            self.name.into_token_trees(output);
            self.colon_token.into_token_trees(output);
        }
        self.ty.into_token_trees(output);
    }
}

pub(crate) struct Fields<const FIELD_TYPES: u8 = { NAMED_FIELDS | UNNAMED_FIELDS | NO_FIELDS }> {
    span: Option<Span>,
    is_named: bool,
    fields: Punctuated<Field, Op![,]>,
}

impl<const FIELD_TYPES: u8> Fields<FIELD_TYPES> {
    fn into_any_fields(self) -> Fields {
        Fields {
            span: self.span,
            is_named: self.is_named,
            fields: self.fields,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Field> + '_ {
        self.fields.0.iter().map(|(t, _)| t)
    }
}

impl<const FIELD_TYPES: u8> TryParse for Fields<FIELD_TYPES> {
    fn desc() -> Cow<'static, str> {
        "fields".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        'a: {
            if let Some(TokenTree::Group(g)) = input.peek() {
                let span = Some(g.span());
                let (is_named, fields) = match g.delimiter() {
                    Delimiter::Brace if FIELD_TYPES & NAMED_FIELDS != 0 => (
                        true,
                        Punctuated::<Field<NAMED_FIELDS>, Op![,]>::parse(&mut into_parse_input(
                            g.stream(),
                        ))?
                        .convert_into(),
                    ),
                    Delimiter::Parenthesis if FIELD_TYPES & UNNAMED_FIELDS != 0 => (
                        false,
                        Punctuated::<Field<UNNAMED_FIELDS>, Op![,]>::parse(&mut into_parse_input(
                            g.stream(),
                        ))?
                        .convert_into(),
                    ),
                    _ => break 'a,
                };
                input.next();
                return Ok(Some(Self {
                    span,
                    is_named,
                    fields,
                }));
            }
        }

        if FIELD_TYPES & NO_FIELDS != 0 {
            return Ok(Some(Self {
                span: None,
                is_named: false,
                fields: Punctuated::new(),
            }));
        }

        Ok(None)
    }
}

impl<const FIELD_TYPES: u8> IntoTokenTrees for Fields<FIELD_TYPES> {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        if let Some(span) = self.span {
            let mut fields = TokenStream::new();
            self.fields.into_token_trees(&mut fields);
            let mut group = proc_macro::Group::new(
                if self.is_named {
                    Delimiter::Brace
                } else {
                    Delimiter::Parenthesis
                },
                fields,
            );
            group.set_span(span);
            output.extend([TokenTree::Group(group)])
        }
    }
}

#[derive(Clone)]
pub(crate) struct Generic {
    const_kw: Option<Kw_const>,
    sym: Vec<TokenTree>,
    colon_token: Option<Op![:]>,
    bounds: Vec<TokenTree>,
    eq_token: Option<Op![=]>,
    init: Vec<TokenTree>,
}

impl Generic {
    pub fn remove_init(&mut self) {
        self.eq_token = None;
        self.init = Vec::new();
    }
}

impl TryParse for Generic {
    fn desc() -> Cow<'static, str> {
        "generic".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        let mut sym = Vec::new();
        let mut bounds = Vec::new();
        let mut init = Vec::new();
        let mut tokens = &mut sym;
        let mut colon_token = None;
        let mut eq_token = None;

        let const_kw = Kw_const::try_parse(input)?;

        'next: while let Some(tt) = input.peek() {
            if let TokenTree::Punct(p) = &tt {
                match p.as_char() {
                    ',' | '>' => break,

                    '<' => {
                        get_matching_generic_angle_brackets(input, tokens)?;
                        continue 'next;
                    }

                    ':' if colon_token.is_none() => {
                        if matches!(p.spacing(), Spacing::Alone) {
                            let Some(TokenTree::Punct(c)) = input.next() else {
                                unreachable!()
                            };
                            colon_token = Some(<Op![:]>::new(c.span()));
                            tokens = &mut bounds;
                        } else {
                            tokens.push(input.next().unwrap());

                            while let Some(TokenTree::Punct(p)) = input.peek() {
                                if p.as_char() == ':' {
                                    tokens.push(input.next().unwrap());
                                } else {
                                    continue 'next;
                                }
                            }
                        }
                    }

                    '-' | '=' if p.spacing() == Spacing::Joint => {
                        tokens.push(input.next().unwrap());
                        if let Some(TokenTree::Punct(_)) = input.peek() {
                            tokens.push(input.next().unwrap());
                        }
                        continue;
                    }

                    '=' if eq_token.is_none() => {
                        if matches!(p.spacing(), Spacing::Alone) {
                            let Some(TokenTree::Punct(c)) = input.next() else {
                                unreachable!()
                            };
                            eq_token = Some(<Op![=]>::new(c.span()));
                            tokens = &mut init;
                        } else {
                            tokens.push(input.next().unwrap());

                            while let Some(TokenTree::Punct(p)) = input.peek() {
                                if p.as_char() == '=' {
                                    tokens.push(input.next().unwrap());
                                } else {
                                    continue 'next;
                                }
                            }
                        }
                    }

                    _ => {
                        tokens.push(input.next().unwrap());
                    }
                }
            } else {
                tokens.push(input.next().unwrap());
            }
        }

        Ok(if sym.is_empty() {
            if let Some(const_kw) = const_kw {
                return Err(Error::with_span(
                    const_kw.0.span(),
                    "expected ident after `const`",
                ));
            }
            if let Some(colon_token) = colon_token {
                return Err(Error::with_span(
                    colon_token.0,
                    "expected ident or lifetime before `:`",
                ));
            }
            None
        } else {
            Some(Generic {
                const_kw,
                sym,
                colon_token,
                bounds,
                eq_token,
                init,
            })
        })
    }
}

impl IntoTokenTrees for Generic {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.const_kw.into_token_trees(output);
        self.sym.into_token_trees(output);
        self.colon_token.into_token_trees(output);
        self.bounds.into_token_trees(output);
        self.eq_token.into_token_trees(output);
        self.init.into_token_trees(output);
    }
}

#[derive(Clone)]
pub(crate) struct Generics {
    lt_token: Op![<],
    generics: Punctuated<Generic, Op![,]>,
    gt_token: Op![>],
}

impl Generics {
    pub fn remove_init(&mut self) {
        for (g, _) in self.generics.0.iter_mut() {
            g.remove_init();
        }
    }
}

impl TryParse for Generics {
    fn desc() -> Cow<'static, str> {
        "generics".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        if let Some(lt_token) = <Op![<]>::try_parse(input)? {
            let generics = Punctuated::<Generic, Op![,]>::parse(input)?;
            let gt_token = <Op![>]>::parse(input)?;

            return Ok(Some(Generics {
                lt_token,
                generics,
                gt_token,
            }));
        }
        Ok(None)
    }
}

impl IntoTokenTrees for Generics {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.lt_token.into_token_trees(output);
        self.generics.into_token_trees(output);
        self.gt_token.into_token_trees(output);
    }
}

pub(crate) struct GenericsSymOnly {
    lt_token: Op![<],
    generics: Punctuated<Vec<TokenTree>, Op![,]>,
    gt_token: Op![>],
}

impl From<&Generics> for GenericsSymOnly {
    fn from(value: &Generics) -> Self {
        Self {
            lt_token: value.lt_token,
            generics: value
                .generics
                .0
                .iter()
                .map(|(t, p)| (t.sym.clone(), *p))
                .collect(),
            gt_token: value.gt_token,
        }
    }
}

impl IntoTokenTrees for GenericsSymOnly {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.lt_token.into_token_trees(output);
        self.generics.into_token_trees(output);
        self.gt_token.into_token_trees(output);
    }
}

pub(crate) struct Group<const DELIMS: u8>(proc_macro::Group);

pub const DELIM_PAREN: u8 = 1;
pub const DELIM_BRACE: u8 = 2;
pub const DELIM_BRACKET: u8 = 4;
pub const DELIM_NONE: u8 = 8;

impl<const DELIMS: u8> TryParse for Group<DELIMS> {
    fn desc() -> Cow<'static, str> {
        let mut desc = String::new();
        if DELIMS & DELIM_PAREN != 0 {
            desc.push_str("`(`...`)`");
            if DELIMS & !DELIM_PAREN != 0 {
                desc.push_str(" or ");
            }
        }
        if DELIMS & DELIM_BRACE != 0 {
            desc.push_str("`{`...`}`");
            if DELIMS & !(DELIM_PAREN | DELIM_BRACE) != 0 {
                desc.push_str(" or ");
            }
        }
        if DELIMS & DELIM_BRACKET != 0 {
            desc.push_str("`[`...`]`");
            if DELIMS & !(DELIM_PAREN | DELIM_BRACE | DELIM_BRACKET) != 0 {
                desc.push_str(" or ");
            }
        }
        if DELIMS & DELIM_NONE != 0 {
            desc.push_str("none-delimited group");
        }
        desc.into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        if let Some(TokenTree::Group(group)) = input.peek() {
            if match group.delimiter() {
                Delimiter::Parenthesis => DELIMS & DELIM_PAREN != 0,
                Delimiter::Brace => DELIMS & DELIM_BRACE != 0,
                Delimiter::Bracket => DELIMS & DELIM_BRACKET != 0,
                Delimiter::None => DELIMS & DELIM_NONE != 0,
            } {
                let Some(TokenTree::Group(group)) = input.next() else {
                    unreachable!()
                };
                Ok(Some(Self(group)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

impl<const DELIMS: u8> IntoTokenTrees for Group<DELIMS> {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        output.extend([TokenTree::Group(self.0)])
    }
}

impl TryParse for Ident {
    fn desc() -> Cow<'static, str> {
        "identifier".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        if let Some(TokenTree::Ident(_)) = input.peek() {
            let Some(TokenTree::Ident(ident)) = input.next() else {
                unreachable!()
            };
            Ok(Some(ident))
        } else {
            Ok(None)
        }
    }
}

impl IntoTokenTrees for Ident {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        output.extend([TokenTree::Ident(self)])
    }
}

impl IntoTokenTrees for Literal {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        output.extend([TokenTree::Literal(self)])
    }
}

#[derive(Clone)]
pub(crate) struct Path {
    coloncolon_token: Option<Op![::]>,
    path: Punctuated<Ident, Op![::]>,
}

impl Path {
    pub fn new(path: &str) -> Result<Self, Error> {
        match path.parse() {
            Ok(ts) => Self::parse_all(&mut { into_parse_input(ts) }),
            Err(_) => Err(Error::new(format!("invalid path `{path}`"))),
        }
    }

    pub fn for_ident(path: &str, ident: Ident) -> Result<Self, Error> {
        let mut path = Self::new(path)?;
        path.path.push_value(ident);
        Ok(path)
    }
}

impl IntoTokenTrees for Path {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.coloncolon_token.into_token_trees(output);
        self.path.into_token_trees(output);
    }
}

impl TryParse for Path {
    fn desc() -> Cow<'static, str> {
        "path".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        let coloncolon_token = <Op![::]>::try_parse(input)?;
        let path = Punctuated::<Ident, Op![::]>::parse(input)?;
        if coloncolon_token.is_none() && path.0.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Self {
                coloncolon_token,
                path,
            }))
        }
    }
}

#[derive(Clone)]
pub(crate) struct Punctuated<T, P>(Vec<(T, Option<P>)>);

impl<T, P> Punctuated<T, P> {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn push_value(&mut self, value: T)
    where
        P: Default,
    {
        if let Some(last) = self.0.last_mut() {
            if last.1.is_none() {
                last.1 = Some(P::default());
            }
        }
        self.0.push((value, None))
    }

    fn convert_into<T2>(self) -> Punctuated<T2, P>
    where
        T: Into<T2>,
    {
        Punctuated(self.0.into_iter().map(|(t, p)| (t.into(), p)).collect())
    }

    pub(crate) fn into_values(self) -> impl Iterator<Item = T> {
        self.0.into_iter().map(|(t, _)| t)
    }
}

impl<T, P> Default for Punctuated<T, P> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, P> FromIterator<(T, Option<P>)> for Punctuated<T, P> {
    fn from_iter<I: IntoIterator<Item = (T, Option<P>)>>(iter: I) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<T, P> TryParse for Punctuated<T, P>
where
    T: TryParse,
    P: TryParse,
{
    fn desc() -> Cow<'static, str> {
        T::desc()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        let mut output = Vec::new();

        while let Some(t) = T::try_parse(input)? {
            let p = P::try_parse(input)?;
            let has_p = p.is_some();
            output.push((t, p));
            if !has_p {
                break;
            }
        }

        Ok(Some(Self(output)))
    }
}

impl<T, P> IntoTokenTrees for Punctuated<T, P>
where
    T: IntoTokenTrees,
    Option<P>: IntoTokenTrees,
{
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        for (t, p) in self.0.into_iter() {
            t.into_token_trees(output);
            p.into_token_trees(output);
        }
    }
}

pub(crate) struct Struct {
    pub attrs: Attributes,
    pub vis: Option<Visibility>,
    pub struct_kw: Option<Kw_struct>,
    pub union_kw: Option<Kw_union>,
    pub ident: Ident,
    pub generics: Option<Generics>,
    pub where_clauses: Option<WhereClauses>,
    pub fields: Fields,
    pub semicolon_token: Option<Op![;]>,
}

impl Struct {
    pub(crate) fn set_attrs_and_vis(&mut self, attrs: Attributes, vis: Option<Visibility>) {
        self.attrs = attrs;
        self.vis = vis;
    }
}

impl Struct {
    pub const fn is_unit_struct(&self) -> bool {
        self.fields.span.is_none()
    }

    pub const fn is_tuple_struct(&self) -> bool {
        !self.is_unit_struct() && !self.fields.is_named
    }
}

impl TryParse for Struct {
    fn desc() -> Cow<'static, str> {
        "struct or union".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        let struct_kw = Kw_struct::try_parse(input)?;
        let union_kw = if struct_kw.is_none() {
            Kw_union::try_parse(input)?
        } else {
            None
        };
        if struct_kw.is_some() || union_kw.is_some() {
            let ident = Ident::parse(input)?;
            let generics = Generics::try_parse(input)?;
            let mut where_clauses = WhereClauses::try_parse(input)?;
            let fields = if union_kw.is_some() {
                Fields::<NAMED_FIELDS>::parse(input)?.into_any_fields()
            } else if where_clauses.is_some() {
                Fields::<{ UNNAMED_FIELDS | NO_FIELDS }>::parse(input)?.into_any_fields()
            } else {
                Fields::parse(input)?
            };
            let semicolon_token = if !fields.is_named {
                if fields.span.is_some() {
                    where_clauses = WhereClauses::try_parse(input)?;
                }
                Some(<Op![;]>::parse(input)?)
            } else {
                None
            };

            Ok(Some(Self {
                attrs: Vec::new(),
                vis: None,
                struct_kw,
                union_kw,
                ident,
                generics,
                where_clauses,
                fields,
                semicolon_token,
            }))
        } else {
            Ok(None)
        }
    }
}

impl IntoTokenTrees for Struct {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.attrs.into_token_trees(output);
        self.vis.into_token_trees(output);
        self.struct_kw.into_token_trees(output);
        self.union_kw.into_token_trees(output);
        self.ident.into_token_trees(output);
        self.generics.into_token_trees(output);
        self.where_clauses.into_token_trees(output);
        self.fields.into_token_trees(output);
        self.semicolon_token.into_token_trees(output);
    }
}

#[derive(Clone)]
pub struct Type(Vec<TokenTree>);

impl From<Vec<TokenTree>> for Type {
    fn from(value: Vec<TokenTree>) -> Self {
        Self(value)
    }
}

impl IntoTokenTrees for Type {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.0.into_token_trees(output)
    }
}

pub(crate) struct Visibility {
    pub_kw: Kw_pub,
    group: Option<Group<DELIM_PAREN>>,
}

impl TryParse for Visibility {
    fn desc() -> Cow<'static, str> {
        "visibility".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        if let Some(pub_kw) = Kw_pub::try_parse(input)? {
            let group = Group::<DELIM_PAREN>::try_parse(input)?;
            Ok(Some(Visibility { pub_kw, group }))
        } else {
            Ok(None)
        }
    }
}

impl IntoTokenTrees for Visibility {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.pub_kw.into_token_trees(output);
        self.group.into_token_trees(output);
    }
}

#[derive(Clone)]
pub(crate) struct WhereClause {
    clause: Vec<TokenTree>,
}

impl From<Vec<TokenTree>> for WhereClause {
    fn from(value: Vec<TokenTree>) -> Self {
        Self { clause: value }
    }
}

impl TryParse for WhereClause {
    fn desc() -> Cow<'static, str> {
        "where clause".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        let mut clause = Vec::new();
        while let Some(tt) = input.peek() {
            match tt {
                TokenTree::Punct(p) => match p.as_char() {
                    '<' => {
                        get_matching_generic_angle_brackets(input, &mut clause)?;
                        continue;
                    }
                    ',' => break,
                    _ => (),
                },

                TokenTree::Group(g) => {
                    if matches!(g.delimiter(), Delimiter::Brace) {
                        break;
                    }
                }

                _ => (),
            }
            clause.push(input.next().unwrap());
        }
        Ok(Some(Self { clause }))
    }
}

impl IntoTokenTrees for WhereClause {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.clause.into_token_trees(output);
    }
}

#[derive(Clone, Default)]
pub(crate) struct WhereClauses {
    where_kw: Kw_where,
    clauses: Punctuated<WhereClause, Op![,]>,
}

impl WhereClauses {
    pub fn push(&mut self, clause: WhereClause) {
        self.clauses.push_value(clause)
    }
}

impl TryParse for WhereClauses {
    fn desc() -> Cow<'static, str> {
        "keyword `where`".into()
    }

    fn try_parse(
        input: &mut Peekable<impl Iterator<Item = TokenTree>>,
    ) -> Result<Option<Self>, Error> {
        if let Some(where_kw) = Kw_where::try_parse(input)? {
            let clauses = Punctuated::<WhereClause, Op![,]>::parse(input)?;
            Ok(Some(Self { where_kw, clauses }))
        } else {
            Ok(None)
        }
    }
}

impl IntoTokenTrees for WhereClauses {
    fn into_token_trees(self, output: &mut impl Extend<TokenTree>) {
        self.where_kw.into_token_trees(output);
        self.clauses.into_token_trees(output);
    }
}
