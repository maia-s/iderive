// Copyright 2021 Maia <66437537+maia-s@users.noreply.github.com>
// Distributed under the Boost Software License, version 1.0
// (See accompanying file LICENSE.md)

#[macro_use]
extern crate iderive;

#[test]
fn unit_struct() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Unit;

    let s = Unit;
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Unit::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Unit", format!("{:?}", s));
}

#[test]
fn tuple_struct_0() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Tuple();

    let s = Tuple();
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Tuple::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Tuple", format!("{:?}", s));
}

#[test]
fn tuple_struct_1() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Tuple(u32);

    let s = Tuple(0);
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Tuple::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Tuple(0)", format!("{:?}", s));
}

#[test]
fn tuple_struct_2() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Tuple(u32, i64);

    let s = Tuple(0, 1);
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Tuple::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert!(s > s4);
    assert_eq!("Tuple(0, 1)", format!("{:?}", s));
}

#[test]
fn named_struct_0() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Named {}

    let s = Named{};
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Named::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Named", format!("{:?}", s));
}

#[test]
fn named_struct_1() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Named {
        one: u32,
    }

    let s = Named{ one: 0 };
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Named::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Named { one: 0 }", format!("{:?}", s));
}

#[test]
fn named_struct_2() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Named {
        one: u32,
        two: i64,
    }

    let s = Named{ one: 0, two: 1 };
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Named::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert!(s > s4);
    assert_eq!("Named { one: 0, two: 1 }", format!("{:?}", s));
}
