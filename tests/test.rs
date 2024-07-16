#![allow(clippy::type_complexity)]

use core::marker::PhantomData;
use iderive::iderive;

#[test]
fn unit_struct() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Unit;

    let s = Unit;
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Unit;
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Unit", format!("{:?}", s));

    #[iderive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Hash)]
    struct Unit2;

    let s = Unit2;
    let s3 = s.clone();
    let s4 = Unit2;
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Unit2", format!("{:?}", s));
}

#[test]
fn tuple_struct_0() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Tuple<const A: usize = 0>();

    let s = Tuple::<0>();
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Tuple::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Tuple", format!("{:?}", s));

    #[iderive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Hash)]
    struct Tuple2();

    let s = Tuple2();
    let s3 = s.clone();
    let s4 = Tuple2::default();
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Tuple2", format!("{:?}", s));
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

    #[iderive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Hash)]
    struct Tuple2(u32);

    let s = Tuple2(0);
    let s3 = s.clone();
    let s4 = Tuple2::default();
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Tuple2(0)", format!("{:?}", s));
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

    #[iderive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Hash)]
    struct Tuple2(u32, i64);

    let s = Tuple2(0, 1);
    let s3 = s.clone();
    let s4 = Tuple2::default();
    assert_eq!(s, s3);
    assert!(s > s4);
    assert_eq!("Tuple2(0, 1)", format!("{:?}", s));
}

#[test]
fn named_struct_0() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Named {}

    let s = Named {};
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Named::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Named", format!("{:?}", s));

    #[iderive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Hash)]
    struct Named2 {}

    let s = Named2 {};
    let s3 = s.clone();
    let s4 = Named2::default();
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Named2", format!("{:?}", s));
}

#[test]
fn named_struct_1() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Named {
        one: u32,
    }

    let s = Named { one: 0 };
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Named::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Named { one: 0 }", format!("{:?}", s));

    #[repr(transparent)]
    #[iderive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Hash)]
    struct Named2 {
        one: u32,
    }

    let s = Named2 { one: 0 };
    let s3 = s.clone();
    let s4 = Named2::default();
    assert_eq!(s, s3);
    assert_eq!(s, s4);
    assert_eq!("Named2 { one: 0 }", format!("{:?}", s));
}

#[test]
fn named_struct_2() {
    #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Named {
        one: u32,
        two: i64,
    }

    let s = Named { one: 0, two: 1 };
    let s2 = s;
    #[allow(clippy::clone_on_copy)]
    let s3 = s.clone();
    let s4 = Named::default();
    assert_eq!(s, s2);
    assert_eq!(s, s3);
    assert!(s > s4);
    assert_eq!("Named { one: 0, two: 1 }", format!("{:?}", s));

    #[iderive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Hash)]
    struct Named2 {
        pub one: u32,
        pub(crate) two: i64,
    }

    let s = Named2 { one: 0, two: 1 };
    let s3 = s.clone();
    let s4 = Named2::default();
    assert_eq!(s, s3);
    assert!(s > s4);
    assert_eq!("Named2 { one: 0, two: 1 }", format!("{:?}", s));
}

#[test]
fn canonical() {
    #[iderive(Clone, Copy)]
    struct S<T>(T);

    let s = S(String::new());
    let _s2 = s.clone();

    #[iderive(PartialEq, Eq, PartialOrd, Ord)]
    struct S2<T>(T);

    let s = S2(0.5);
    let s2 = S2(1.5);
    assert!(s < s2);
}

trait Trait<T> {}

#[test]
fn generics() {
    impl<T> Trait<T> for fn() -> String {}
    struct S<T, const A: bool>(T);

    fn zero() -> i32 {
        0
    }

    fn test<F: Fn() -> i32>(_: F) {
        #[iderive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
        struct Struct<
            T,
            W: Fn() -> i32,
            U: crate::Trait<fn() -> T> = fn() -> T,
            const A: ::core::primitive::bool = { !false },
            V = S<fn() -> fn() -> fn(), A>,
        >(PhantomData<fn() -> T>, PhantomData<(U, fn() -> V, W)>);

        let s = Struct::<String, F>::default();
        let s2 = s;
        let s3 = s;
        assert!(s2 <= s3);
        let _ = s.0;
        let _ = s.1;
    }

    test(zero);
}
