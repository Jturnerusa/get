use get::Get;

macro_rules! testcase {
    ($test:literal) => {
        concat!("tests/trybuild/", $test)
    };
}

#[derive(Get)]
pub struct Cat<'a, T> {
    name: &'a str,
    age: u64,
    owner: T,
}

#[derive(Get)]
pub struct CatTuple<'a, T>(
    #[get(method = "name")] &'a str,
    #[get(method = "age")] u64,
    #[get(method = "owner")] T,
);

#[test]
fn cat_struct() {
    let cat = Cat {
        name: "cat",
        age: 1,
        owner: (),
    };
    assert_eq!(*cat.name(), "cat");
    assert_eq!(*cat.age(), 1);
    assert!(matches!(cat.owner(), ()));
}

#[test]
fn cat_tuple_struct() {
    let cat = CatTuple("cat", 1, ());
    assert_eq!(*cat.name(), "cat");
    assert_eq!(*cat.age(), 1);
    assert!(matches!(cat.owner(), ()));
}

#[test]
fn trybuild() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail(testcase!("unit-struct.rs"));
    tests.compile_fail(testcase!("tuple-struct-without-attribute.rs"));
    tests.compile_fail(testcase!("invalid-attribute.rs"));
}
