use get::Get;

macro_rules! testcase {
    ($test:literal) => {
        concat!("tests/trybuild/", $test)
    };
}

#[derive(Get)]
pub struct CatStruct<'a, T> {
    name: &'a str,
    age: u64,
    owner: T,
}

#[derive(Get)]
pub struct CatTupleStruct<'a, T>(
    #[get(method = "name")] &'a str,
    #[get(method = "age")] u64,
    #[get(method = "owner")] T,
);

#[test]
fn cat_struct() {
    let cat = CatStruct {
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
    let cat = CatTupleStruct("cat", 1, ());
    assert_eq!(*cat.name(), "cat");
    assert_eq!(*cat.age(), 1);
    assert!(matches!(cat.owner(), ()));
}
