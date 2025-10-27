macro_rules! testcase {
    ($test:literal) => {
        concat!("tests/trybuild/", $test)
    };
}

#[test]
fn trybuild() {
    let tests = trybuild::TestCases::new();
    tests.compile_fail(testcase!("unit-struct.rs"));
    tests.compile_fail(testcase!("tuple-struct-without-attribute.rs"));
    tests.compile_fail(testcase!("invalid-attribute.rs"));
    tests.compile_fail(testcase!("hidden-field.rs"));
}

mod get {
    use get::Get;
    #[derive(Get)]
    pub struct Cat<T> {
        #[get(kind = "deref")]
        name: String,
        #[get(kind = "move")]
        age: u64,
        owner: T,
    }

    #[derive(Clone, Get)]
    pub struct CatTuple<T>(
        #[get(method = "name", kind = "deref")] String,
        #[get(method = "age", kind = "move")] u64,
        #[get(method = "owner")] T,
    );

    #[test]
    fn cat_struct() {
        let cat = Cat {
            name: "cat".to_string(),
            age: 1,
            owner: (),
        };
        assert_eq!(cat.name(), "cat");
        assert!(matches!(cat.owner(), ()));
        assert_eq!(cat.age(), 1);
    }

    #[test]
    fn cat_tuple_struct() {
        let cat = CatTuple("cat".to_string(), 1, ());
        assert_eq!(cat.name(), "cat");
        assert!(matches!(cat.owner(), ()));
        assert_eq!(cat.age(), 1);
    }
}
