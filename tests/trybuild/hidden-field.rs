use get::Get;

#[derive(Get)]
struct Crab {
    #[get(hide)]
    name: String,
    age: u64,
}

fn crab() {
    let ferris = Crab {
        name: "ferris".to_string(),
        age: 1,
    };
    assert_eq!(ferris.name().as_str(), "ferris");
}

pub fn main() {}
