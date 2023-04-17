use get::Get;

#[derive(Get)]
pub struct Cat {
    #[get()]
    name: String,
}

#[derive(Get)]
pub struct Dog {
    #[get]
    name: String,
}

#[derive(Get)]
pub struct Crab {
    #[get(invalid_name = "invalid_value")]
    name: String,
}

pub fn main() {}
