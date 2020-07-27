mod de;
mod error;
mod parser;
mod tokenizer;

pub use de::from_str;
pub use error::Error;
pub use tokenizer::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
