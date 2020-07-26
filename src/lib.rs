mod de;
mod error;
mod parser;
mod tokenizer;

pub use tokenizer::*;
pub use error::Error;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
