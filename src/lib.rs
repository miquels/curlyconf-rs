#![doc(html_root_url = "https://docs.rs/curlyconf/0.1.0")]
//! ## Curlyconf
//!
//! Curlyconf is a configuration file reader for the configuration
//! file format used by, for example, named.conf and dhcpd.conf.
//!
//! ## Example config (file.cfg)
//!
//! ```text
//! person charlie {
//! 	fullname "Charlie Brown";
//! 	address 192.168.1.1;
//! }
//! person snoopy {
//! 	fullname "Snoopy";
//! }
//! ```
//!
//! ## Example code
//!
//! ```no_run
//! use serde::Deserialize;
//!
//! // The initial section of any config is a rust struct.
//! #[derive(Debug, Deserialize)]
//! struct Config {
//!     person: Vec<Person>,
//! }
//!
//! #[derive(Debug, Deserialize)]
//! struct Person {
//!     #[serde(rename = "__label__")]
//!     name: String,
//!     #[serde(default)]
//!     fullname: Option<String>,
//!     #[serde(default)]
//!     address: Option<std::net::IpAddr>,
//! }
//!
//! fn main() {
//!     // Read the configuration file.
//!     let config: Config = match curlyconf::from_file("file.cfg") {
//!         Ok(cfg) => cfg,
//!         Err(e) => {
//!             eprintln!("{}", e);
//!             std::process::exit(1);
//!         }
//!     };
//!
//!     // Print what we got (println!("{:?}", config) would be easier...).
//!     for (i, p) in config.person.iter().enumerate() {
//!         println!("{}: {} fullname {:?} addr {:?}", i, p.name, p.fullname, p.address);
//!     }
//! }
//!
//! ```
//!
//! ## This will print:
//!
//! ```text
//! 0: charlie fullname Some("Charlie Brown") addr Some(V4(192.168.1.1))
//! 1: snoopy fullname Some("Snoopy") addr None
//! ```
//!
//! Curlyconf uses [serde](https://crates.io/crates/serde) to deserialize the
//! configuration file values to rust types, just like almost every other
//! crate that does something similar.
//!
//! ## Sections and values.
//!
//! The configuration file contains section names, labels, sections, value names, and values:
//!
//! - **sections**. they have a section\_name, an optional label, and contain
//!   a list of other sections and values. The rust type of a section is a struct.
//! - **values**. this is a value\_name, followed by a value. If the value is a `Vec`,
//!   there can be multiple values, separated by a comma.
//!
//! A section can only have a label if:
//!
//! - it is part of a `HashMap<Key, Section>`, or
//! - it is part of a `Vec<Section>` and the rust struct that corresponds to the
//!   section has a `__label__` field. That field will be set to the label value.
//!
//! The label type can be any type, it does not have to be a string - it could
//! also be, for example, a `PathBuf` or `IpAddr`.
//!
//! The basic structure of a config file is thus:
//!
//! ```text
//! section_name [label] {
//!     value_name value [,value...];
//!     value_name value [,value...];
//!     section_name [label] {
//!         value_name value [,value...];
//!     }
//! }
//! ```
//!
//! `Enum`s are also supported (see the `serde` docs) so you can do things like:
//!
//! ```no_run
//! # use serde::Deserialize;
//! #[derive(Debug, Deserialize)]
//! struct Config {
//!     animal: Animal,
//! }
//!
//! #[derive(Debug, Deserialize)]
//! enum Animal {
//!     Cat {
//!          purrs: bool,
//!     },
//!     Dog {
//!          barks: bool,
//!     },
//! }
//! ```
//!
//! And then have a config like
//!
//! ```text
//! animal cat {
//!     purrs;
//! }
//! ```
//!
//! ## Includes.
//!
//! In every section it is possible to include another file, or multiple files.
//! Simply do:
//!
//! ```text
//! include otherconfig.cfg;
//! ```
//!
//! If the pathname of the included file is relative, it will be interpreted
//! as relative to the current file being parsed. It's possible to use
//! wildcards as well, for example:
//!
//! ```text
//! configs {
//!     include conf.d/*.cfg;
//! }
//! ```
//!
//! There are some limitations, the main one is that you cannot use the "include"
//! statement in the middle of a map or list of subsections. Because of the
//! "include" statement there, the parser cannot lookahead through that
//! statement to see if the list is bding continued.
//!

pub(crate) const DEBUG: bool = false;

macro_rules! debug {
    ($($tt:tt)*) => {
        if $crate::DEBUG {
            log::debug!($($tt)*)
        }
    }
}

mod cfg;
mod de;
mod error;
mod parser;
mod tokenizer;

pub use cfg::{from_file, from_str, Builder, Parser, ParserAccess};
pub use error::Error;
pub use tokenizer::Mode;
pub use parser::Watcher;
