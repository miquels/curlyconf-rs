# curlyconf-rs

Curlyconf is a configuration file reader for the configuration
file format used by, for example, named.conf and dhcpd.conf.

Why? Because I wanted something more readable than JSON or YAML.
And something more hierarchical than TOML.

I'm still considering naming this crate `pint`, for Pint Is Not Toml.
Coming up with a good name is hard. Perhaps I should try something
easier first, like studying cache invalidation strategies.

## Example config (file.cfg)

```text
person charlie {
	fullname "Charlie Brown";
	address 192.168.1.1;
}
person snoopy {
	fullname "Snoopy";
}
```

## Example code

```rust
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Config {
    person: Vec<Person>,
}

#[derive(Debug, Deserialize)]
struct Person {
    #[serde(rename = "__label__")]
    name: String,
    #[serde(default)]
    fullname: Option<String>,
    #[serde(default)]
    address: Option<IpAddr>,
}

type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

fn main() -> Result<()> {
    // Read the configuration file.
    let config: Config = curlyconf::from_file("file.cfg", curlyconf::Mode::Semicolon)?;

    // Print what we got (println!("{:?}", config) would be easier...).
    for (i, p) in config.person.iter().enumerate() {
        println!("{}: {} fullname {:?} addr {:?}", i, p.name, p.fullname, p.addr);
    }

    Ok(())
}
```
## This will print:

	0: charlie fullname Some("Charlie Brown") addr Some(V4(192.168.1.1))
	1: snoopy fullname Some("Snoopy") addr None

Curlyconf uses [serde](https://crates.io/crates/serde) to deserialize the
configuration file values to rust types, just like almost every other
crate that does something similar.

## Sections and values.

The configuration file contains section names, labels, sections, value names, and values:

- sections. they have a section\_name, an optional label, and contain
  a list of other sections and values.
- values. this is a value\_name, followed by a value. If the value is a `Vec`,
  there can be multiple values, separated by a comma.

A section can only have a label if:

- it is part of a `Vec<Section>` (TODO: HashMaps)
- the Rust struct that corresponds to the section has a `__label__` field.

The `__label__` field of the struct will then be set to the label value.
The label type can be any type, it does not have to be a string - it could
also be, for example, a `PathBuf` or `IpAddr`.

```text
section_name [label] {
    value_name value [,value...];
    value_name value [,value...];
    section_name [label] {
        value_name value [,value...];
    }
}
```

## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.

