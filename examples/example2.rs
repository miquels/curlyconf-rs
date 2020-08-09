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
    address: Option<std::net::IpAddr>,
}

type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

const CFG: &'static str = r#"
person charlie {
	fullname "Charlie Brown";
	address 192.168.1.1;
}
person snoopy {
	fullname "Snoopy";
}
"#;

fn main() -> Result<()> {
    // Read the configuration file.
    let config: Config = curlyconf::from_str(CFG)?;

    // Print what we got (println!("{:?}", config) would be easier...).
    for (i, p) in config.person.iter().enumerate() {
        println!(
            "{}: {} fullname {:?} addr {:?}",
            i, p.name, p.fullname, p.address
        );
    }

    Ok(())
}
