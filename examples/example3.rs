use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Config {
    runtime:    Runtime,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
enum Runtime {
    Threaded(Threaded),
    MultiSingle(MultiSingle),
}

#[derive(Deserialize, Debug)]
struct Threaded {
    #[serde(default)]
    name:   Option<String>,
}

#[derive(Deserialize, Debug)]
struct MultiSingle {
    #[serde(default)]
    name:   Option<String>,
}

type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

const CFG: &'static str = r#"
runtime multisingle {
    name "bla";
}
"#;

fn main() -> Result<()> {
    let config: Config = curlyconf::from_str(CFG)?;
    println!("{:#?}", config);
    Ok(())
}
