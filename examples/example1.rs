use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Config {
    peer: Vec<Peer>,
    logger: Logger,
    color: Vec<Color>,
    #[serde(default)]
    either: Option<Either>,
}

#[derive(Debug, Deserialize)]
struct Peer {
    #[serde(rename = "__label__")]
    name: String,
    hostname: String,
}

#[derive(Debug, Deserialize)]
struct Logger {
    #[serde(rename = "__label__")]
    name: String,
    target: std::net::IpAddr,
    tuple: (u32, String),
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Color {
    Red,
    Green,
    Blue,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
enum Either {
    Peer(Peer),
    Logger(Logger),
    Users { name: String, id: u64 },
}

type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

fn main() -> Result<()> {
    env_logger::init();

    let cfg = r#"
        # Peer.
        logger syslog {
            target 1.2.3.4;
            tuple 10, 12;
        }
        peer foo {
            hostname "host1";
        }
        peer bar {
            hostname "host2";
        }
        color red, blue;
        either peer baz {
            hostname "host3";
        }
"#;

    let cfg2 = r#"
        # Peer.
        peer foo {
            hostname "host1"
        }
        peer bar {
            hostname "host2"
        }
        logger syslog {
            target 1.2.3.4
            tuple 10, "12"
        }
        color blue
        either users {
            name "mikevs"
            id 1000
        }
"#;

    match curlyconf::from_str(cfg) {
        Err(e) => println!("{}", e),
        Ok(config @ Config { .. }) => println!("{:#?}", config),
    }

    let cfg_parser = curlyconf::Builder::new()
        .mode(curlyconf::Mode::Newline)
        .from_str(cfg2);

    match cfg_parser {
        Err(e) => println!("{}", e),
        Ok(config @ Config { .. }) => println!("{:#?}", config),
    }

    Ok(())
}
