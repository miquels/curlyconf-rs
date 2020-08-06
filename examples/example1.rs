use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Config {
    peer: Vec<Peer>,
    logger: Logger,
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
"#;

    match curlyconf::from_str(cfg, curlyconf::Mode::Semicolon) {
        Err(e) => println!("{}", e),
        Ok(config @ Config { .. }) => println!("{:#?}", config),
    }

    match curlyconf::from_str(cfg2, curlyconf::Mode::Newline) {
        Err(e) => println!("{}", e),
        Ok(config @ Config { .. }) => println!("{:#?}", config),
    }

    Ok(())
}
