use std::collections::HashMap;

use serde::Deserialize;

use newspeers::NewsPeer;

#[derive(Debug, Deserialize)]
struct Config {
    newspeer: HashMap<String, NewsPeer>,
}

type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

fn main() -> Result<()> {
    env_logger::init();

    // Read the configuration file.
    let config = match curlyconf::Builder::new()
        .alias::<Config>("label", "newspeer")
        .alias::<Config>("peer", "newspeer")
        .alias::<NewsPeer>("addgroup", "groups")
        .alias::<NewsPeer>("delgroup", "groups")
        .alias::<NewsPeer>("delgroupany", "groups")
        .alias::<NewsPeer>("alias", "pathalias")
        .alias::<NewsPeer>("addist", "distributions")
        .alias::<NewsPeer>("deldist", "distributions")
        .ignore::<NewsPeer>("realtime")
        .from_file("examples/newspeers.cfg") {
        Ok(cfg @ Config{..}) => cfg,
        Err(e) => {
            println!("{}", e);
            std::process::exit(1);
        },
    };

    println!("{:#?}", config);
    Ok(())
}

mod newspeers {
    use std::fmt;
    use std::net::IpAddr;

    use curlyconf::{Parser, ParserAccess};
    use serde::{de::Deserializer, Deserialize, de::SeqAccess, de::Visitor};
    use ipnet::IpNet;

    #[derive(Default, Debug, Clone, Deserialize)]
    #[serde(default)]
    pub struct NewsPeer {
        /// Name of this feed.
        #[serde(rename = "__label__")]
        pub label:              String,

        /// used both to filter incoming and outgoing articles.
        pub pathalias:          Vec<String>,

        /// used on connects from remote host
        pub inhost:             Vec<String>,
        pub innet:              Vec<IpNet>,
        pub maxconnect:         u32,
        pub readonly:           bool,

        /// used when processing incoming articles
        pub filter:             WildMatList,
        pub nomismatch:         bool,
        pub precomreject:       bool,

        /// used to select outgoing articles.
        pub maxcross:           u32,
        pub maxpath:            u32,
        pub maxsize:            u64,
        pub minsize:            u64,
        pub mincross:           u32,
        pub minpath:            u32,
        pub arttypes:           Vec<String>,
        pub groups:             WildMatList,
        pub requiregroups:      WildMatList,
        pub distributions:      Vec<String>,
        pub hashfeed:           HashFeed,

        /// used with the outgoing feed.
        #[serde(rename = "hostname")]
        pub outhost:            String,
        pub bindaddress:        Option<IpAddr>,
        pub port:               u16,
        pub maxparallel:        u32,
        pub maxstream:          u32,
        pub nobatch:            bool,
        pub maxqueue:           u32,
        pub send_headfeed:      bool,
        pub accept_headfeed:    bool,
        pub preservebytes:      bool,

        /// non-config items.
        #[serde(skip)]
        pub index:              usize,
    }

    #[derive(Default, Debug, Clone)]
    pub struct WildMatList(Vec<String>);

    impl<'de> Deserialize<'de> for WildMatList {

        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            struct WildMatVisitor { parser: Parser }

            impl<'de> Visitor<'de> for WildMatVisitor {

                type Value = WildMatList;

                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str("a wildmat list")
                }

                fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                where
                    A: SeqAccess<'de>,
                {
                    let mut values = Vec::new();
                    while let Some(mut value) = seq.next_element::<String>()? {
                        match self.parser.value_name().as_str() {
                            "delgroup" => value.insert_str(0, "!"),
                            "delgroupany" => value.insert_str(0, "@"),
                            _ => {},
                        }
                        values.push(value);
                    }

                    Ok(WildMatList(values))
                }
            }

            let parser = deserializer.parser();
            deserializer.deserialize_seq(WildMatVisitor{ parser })
        }
    }

    #[derive(Default, Debug, Clone)]
    pub struct HashFeed(Vec<String>);

    impl<'de> Deserialize<'de> for HashFeed {

        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            struct HashFeedVisitor;

            impl<'de> Visitor<'de> for HashFeedVisitor {
                type Value = HashFeed;

                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str("a wildmat list")
                }

                fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                where
                    A: SeqAccess<'de>,
                {
                    let mut values = Vec::new();

                    while let Some(value) = seq.next_element()? {
                        values.push(value);
                    }

                    Ok(HashFeed(values))
                }
            }

            deserializer.deserialize_seq(HashFeedVisitor)
        }
    }
}
