use clap::Parser;
use serde::Deserialize;
use std::error::Error;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::{fmt, io};

use crate::interpretter::SprocketInterpretter;
use crate::parser::SprocketParser;
use crate::sprocket::{SprocketError, SprocketResult};

#[derive(Deserialize)]
#[serde(default)]
pub struct Config {}

impl Default for Config {
    fn default() -> Self {
        Config {}
    }
}

impl Config {}

/// The sprocket language toolkit.
#[derive(Parser)]
#[clap(author, version, about)]
pub struct Cli {
    /// Path to configuration file
    #[clap(parse(from_os_str), short = 'c', long = "config")]
    config_path: Option<PathBuf>,

    /// Path to input file
    #[clap(parse(from_os_str), short = 'i', long = "input")]
    input_path: Option<PathBuf>,

    /// Path to output file
    #[clap(parse(from_os_str), short = 'o', long = "output")]
    output_path: Option<PathBuf>,
}

impl Cli {
    fn config(self: &Cli) -> Config {
        let config: Config = match &self.config_path {
            Some(config_path) => {
                serde_json::from_str(&std::fs::read_to_string(config_path).unwrap()).unwrap()
            }
            None => {
                let default_path = PathBuf::from("./sprocket.conf");
                let config_json = std::fs::read_to_string(&default_path);
                match config_json {
                    Ok(config_json) => serde_json::from_str(&config_json).unwrap(),
                    Err(_) => Config::default(),
                }
            }
        };
        config
    }

    pub fn run() -> SprocketResult<()> {
        let cli = Cli::parse();
        let _config = cli.config();
        if let Some(_) = &cli.input_path {
            return cli.run_parse_input();
        }
        cli.run_repl()
    }

    pub fn run_parse_input(&self) -> SprocketResult<()> {
        let input_path = match &self.input_path {
            Some(input_path) => input_path,
            None => return Err(SprocketError::MiscError),
        };
        let default_dest = PathBuf::from("./sprocket.dump");
        let dest = match &self.output_path {
            Some(output_path) => output_path,
            None => &default_dest,
        };
        let source = std::fs::read_to_string(input_path.to_str().unwrap()).unwrap();
        let interp = SprocketInterpretter::new(&source)?;
        let mut dest_buf = match File::create(dest) {
            Ok(dest_buf) => dest_buf,
            Err(_) => return Err(SprocketError::MiscError),
        };
        let main_task = match interp.call_stack.lookup_task("__main__")? {
            Some(task) => task,
            None => return Err(SprocketError::MiscError),
        };
        for ast in main_task {
            dest_buf.write_all(format!("{:?}", &ast).as_bytes()).unwrap();
            dest_buf.write_all("\n".as_bytes()).unwrap();
        }
        interp.run().unwrap();
        Ok(())
    }

    pub fn run_repl(&self) -> SprocketResult<()> {
        let mut parser = SprocketParser::new();
        loop {
            let mut stdout = io::stdout();
            stdout.write_all(b"\xE2\x9A\x99 > ").unwrap();
            stdout.flush().unwrap();

            let mut buf = String::new();
            match io::stdin().read_line(&mut buf) {
                Ok(_) => {}
                Err(_) => return Err(SprocketError::MiscError),
            };
            buf = buf.trim().to_string();
            if buf == "exit" {
                break;
            }
            match parser.parse(&buf) {
                Ok(ast) => {
                    for ast_node in ast {
                        println!("{:?}", ast_node);
                    }
                    parser.reset();
                }
                // TODO: we could make this REPL allow continuing user input across newlines, but this will require some work at the parser.
                Err(err) => {
                    println!("{:?}", err);
                }
            }
            buf.clear();
        }
        Ok(())
    }
}
