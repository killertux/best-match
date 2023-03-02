use std::{
    fs::File,
    io::{stdin, stdout, BufRead, BufReader, BufWriter, Cursor, Write},
    path::PathBuf,
};

use anyhow::Result;
use clap::Parser;
use index::Index;

use crate::tokenizer::Tokenizer;

mod index;
mod tokenizer;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
enum Args {
    CreateIndex {
        #[arg(short, long)]
        path_to_index: PathBuf,
        #[arg(short, long)]
        output: PathBuf,
        #[arg(short, long)]
        whitelist_extension: Vec<String>,
    },
    Search {
        #[arg(short, long)]
        index_path: PathBuf,
    },
}

fn main() -> Result<()> {
    let args = Args::parse();
    match args {
        Args::CreateIndex {
            path_to_index,
            output,
            whitelist_extension,
        } => {
            let index = Index::build_from_dir(path_to_index, &whitelist_extension)?;
            ciborium::ser::into_writer(&index, BufWriter::new(File::create(output)?))?
        }
        Args::Search { index_path } => {
            println!("Loading index");
            let index: Index = ciborium::de::from_reader(BufReader::new(File::open(index_path)?))?;
            let stdin = BufReader::new(stdin());
            print!("Type a search term > ");
            stdout().flush()?;
            let lines = stdin.lines();
            for line in lines {
                let line = line?;
                let terms = Tokenizer::new(Cursor::new(line));
                let mut result = index.search(terms);
                result.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
                println!("### Results ###");
                result
                    .into_iter()
                    .take(10)
                    .for_each(|(path, score)| println!("{path} => {score}", path = path.display()));
                print!("Type a search term > ");
                stdout().flush()?;
            }
        }
    }
    Ok(())
}
