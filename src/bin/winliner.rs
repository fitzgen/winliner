use clap::Parser;

#[derive(Parser)]
pub struct Options {}

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let options = Options::parse();
    todo!()
}
