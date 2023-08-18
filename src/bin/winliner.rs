use clap::Parser;

/// TODO FITZGEN
#[derive(Parser)]
enum Command {
    Instrument(winliner::Instrumenter),
    Profile(ProfileCommand),
    Merge(MergeCommand),
    Optimize(winliner::Optimizer),
}

/// TODO FITZGEN
#[derive(Parser)]
struct ProfileCommand {}

/// TODO FITZGEN
#[derive(Parser)]
struct MergeCommand {}

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let _options = Command::parse();
    todo!()
}
