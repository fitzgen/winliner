use anyhow::{Context, Result};
use clap::Parser;
use std::io::Write;
use std::path::PathBuf;
use wasmtime_wasi::WasiCtx;
use winliner::{InstrumentationStrategy, Instrumenter, Optimizer, Profile, ProfileBuilder};

/// Winliner: The WebAssembly indirect call inliner!
///
/// Winliner speculatively inlines indirect calls in WebAssembly, based on
/// observed information from a previous profiling phase. This is a form of
/// profile-guided optimization (PGO) that we affectionately call *winlining*.
///
/// First, you ask Winliner to instrument your Wasm program to observe the
/// actual target callee of every indirect call site:
///
///     $ winliner instrument my-program.wasm > my-program.instrumented.wasm
///
/// Next, you run the instrumented program, building up a profile. This can
/// either be done in your Wasm environment of choice (e.g. the Web) with a
/// little glue code to extract and shepherd out the profile, or you can run
/// within Winliner itself and the Wasmtime-based WASI environment that comes
/// with it:
///
///     $ winliner profile my-program.instrumented.wasm > profile.json
///
/// Finally, you invoke Winliner again, this time providing it with the recorded
/// profile, and it optimizes your Wasm program based on the behavior observed
/// in that profile:
///
///     $ winliner optimize --profile profile.json my-program.wasm > my-program.winlined.wasm
///
/// You can also use Winliner as a library, rather than a CLI tool, if you
/// prefer. See https://docs.rs/winliner for details.
#[derive(Parser)]
#[clap(author, version, verbatim_doc_comment)]
enum Command {
    Instrument(InstrumentCommand),
    Profile(ProfileCommand),
    Merge(MergeCommand),
    Optimize(OptimizeCommand),
}

fn main() -> Result<()> {
    env_logger::init();
    match Command::parse() {
        Command::Instrument(i) => instrument(i),
        Command::Profile(p) => profile(p),
        Command::Merge(m) => merge(m),
        Command::Optimize(o) => optimize(o),
    }
}

#[derive(Parser)]
struct InstrumentCommand {
    #[clap(flatten)]
    instrumenter: Instrumenter,

    /// Where to write the instrumented output Wasm file to. The output is
    /// written to stdout if no path is supplied.
    #[clap(short, long)]
    output: Option<PathBuf>,

    /// The Wasm file to be instrumented.
    input: PathBuf,
}

fn instrument(command: InstrumentCommand) -> Result<()> {
    let input = std::fs::read(&command.input)
        .with_context(|| format!("failed to read input from '{}'", command.input.display()))?;

    let output = command.instrumenter.instrument(&input)?;

    if let Some(path) = command.output.as_ref() {
        std::fs::write(path, &output)
            .with_context(|| format!("failed to write output to '{}'", path.display()))?;
    } else {
        let stdout = std::io::stdout();
        let mut stdout = stdout.lock();
        stdout
            .write_all(&output)
            .context("failed to write output to stdout")?;
    }

    Ok(())
}

/// Execute an instrumented Wasm program and collect profiling data.
///
/// This command will set up a WASI environment, invoke the Wasm's exported
/// `_start` function, collect a profile of the execution, and finally write the
/// profile out to the configured destination.
#[derive(Parser)]
struct ProfileCommand {
    /// The strategy with which the Wasm was instrumented.
    #[clap(short, long, default_value = "three-globals")]
    strategy: InstrumentationStrategy,

    /// Which file system directories should be made available in the Wasm
    /// guest?
    ///
    /// None are available by default.
    #[clap(long = "dir", value_name = "directory")]
    dirs: Vec<PathBuf>,

    /// Which guest directories should be mapped to a host directory?
    ///
    /// The `--mapdir` option differs from `--dir` in that it allows giving a
    /// custom guest name to the directory that is different from its name in
    /// the host.
    ///
    /// None are mapped by default.
    #[clap(
        long = "mapdir",
        value_name = "GUEST_DIR::HOST_DIR",
        value_parser = parse_map_dirs,
    )]
    map_dirs: Vec<(PathBuf, PathBuf)>,

    /// Make the Wasm guest inherit `stdin`, `stderr`, and `stdout`.
    #[clap(long)]
    inherit_stdio: bool,

    /// Make the Wasm guest inherit environment variables.
    #[clap(long)]
    inherit_env: bool,

    /// Where to write the resulting profile to. The profile is written to
    /// stdout by default if no path is supplied.
    #[clap(short, long)]
    output: Option<PathBuf>,

    /// The path of the instrumented Wasm module.
    #[clap(value_name = "INSTRUMENTED_WASM")]
    wasm: PathBuf,

    /// Arguments passed through to Wasm.
    #[clap(last = true)]
    wasm_args: Vec<String>,
}

fn profile(command: ProfileCommand) -> Result<()> {
    use wasmtime::*;

    let engine = Engine::default();
    let module = Module::from_file(&engine, &command.wasm)?;

    let mut linker = Linker::new(&engine);
    wasmtime_wasi::add_to_linker(
        &mut linker,
        |(_, ctx): &mut (Option<ProfileBuilder>, WasiCtx)| ctx,
    )?;

    if command.strategy == InstrumentationStrategy::HostCalls {
        linker.func_wrap(
            "winliner",
            "add_indirect_call",
            |mut caller: Caller<(Option<ProfileBuilder>, WasiCtx)>, callee, call_site| {
                let builder = caller.data_mut().0.as_mut().unwrap();
                builder.add_indirect_call(callee, call_site);
            },
        )?;
    }

    let wasi_ctx = wasi_context(&command)?;
    let mut store = Store::new(
        &engine,
        (
            match command.strategy {
                InstrumentationStrategy::ThreeGlobals => None,
                InstrumentationStrategy::HostCalls => Some(ProfileBuilder::new()),
            },
            wasi_ctx,
        ),
    );

    let instance = linker.instantiate(&mut store, &module)?;
    let start = instance
        .get_typed_func::<(), ()>(&mut store, "_start")
        .context("Wasm module must export a `_start: [] -> []` function")?;
    start
        .call(&mut store, ())
        .context("Error when executing Wasm")?;

    let profile =
        match command.strategy {
            InstrumentationStrategy::ThreeGlobals => Profile::from_three_globals(|name| {
                match instance.get_global(&mut store, name)?.get(&mut store) {
                    Val::I32(x) => Some(x as u32 as u64),
                    Val::I64(x) => Some(x as u64),
                    _ => None,
                }
            })?,
            InstrumentationStrategy::HostCalls => store.data_mut().0.take().unwrap().build(),
        };

    let stdout;
    let output_name;
    let output: Box<dyn Write> = match command.output.as_ref() {
        Some(path) => {
            output_name = format!("'{}'", path.display());
            Box::new(std::io::BufWriter::new(std::fs::File::create(path)?))
        }
        None => {
            output_name = "stdout".to_string();
            stdout = std::io::stdout();
            let stdout = stdout.lock();
            Box::new(stdout)
        }
    };

    serde_json::to_writer(output, &profile)
        .with_context(|| format!("failed to write profile to {output_name}"))?;

    Ok(())
}

fn parse_map_dirs(s: &str) -> Result<(PathBuf, PathBuf)> {
    let parts: Vec<&str> = s.split("::").collect();
    if parts.len() != 2 {
        anyhow::bail!("`--map-dir` values must contain exactly one double colon ('::')");
    }
    Ok((parts[0].into(), parts[1].into()))
}

fn wasi_context(command: &ProfileCommand) -> Result<WasiCtx> {
    let mut ctx = wasi_cap_std_sync::WasiCtxBuilder::new();

    if command.inherit_stdio {
        ctx = ctx.inherit_stdio();
    }

    if command.inherit_env {
        ctx = ctx.inherit_env()?;
    }

    for dir in &command.dirs {
        log::info!("Preopening directory: {}", dir.display());
        let preopened = wasmtime_wasi::sync::Dir::open_ambient_dir(
            dir,
            wasmtime_wasi::sync::ambient_authority(),
        )
        .with_context(|| format!("failed to open directory: {}", dir.display()))?;
        ctx = ctx.preopened_dir(preopened, dir)?;
    }

    for (guest_dir, host_dir) in &command.map_dirs {
        log::info!(
            "Preopening directory: {}::{}",
            guest_dir.display(),
            host_dir.display()
        );
        let preopened = wasmtime_wasi::sync::Dir::open_ambient_dir(
            host_dir,
            wasmtime_wasi::sync::ambient_authority(),
        )
        .with_context(|| format!("failed to open directory: {}", host_dir.display()))?;
        ctx = ctx.preopened_dir(preopened, guest_dir)?;
    }

    ctx = ctx.args(&command.wasm_args)?;

    Ok(ctx.build())
}

/// Merge multiple profiles into a single profile.
#[derive(Parser)]
struct MergeCommand {
    /// Where to write the resulting merged profile to. The merged profile is
    /// written to stdout by default if no output path is supplied.
    #[clap(short, long)]
    output: Option<PathBuf>,

    /// The profiles to merge together.
    profiles: Vec<PathBuf>,
}

fn merge(command: MergeCommand) -> Result<()> {
    let mut merged = Profile::default();

    for path in &command.profiles {
        let file = std::fs::File::open(path)
            .with_context(|| format!("failed to open '{}'", path.display()))?;
        let file = std::io::BufReader::new(file);
        let profile = serde_json::from_reader(file)
            .with_context(|| format!("failed to read profile from '{}'", path.display()))?;
        merged.merge(&profile);
    }

    let stdout;
    let output_name;
    let output: Box<dyn Write> = match command.output.as_ref() {
        Some(path) => {
            output_name = format!("'{}'", path.display());
            Box::new(std::io::BufWriter::new(std::fs::File::create(path)?))
        }
        None => {
            output_name = "stdout".to_string();
            stdout = std::io::stdout();
            let stdout = stdout.lock();
            Box::new(stdout)
        }
    };

    serde_json::to_writer(output, &merged)
        .with_context(|| format!("failed to write merged profile to {output_name}"))?;

    Ok(())
}

/// TODO FITZGEN
#[derive(Parser)]
struct OptimizeCommand {
    #[clap(flatten)]
    optimizer: Optimizer,
}

fn optimize(command: OptimizeCommand) -> Result<()> {
    let _ = command;
    Ok(())
}
