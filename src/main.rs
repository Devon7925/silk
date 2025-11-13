use silk::Diagnostic;
use silk::compile;
use std::env;
use std::fmt;
use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;
use std::process;

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        process::exit(1);
    }
}

fn run() -> Result<(), CliError> {
    match parse_invocation()? {
        Invocation::Help => {
            print_help();
            Ok(())
        }
        Invocation::Version => {
            print_version();
            Ok(())
        }
        Invocation::Run(options) => execute(options),
    }
}

fn execute(options: CliOptions) -> Result<(), CliError> {
    let (source, label) = read_source(&options.input)?;
    let wasm = compile(source.clone()).map_err(|diagnostic| CliError::Compilation {
        diagnostic,
        source_code: source,
        filename: label,
    })?;
    write_output(&options.output, &wasm)
}

fn parse_invocation() -> Result<Invocation, CliError> {
    let mut args = env::args().skip(1);
    let mut output = OutputTarget::Stdout;
    let mut input = None;
    let mut allow_flags = true;

    while let Some(arg) = args.next() {
        if allow_flags {
            match arg.as_str() {
                "-h" | "--help" => return Ok(Invocation::Help),
                "-V" | "--version" => return Ok(Invocation::Version),
                "-o" | "--output" => {
                    let value = args.next().ok_or(CliError::MissingValue("--output"))?;
                    if value == "-" {
                        output = OutputTarget::Stdout;
                    } else {
                        output = OutputTarget::File(PathBuf::from(value));
                    }
                    continue;
                }
                "--" => {
                    allow_flags = false;
                    continue;
                }
                _ => {}
            }

            if arg.starts_with('-') {
                return Err(CliError::UnexpectedArgument(arg));
            }
        }

        if input.is_none() {
            input = Some(InputSource::from_arg(arg));
        } else {
            return Err(CliError::UnexpectedArgument(arg));
        }
    }

    Ok(Invocation::Run(CliOptions {
        input: input.ok_or(CliError::MissingInput)?,
        output,
    }))
}

fn read_source(input: &InputSource) -> Result<(String, String), CliError> {
    match input {
        InputSource::File(path) => {
            let contents =
                fs::read_to_string(path).map_err(|err| CliError::io(Some(path.clone()), err))?;
            Ok((contents, path.display().to_string()))
        }
        InputSource::Stdin => {
            let mut contents = String::new();
            io::stdin()
                .read_to_string(&mut contents)
                .map_err(|err| CliError::io(None, err))?;
            Ok((contents, "<stdin>".to_string()))
        }
    }
}

fn write_output(target: &OutputTarget, bytes: &[u8]) -> Result<(), CliError> {
    match target {
        OutputTarget::Stdout => {
            let mut stdout = io::stdout();
            stdout
                .write_all(bytes)
                .map_err(|err| CliError::io(None, err))?;
            stdout.flush().map_err(|err| CliError::io(None, err))
        }
        OutputTarget::File(path) => {
            fs::write(path, bytes).map_err(|err| CliError::io(Some(path.clone()), err))
        }
    }
}

fn print_help() {
    println!(
        "silk v{version}

Usage:
  silk [OPTIONS] <FILE | ->

Options:
  -o, --output <FILE>   Write the emitted wasm module to <FILE>
  -h, --help            Show this help message
  -V, --version         Show version information

If FILE is '-', source is read from stdin. The wasm module is written to stdout when no \
output file is provided.",
        version = env!("CARGO_PKG_VERSION"),
    );
}

fn print_version() {
    println!("silk {}", env!("CARGO_PKG_VERSION"));
}

#[derive(Debug)]
enum Invocation {
    Help,
    Version,
    Run(CliOptions),
}

#[derive(Debug)]
struct CliOptions {
    input: InputSource,
    output: OutputTarget,
}

#[derive(Debug)]
enum InputSource {
    File(PathBuf),
    Stdin,
}

impl InputSource {
    fn from_arg(arg: String) -> Self {
        if arg == "-" {
            InputSource::Stdin
        } else {
            InputSource::File(PathBuf::from(arg))
        }
    }
}

#[derive(Debug)]
enum OutputTarget {
    Stdout,
    File(PathBuf),
}

#[derive(Debug)]
enum CliError {
    MissingInput,
    MissingValue(&'static str),
    UnexpectedArgument(String),
    Io {
        path: Option<PathBuf>,
        source: io::Error,
    },
    Compilation {
        diagnostic: Diagnostic,
        source_code: String,
        filename: String,
    },
}

impl CliError {
    fn io(path: Option<PathBuf>, source: io::Error) -> Self {
        CliError::Io { path, source }
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CliError::MissingInput => {
                write!(f, "No input file supplied. Run `silk --help` for usage.")
            }
            CliError::MissingValue(flag) => {
                write!(
                    f,
                    "Flag {flag} requires a value. Run `silk --help` for usage."
                )
            }
            CliError::UnexpectedArgument(arg) => write!(
                f,
                "Unexpected argument `{arg}`. Run `silk --help` for usage."
            ),
            CliError::Io { path, source } => {
                if let Some(path) = path {
                    write!(f, "I/O error for {}: {}", path.display(), source)
                } else {
                    write!(f, "I/O error: {source}")
                }
            }
            CliError::Compilation {
                diagnostic,
                source_code,
                filename,
            } => write!(
                f,
                "Compilation failed for {filename}:\n{}",
                diagnostic.render_with_source(source_code)
            ),
        }
    }
}
