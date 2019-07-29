#![warn(clippy::all)]

use j4rs::{ClasspathEntry, Instance, InvocationArg, Jvm, JvmBuilder};
use std::error::Error;
use std::fmt::{self, Display};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    #[structopt(short = "f", long = "file")]
    filename: String,
}

static CP_ENTRIES: &[&str] = &[
    "/home/fred/Eclipse/JATParser/target/JATParser-0.1.0-jar-with-dependencies.jar",
    // "/home/fred/Eclipse/javaparser-javaparser-parent-3.13.7/\
    //  javaparser-core/target/javaparser-core-3.13.7.jar",
    // "/home/fred/Eclipse/javaparser-javaparser-parent-3.13.7/\
    //  javaparser-core-serialization/target/javaparser-core-serialization-3.13.7.jar",
    // "/home/fred/Eclipse/javaparser-javaparser-parent-3.13.7/\
    //  javaparser-symbol-solver-core/target/javaparser-symbol-solver-core-3.13.7.jar",
    // "/home/fred/Eclipse/javaparser-javaparser-parent-3.13.7/\
    //  javaparser-symbol-solver-model/target/javaparser-symbol-solver-model-3.13.7.jar",
];

fn classpaths() -> Vec<ClasspathEntry<'static>> {
    CP_ENTRIES.iter().map(|e| ClasspathEntry::new(e)).collect()
}

#[derive(Debug)]
enum JATError {
    ParserError(u32),
}

impl Display for JATError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            JATError::ParserError(retcode) => write!(f, "Parser error, return code = {}", retcode),
        }
    }
}

impl Error for JATError {}

fn main() -> Result<(), Box<dyn Error>> {
    let opt = Opt::from_args();

    let jvm = JvmBuilder::new().classpath_entries(classpaths()).build()?;

    let parser = jvm.create_instance("main.Parser", &[])?;
    jvm.invoke(&parser, "parse", &[InvocationArg::from(opt.filename)])?;

    let retcode = jvm.invoke(&parser, "getReturnCode", &[])?;
    let retcode: u32 = jvm.to_rust(retcode)?;

    if retcode != 0 {
        eprintln!("Error running JATParser, error code = {}", retcode);

        let log = jvm.invoke(&parser, "getLog", &[])?;
        let log: String = jvm.to_rust(log)?;

        eprintln!("Log: {}", log);

        return Err(Box::new(JATError::ParserError(retcode)));
    }

    let result = jvm.invoke(&parser, "getResult", &[])?;
    let result: String = jvm.to_rust(result)?;

    println!("Result:");
    println!("{}", result);

    Ok(())
}
