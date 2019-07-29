#![warn(clippy::all)]

use j4rs::{ClasspathEntry, Instance, InvocationArg, Jvm, JvmBuilder};
use std::error::Error;

static CP_ENTRIES: &[&str] = &[
    "/home/fred/Eclipse/javaparser-javaparser-parent-3.13.7/\
     javaparser-core/target/javaparser-core-3.13.7.jar",
    "/home/fred/Eclipse/javaparser-javaparser-parent-3.13.7/\
     javaparser-core-serialization/target/javaparser-core-serialization-3.13.7.jar",
    "/home/fred/Eclipse/javaparser-javaparser-parent-3.13.7/\
     javaparser-symbol-solver-core/target/javaparser-symbol-solver-core-3.13.7.jar",
    "/home/fred/Eclipse/javaparser-javaparser-parent-3.13.7/\
     javaparser-symbol-solver-model/target/javaparser-symbol-solver-model-3.13.7.jar",
];

fn classpaths() -> Vec<ClasspathEntry<'static>> {
    CP_ENTRIES
        .into_iter()
        .map(|e| ClasspathEntry::new(e))
        .collect()
}

const TYPE_SOLVER: &str = "com.github.javaparser.symbolsolver.model.resolution.TypeSolver";
const COMBINED_TYPE_SOLVER: &str =
    "com.github.javaparser.symbolsolver.resolution.typesolvers.CombinedTypeSolver";
const REFLECTION_TYPE_SOLVER: &str =
    "com.github.javaparser.symbolsolver.resolution.typesolvers.ReflectionTypeSolver";

macro_rules! jcreate {
    ($jvm:ident, $name:expr) => {
        $jvm.create_instance($name, &[])
    };
    ($jvm:ident, $name:expr, $($arg:expr),+) => {
        $jvm.create_instance($name, &[$(InvocationArg::from($arg),)+])
    };
}

macro_rules! jcreate_array {
    ($jvm:ident, $elem_class:expr) => {
        $jvm.create_java_array($elem_class, &[])
    };
    ($jvm:ident, $elem_class:expr, $($elem:expr),+) => {
        $jvm.create_java_array($elem_class, &[$(InvocationArg::from($elem),)+])
    };
}

macro_rules! jcast {
    ($jvm:ident, $typ:expr, $inst:expr) => {
        $jvm.cast($inst, $typ)
    };
}

// macro_rules! jcreate_variadic {
//     ($jvm:ident, $name:expr) => {
//         $jvm.create_instance($name, &[])
//     };
//     ($jvm:ident, $name:expr, $($arg:expr),+) => {

//         $jvm.create_instance($name, )
//     };
// };

fn main() -> Result<(), Box<dyn Error>> {
    let jvm = JvmBuilder::new().classpath_entries(classpaths()).build()?;
    let reflection_ts = jcreate!(jvm, REFLECTION_TYPE_SOLVER)?;
    let reflection_ts_cast = jcast!(jvm, TYPE_SOLVER, &reflection_ts)?;
    let combined_ts_args = jcreate_array!(jvm, TYPE_SOLVER, reflection_ts_cast)?;
    let combined_ts = jvm.create_instance(
        COMBINED_TYPE_SOLVER,
        &[InvocationArg::from(combined_ts_args)],
    )?;
    Ok(())
}
