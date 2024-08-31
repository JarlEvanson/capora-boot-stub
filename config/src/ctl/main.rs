//! Helper executable for managing `capora-boot-stub`'s configuration.

use cli::parse_arguments;

pub mod cli;

fn main() {
    match parse_arguments() {
        cli::Action::Configure {
            path,
            application,
            modules,
        } => todo!(),
    }
}
