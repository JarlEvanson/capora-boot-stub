//! Build script to `test-application`.

fn main() {
    println!("cargo::rustc-link-arg=-Ttest-application/linker_script.ld");
}
