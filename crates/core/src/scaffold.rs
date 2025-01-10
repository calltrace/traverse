use std::{fs, io::Write, path::Path};

fn generate_rust_project(base_dir: &Path, project_name: &str, dl_content: &str) {
    let cargo_toml_content = format!(
        r#"[package]
name = "{project_name}"
version = "0.1.0"
edition = "2021"

[dependencies]
differential-datalog = "0.50" # Adjust version as needed
"#,
        project_name = project_name
    );

    let cargo_toml_path = base_dir.join("Cargo.toml");
    let mut cargo_toml_file = fs::File::create(&cargo_toml_path).expect("Failed to create Cargo.toml");
    cargo_toml_file
        .write_all(cargo_toml_content.as_bytes())
        .expect("Failed to write Cargo.toml");

    let dl_file_path = base_dir.join(format!("{project_name}.dl", project_name = project_name));
    let mut dl_file = fs::File::create(&dl_file_path).expect("Failed to create .dl file");
    dl_file
        .write_all(dl_content.as_bytes())
        .expect("Failed to write .dl file");

    let src_dir = base_dir.join("src");
    fs::create_dir_all(&src_dir).expect("Failed to create src directory");
    let lib_file_path = src_dir.join("lib.rs");
    let lib_code = r#"// Placeholder lib.rs file
// Generated code will go here.
"#;
    let mut lib_file = fs::File::create(&lib_file_path).expect("Failed to create lib.rs");
    lib_file
        .write_all(lib_code.as_bytes())
        .expect("Failed to write lib.rs");

    let script_path = base_dir.join("generate_ddlog_crate.sh");
    let script_content = format!(
        r#"#!/bin/bash

set -ex

# Ensure ddlog CLI is available
if ! command -v ddlog &> /dev/null
then
    echo "ddlog could not be found. Install it first."
    exit 1
fi

THIS_DIR="$( cd "$( dirname "${{BASH_SOURCE[0]}}" )" >/dev/null 2>&1 && pwd )"

# Run DDlog to generate Rust code
(cd "${{THIS_DIR}}" && ddlog -i {dl_file} -L ../../lib)

# Build the Rust project
(cd "${{THIS_DIR}}" && cargo build)
"#,
        dl_file = format!("{project_name}.dl", project_name = project_name)
    );

    let mut script_file = fs::File::create(&script_path).expect("Failed to create shell script");
    script_file
        .write_all(script_content.as_bytes())
        .expect("Failed to write shell script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&script_path)
            .expect("Failed to get script file metadata")
            .permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&script_path, perms).expect("Failed to set script file permissions");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs, io::Read, os::unix::fs::PermissionsExt};
    use tempfile::tempdir;

    #[test]
    fn test_generate_rust_project() {
        let project_name = "test_project";
        let dl_content = r#"
input relation Word1(word: string, cat: string)
input relation Word2(word: string, cat: string)
"#;

        let temp_dir = tempdir().expect("Failed to create temporary directory");
        let base_dir = temp_dir.path();

        generate_rust_project(base_dir, project_name, dl_content);

        let cargo_toml_path = base_dir.join("Cargo.toml");
        assert!(cargo_toml_path.exists(), "Cargo.toml file was not created");
        let mut cargo_toml_content = String::new();
        fs::File::open(&cargo_toml_path)
            .expect("Failed to open Cargo.toml")
            .read_to_string(&mut cargo_toml_content)
            .expect("Failed to read Cargo.toml");
        assert!(cargo_toml_content.contains("[package]"));
        assert!(cargo_toml_content.contains("name = \"test_project\""));

        let dl_file_path = base_dir.join("test_project.dl");
        assert!(dl_file_path.exists(), ".dl file was not created");
        let mut dl_file_content = String::new();
        fs::File::open(&dl_file_path)
            .expect("Failed to open .dl file")
            .read_to_string(&mut dl_file_content)
            .expect("Failed to read .dl file");
        assert_eq!(dl_file_content, dl_content);

        let lib_file_path = base_dir.join("src/lib.rs");
        assert!(lib_file_path.exists(), "lib.rs file was not created");

        let script_path = base_dir.join("generate_ddlog_crate.sh");
        assert!(script_path.exists(), "Shell script was not created");
        let mut script_content = String::new();
        fs::File::open(&script_path)
            .expect("Failed to open shell script")
            .read_to_string(&mut script_content)
            .expect("Failed to read shell script");
        assert!(script_content.contains("#!/bin/bash"));
        assert!(script_content.contains("ddlog -i test_project.dl"));
        assert!(script_content.contains("cargo build"));

        #[cfg(unix)]
        {
            let perms = fs::metadata(&script_path)
                .expect("Failed to get shell script metadata")
                .permissions();
            assert_eq!(perms.mode() & 0o777, 0o755, "Shell script is not executable");
        }
    }
}
