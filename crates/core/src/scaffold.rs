use std::{
    fs::{self, File},
    io::{Result as IoResult, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

pub fn read_treesitter_grammars(treesitter_dirs: &[PathBuf]) -> String {
    let mut relations = String::new();
    for dir in treesitter_dirs {
        relations.push_str(&format!("// Stub: Reading Tree-Sitter folder: {}\n", dir.display()));
    }
    relations.push_str("// TODO: actual Tree-Sitter -> DDlog generation\n");
    relations
}

#[derive(Debug)]
pub struct DslAst {
    pub expressions: Vec<String>,
}

pub fn parse_custom_dsl(source: &str) -> DslAst {
    let expressions = source
        .lines()
        .map(|l| l.trim().to_string())
        .filter(|l| !l.is_empty())
        .collect();
    DslAst { expressions }
}

pub fn dsl_to_ddlog(ast: &DslAst) -> String {
    let mut ddlog_code = String::new();
    for (idx, expr) in ast.expressions.iter().enumerate() {
        ddlog_code.push_str(&format!(
            "// DSL expression #{} -> DDlog\n// Original: {}\n// TODO: Actual DSL -> DDlog logic.\n\n",
            idx + 1,
            expr
        ));
    }
    ddlog_code
}

pub fn generate_rust_project(base_dir: &Path, project_name: &str, dl_content: &str) {
    let cargo_toml_content = format!(
        r#"[package]
name = "{project_name}"
version = "0.1.0"
edition = "2021"

[dependencies]
differential-datalog = "0.50"
"#,
        project_name = project_name
    );

    let cargo_toml_path = base_dir.join("Cargo.toml");
    let mut cargo_toml_file =
        File::create(&cargo_toml_path).expect("Failed to create Cargo.toml");
    cargo_toml_file
        .write_all(cargo_toml_content.as_bytes())
        .expect("Failed to write Cargo.toml");

    let dl_file_path = base_dir.join(format!("{project_name}.dl"));
    let mut dl_file = File::create(&dl_file_path).expect("Failed to create .dl file");
    dl_file
        .write_all(dl_content.as_bytes())
        .expect("Failed to write .dl file");

    let src_dir = base_dir.join("src");
    fs::create_dir_all(&src_dir).expect("Failed to create src directory");

    let lib_file_path = src_dir.join("lib.rs");
    let lib_code = r#"pub fn hello() {
    println!("Hello from the generated DDlog crate!");
}
"#;
    let mut lib_file = File::create(&lib_file_path).expect("Failed to create lib.rs");
    lib_file
        .write_all(lib_code.as_bytes())
        .expect("Failed to write lib.rs");

    write_toolchain_toml(base_dir).expect("Failed to write rust-toolchain.toml");
}

fn write_toolchain_toml(base_dir: &Path) -> IoResult<()> {
    let toolchain_path = base_dir.join("rust-toolchain.toml");
    let toolchain_content = r#"[toolchain]
channel = "1.76"
"#;
    fs::write(toolchain_path, toolchain_content)
}

pub fn build_ddlog_crate(base_dir: &Path, project_name: &str) -> Result<(), String> {
    if which::which("ddlog").is_err() {
        return Err("`ddlog` binary not found on PATH".into());
    }

    let dl_file = format!("{}.dl", project_name);
    let ddlog_status = Command::new("ddlog")
        .args(["-i", &dl_file, "-L", "../../lib"])
        .current_dir(base_dir)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| format!("Failed to launch ddlog command: {}", e))?;

    if !ddlog_status.success() {
        return Err(format!("ddlog command failed on {:?}", dl_file));
    }

    let cargo_status = Command::new("cargo")
        .arg("build")
        .current_dir(base_dir)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| format!("Failed to launch cargo build: {}", e))?;

    if !cargo_status.success() {
        return Err("Cargo build failed.".into());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;
    use std::{fs, io::Read};

    #[test]
    fn test_generate_rust_project() {
        let dir = tempdir().expect("Failed to create temp dir");
        let project_name = "test_project";
        let dl_content = "// test dl content";
        let base_dir = dir.path();

        generate_rust_project(base_dir, project_name, dl_content);

        let cargo_toml_path = base_dir.join("Cargo.toml");
        assert!(cargo_toml_path.exists());

        let mut cargo_toml_data = String::new();
        fs::File::open(&cargo_toml_path)
            .unwrap()
            .read_to_string(&mut cargo_toml_data)
            .unwrap();
        assert!(cargo_toml_data.contains("name = \"test_project\""));

        let dl_path = base_dir.join("test_project.dl");
        assert!(dl_path.exists());

        let mut dl_data = String::new();
        fs::File::open(&dl_path)
            .unwrap()
            .read_to_string(&mut dl_data)
            .unwrap();
        assert_eq!(dl_data, dl_content);

        let lib_rs_path = base_dir.join("src/lib.rs");
        assert!(lib_rs_path.exists());

        let toolchain_path = base_dir.join("rust-toolchain.toml");
        assert!(toolchain_path.exists());

        let mut toolchain_data = String::new();
        fs::File::open(&toolchain_path)
            .unwrap()
            .read_to_string(&mut toolchain_data)
            .unwrap();
        assert!(toolchain_data.contains("channel = \"1.76\""));
    }

    #[test]
    fn test_build_ddlog_crate() {
        if which::which("ddlog").is_err() {
            eprintln!("Skipping test_build_ddlog_crate because ddlog is not installed");
            return;
        }

        let dir = tempdir().expect("Failed to create temp dir");
        let project_name = "test_build";
        let dl_content = "input relation Test(x: signed<64>)";

        generate_rust_project(dir.path(), project_name, dl_content);

        let build_result = build_ddlog_crate(dir.path(), project_name);
        assert!(build_result.is_ok());
    }
}
