use std::{
    fs::{self, File},
    io::{Read, Result as IoResult, Write},
    path::Path,
    process::{Command, Stdio},
};
use tempdir::TempDir;

use crate::facts::DDLogCommand;

pub fn validate(dl_program: &str) -> Result<String, String> {
    if which::which("ddlog").is_err() {
        return Err("`ddlog` binary not found on PATH".into());
    }

    // save the program to a temporary file using Temp crate
    let temp_dir = TempDir::new("ddlog")
        .map_err(|e| format!("Failed to create temporary directory: {}", e))?;
    let dl_file = temp_dir.path().join("program.dl");
    let mut file =
        File::create(&dl_file).map_err(|e| format!("Failed to create .dl file: {}", e))?;
    file.write_all(dl_program.as_bytes())
        .map_err(|e| format!("Failed to write .dl file: {}", e))?;


    let mut ddlog_run = Command::new("ddlog")
        .args(["-i", &dl_file.to_string_lossy(), "--action=validate"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|e| format!("Failed to launch ddlog command: {}", e))?;

    let status = ddlog_run
        .wait()
        .map_err(|e| format!("Failed to wait for ddlog process: {}", e))?;

    if status.success() {
        let mut stdout = ddlog_run.stdout.take().ok_or("Failed to get stdout")?;

        // read stdout
        let mut buffer = Vec::new();
        stdout
            .read_to_end(&mut buffer)
            .map_err(|e| format!("Failed to read stdout: {}", e))?;

        let bytes = bytes::Bytes::from(buffer);
        Ok(String::from_utf8_lossy(&bytes).into())
    } else {
        let exit_code = status.code().unwrap_or(-1);
        Err(format!(
            "DDlog validation failed with exit code: {}\n",
            exit_code,
        ))
    }
}

pub fn generate_rust_project(base_dir: &Path, project_name: &str, dl_content: &str) {
    let cargo_toml_content = format!(
        r#"[package]
name = "{project_name}"
version = "0.1.0"
edition = "2021"

[lib]
name = "{project_name}"

[[bin]]
name = "{project_name}"
path = "src/main.rs"

[dependencies]
differential-datalog = "0.50"
"#,
        project_name = project_name
    );

    let cargo_toml_path = base_dir.join("Cargo.toml");
    let mut cargo_toml_file = File::create(&cargo_toml_path).expect("Failed to create Cargo.toml");
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

    let main_file_path = src_dir.join("main.rs");
    let main_code = format!(
        r#"fn main() {{
    {crate_name}::hello();
}}
"#,
        crate_name = project_name
    );
    let mut main_file = File::create(&main_file_path).expect("Failed to create main.rs");
    main_file
        .write_all(main_code.as_bytes())
        .expect("Failed to write main.rs");

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

    let project_dir = format!(
        "{}/{}_ddlog",
        base_dir.to_str().unwrap_or_default(),
        project_name
    );

    let cargo_status = Command::new("cargo")
        .args(["+1.76", "build"])
        .current_dir(project_dir)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .map_err(|e| format!("Failed to launch cargo build: {}", e))?;

    if !cargo_status.success() {
        return Err("Cargo build failed.".into());
    }

    Ok(())
}

pub fn run_ddlog_crate(
    base_dir: &Path,
    project_name: &str,
    cmds: &[DDLogCommand],
) -> Result<String, String> {
    let project_dir = format!(
        "{}/{}_ddlog",
        base_dir.to_str().unwrap_or_default(),
        project_name
    );
    let dat_content = cmds
        .iter()
        .map(|cmd| cmd.to_string())
        .collect::<Vec<String>>()
        .join("\n");

    let exec_path = format!("target/debug/{}_cli", project_name);

    println!(
        "Running generated DDLog application: {}/{}",
        project_dir, exec_path
    );
    let mut ddlog_app_run = Command::new(&exec_path)
        .current_dir(project_dir)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|e| {
            format!(
                "Failed to launch generated DDLog application: {} ({})",
                e, exec_path
            )
        })?;

    // print just first 80 lines of the facts
    println!(
        "Facts:\n{}",
        dat_content
            .lines()
            .take(80)
            .collect::<Vec<&str>>()
            .join("\n")
    );
    write!(ddlog_app_run.stdin.as_ref().unwrap(), "{}", dat_content).unwrap();

    if ddlog_app_run.wait().is_ok() {
        let mut stdout = ddlog_app_run.stdout.take().ok_or("Failed to get stdout")?;

        let mut buffer = Vec::new();
        stdout
            .read_to_end(&mut buffer)
            .map_err(|e| format!("Failed to read stdout: {}", e))?;

        let bytes = bytes::Bytes::from(buffer);

        let output = String::from_utf8_lossy(&bytes);

        Ok(output.into())
    } else {
        Err("Failed to run ddlog app".into())
    }
}

pub fn teardown_ddlog_project(base_dir: &Path, project_name: &str) -> Result<(), String> {
    let project_descriptor = format!(
        "{}/{}.dl",
        base_dir.to_str().unwrap_or_default(),
        project_name
    );
    let project_dir = format!(
        "{}/{}_ddlog",
        base_dir.to_str().unwrap_or_default(),
        project_name
    );
    fs::remove_file(project_descriptor)
        .map_err(|e| format!("Failed to remove project descriptor: {}", e))?;
    fs::remove_dir_all(project_dir).map_err(|e| format!("Failed to remove project dir: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{fs, io::Read};
    use tempfile::tempdir;

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

        let main_rs_path = base_dir.join("src/main.rs");
        assert!(main_rs_path.exists());

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
        let dl_content = r#"
input relation TestRun(x: signed<64>)
output relation DoubledRun(x: signed<64>)
DoubledRun(t) :- TestRun(d),
	var t = d * 2.
"#;

        generate_rust_project(dir.path(), project_name, dl_content);
        let build_result = build_ddlog_crate(dir.path(), project_name);
        assert!(build_result.is_ok());
    }

    #[test]
    fn test_run_ddlog_crate() {
        if which::which("ddlog").is_err() {
            eprintln!("Skipping test_run_ddlog_crate because ddlog is not installed");
            return;
        }

        let dir = tempdir().expect("Failed to create temp dir");
        let project_name = "test_run_project";
        let dl_content = r#"
input relation TestRun(x: signed<64>)
output relation DoubledRun(x: signed<64>)
DoubledRun(t) :- TestRun(d),
	var t = d * 2.
"#;

        generate_rust_project(dir.path(), project_name, dl_content);

        let build_result = build_ddlog_crate(dir.path(), project_name);
        assert!(build_result.is_ok());

        let commands = vec![
            DDLogCommand::Start,
            DDLogCommand::create_fact("TestRun".to_string(), 1, 0, Some("1".to_string())),
            DDLogCommand::CommitDumpChanges,
        ];

        let run_output = run_ddlog_crate(dir.path(), project_name, &commands)
            .expect("Failed to run ddlog crate");

        assert!(
            run_output.contains("DoubledRun{.x = 2}: +1"),
            "Expected message not found in output:\n{}",
            run_output
        );
    }
}
