use std::{
    fs::{self, File}, io::{BufReader, BufWriter, Read, Result as IoResult, Write}, os::fd::AsRawFd, path::Path, process::{Command, Stdio}
};
use tempdir::TempDir;
use tempfile::NamedTempFile;

use crate::facts::DDLogCommand;

pub fn validate(dl_program: &str, enable_tracing: bool) -> Result<String, String> {
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
        .stderr(Stdio::piped()) // Capture stderr for error reporting
        .spawn()
        .map_err(|e| format!("Failed to launch ddlog command: {}", e))?;

    let stderr = ddlog_run.stderr.take().ok_or("Failed to capture stderr")?;

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
        // Read stderr to get error message
        let mut stderr_reader = BufReader::new(stderr);
        let mut error_output = String::new();
        stderr_reader
            .read_to_string(&mut error_output)
            .map_err(|e| format!("Failed to read stderr: {}", e))?;

        // If tracing is enabled, display the DL file with line numbers and highlight the error line
        if enable_tracing {
            // Try to parse the error message to find the line number
            if let Some(error_line) = parse_ddlog_error(&error_output, "program.dl") {
                // Display the DL program with line numbers and highlight the error line
                println!(
                    "
=== DDLog Program with Error Highlighting ==="
                );
                println!(
                    "{}",
                    format_dl_file_with_line_numbers(dl_program, error_line)
                );
                println!(
                    "=== End of DDLog Program ===
"
                );
            }

            // Print the original error message
            eprintln!("DDLog Validation Error: {}", error_output);
        }

        let exit_code = status.code().unwrap_or(-1);
        Err(format!(
            "DDlog validation failed with exit code: {}
{}",
            exit_code, error_output
        ))
    }
}

pub fn generate_rust_project(base_dir: &Path, project_name: &str, dl_content: &str) {
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

pub fn build_ddlog_crate(
    base_dir: &Path,
    project_name: &str,
    enable_tracing: bool,
) -> Result<(), String> {
    if which::which("ddlog").is_err() {
        return Err("`ddlog` binary not found on PATH".into());
    }

    let dl_file = format!("{}.dl", project_name);
    let dl_file_path = base_dir.join(&dl_file);

    // Capture stderr to parse error messages
    let mut ddlog_process = Command::new("ddlog")
        .args(["-i", &dl_file, "-L", "../../lib"])
        .current_dir(base_dir)
        .stdout(Stdio::inherit())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to launch ddlog command: {}", e))?;

    let stderr = ddlog_process
        .stderr
        .take()
        .ok_or("Failed to capture stderr")?;
    let status = ddlog_process
        .wait()
        .map_err(|e| format!("Failed to wait for ddlog process: {}", e))?;

    if !status.success() {
        // Read stderr to get error message
        let mut stderr_reader = BufReader::new(stderr);
        let mut error_output = String::new();
        stderr_reader
            .read_to_string(&mut error_output)
            .map_err(|e| format!("Failed to read stderr: {}", e))?;

        // If tracing is enabled, display the DL file with line numbers and highlight the error line
        if enable_tracing {
            // Try to parse the error message to find the line number
            if let Some(error_line) = parse_ddlog_error(&error_output, &dl_file) {
                // Read the DL file
                if let Ok(dl_content) = fs::read_to_string(&dl_file_path) {
                    println!(
                        "
=== DDLog File with Error Highlighting ==="
                    );
                    println!(
                        "{}",
                        format_dl_file_with_line_numbers(&dl_content, error_line)
                    );
                    println!(
                        "=== End of DDLog File ===
"
                    );
                }
            }

            // Print the original error message
            eprintln!("DDLog Error: {}", error_output);
        }

        return Err(format!(
            "ddlog command failed on {:?}: {}",
            dl_file, error_output
        ));
    }

    let project_dir = format!(
        "{}/{}_ddlog",
        base_dir.to_str().unwrap_or_default(),
        project_name
    );

    // Capture stderr for cargo build as well
    let mut cargo_process = Command::new("cargo")
        .args(["+1.76", "build"])
        .env("RUSTFLAGS", "-A warnings")
        .current_dir(&project_dir)
        .stdout(Stdio::inherit())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to launch cargo build: {}", e))?;

    let cargo_stderr = cargo_process
        .stderr
        .take()
        .ok_or("Failed to capture stderr")?;
    let cargo_status = cargo_process
        .wait()
        .map_err(|e| format!("Failed to wait for cargo process: {}", e))?;

    if !cargo_status.success() {
        let mut stderr_reader = BufReader::new(cargo_stderr);
        let mut error_output = String::new();
        stderr_reader
            .read_to_string(&mut error_output)
            .map_err(|e| format!("Failed to read stderr: {}", e))?;

        if enable_tracing {
            eprintln!("Cargo Build Error: {}", error_output);
        }

        return Err(format!("Cargo build failed: {}", error_output));
    }

    Ok(())
}

pub fn run_ddlog_crate(
    base_dir: &Path,
    project_name: &str,
    cmds: &[DDLogCommand],
    enable_tracing: bool,
) -> Result<String, String> {
    let project_dir = format!(
        "{}/{}_ddlog",
        base_dir.to_str().unwrap_or_default(),
        project_name
    );

    // Pre-compute all commands as a single string for faster writing
    let dat_content = cmds
        .iter()
        .map(|cmd| cmd.to_string())
        .collect::<Vec<String>>()
        .join(
            "
",
        );

    let exec_path = format!("target/debug/{}_cli", project_name);
    if enable_tracing {
        println!(
            "Running generated DDLog application: {}/{}",
            project_dir, exec_path
        );
    }

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

    // Print just first 80 lines of the facts if tracing is enabled
    if enable_tracing {
        println!(
            "Facts:
{}",
            dat_content.lines().take(80).collect::<Vec<&str>>().join(
                "
"
            )
        );
    }

    // Use BufWriter for more efficient writing to stdin
    if let Some(mut stdin) = ddlog_app_run.stdin.take() {
        // Use a large buffer for better performance
        println!("Writing facts to stdin...");
        // Write all data at once
        stdin
            .write_all(dat_content.as_bytes())
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;
        println!("Done writing facts to stdin.");

        // Explicitly drop the writer to close stdin
        drop(stdin);
        println!("Stdin closed.");
    } else {
        return Err("Failed to get stdin handle".into());
    }

    println!("Waiting for the generated DDLog application to finish...");

    // Wait for the process to complete and capture output
    match ddlog_app_run.wait() {
        Ok(_) => {
            let mut stdout = ddlog_app_run.stdout.take().ok_or("Failed to get stdout")?;

            // Use a pre-allocated buffer with reasonable size
            let mut buffer = Vec::with_capacity(1024 * 1024); // 1MB initial capacity
            stdout
                .read_to_end(&mut buffer)
                .map_err(|e| format!("Failed to read stdout: {}", e))?;

            let output = String::from_utf8_lossy(&buffer);
            Ok(output.into())
        }
        Err(e) => Err(format!("Failed to run ddlog app: {}", e)),
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

/// Parse a DDLog error message to extract the line number
fn parse_ddlog_error(error_message: &str, dl_file: &str) -> Option<usize> {
    // Pattern to match DDLog error format: program.dl:218.157-218.172
    // or path/to/file.dl:218.157-218.172
    let file_basename = Path::new(dl_file).file_name()?.to_str()?;

    // Look for the line number in the error message
    for line in error_message.lines() {
        if line.contains(file_basename) {
            // Extract line number using regex
            let re = regex::Regex::new(r"(\w+\.dl):(\d+)\.(\d+)-(\d+)\.(\d+)").ok()?;
            if let Some(captures) = re.captures(line) {
                if let Some(line_str) = captures.get(2) {
                    return line_str.as_str().parse::<usize>().ok();
                }
            }
        }
    }

    None
}

/// Format a DL file with line numbers and highlight the error line
fn format_dl_file_with_line_numbers(content: &str, error_line: usize) -> String {
    let lines = content.lines().collect::<Vec<_>>();
    let max_line_num_width = lines.len().to_string().len();
    let mut formatted_lines = Vec::new();

    for (i, line) in lines.iter().enumerate() {
        let line_num = (i + 1).to_string();
        let padded_line_num = format!("{:>width$}", line_num, width = max_line_num_width);

        if i + 1 == error_line {
            // Highlight the error line
            formatted_lines.push(format!("{} | >>> {} <<<", padded_line_num, line));
        } else {
            formatted_lines.push(format!("{} | {}", padded_line_num, line));
        }
    }

    formatted_lines.join(
        "
",
    )
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
        let build_result = build_ddlog_crate(dir.path(), project_name, false);
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

        let build_result = build_ddlog_crate(dir.path(), project_name, false);
        assert!(build_result.is_ok());

        let commands = vec![
            DDLogCommand::Start,
            DDLogCommand::create_fact("TestRun".to_string(), 1, 0, Some("1".to_string())),
            DDLogCommand::CommitDumpChanges,
        ];

        let run_output = run_ddlog_crate(dir.path(), project_name, &commands, false)
            .expect("Failed to run ddlog crate");

        assert!(
            run_output.contains("DoubledRun{.x = 2}: +1"),
            "Expected message not found in output:
{}",
            run_output
        );
    }
}

