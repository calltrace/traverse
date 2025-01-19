use std::{
    fs::{self, File},
    io::{Read, Result as IoResult, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};
use tempdir::TempDir;

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
        .args([
            "-i",
            &dl_file.to_string_lossy(),
            "--action=validate",
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|e| format!("Failed to launch ddlog command: {}", e))?;

    if ddlog_run.wait().is_ok() {
        let mut stdout = ddlog_run.stdout.take().ok_or("Failed to get stdout")?;

        // read stdout
        let mut buffer = Vec::new();
        stdout
            .read_to_end(&mut buffer)
            .map_err(|e| format!("Failed to read stdout: {}", e))?;

        let bytes = bytes::Bytes::from(buffer);
        Ok(String::from_utf8_lossy(&bytes).into())

   } else {
        // read stderr 
        let mut stderr = ddlog_run.stderr.take().ok_or("Failed to get stderr")?;
        let mut buffer = Vec::new();
        stderr
            .read_to_end(&mut buffer)
            .map_err(|e| format!("Failed to read stderr: {}", e))?;
        let bytes = bytes::Bytes::from(buffer);
        let stderr_dump: String = String::from_utf8_lossy(&bytes).into();
 
        Err(stderr_dump)
    }

}
