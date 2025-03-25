use std::env;
use std::fs;
use std::process::Command;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() > 1 && args[1] == "cleanall" {
        let status = Command::new("cargo")
            .arg("clean")
            .status()
            .expect("Failed to execute cargo clean");

        if !status.success() {
            eprintln!("cargo clean failed");
            std::process::exit(1);
        }

        clean_test_project();
    } else {
        clean_test_project();
    }
}

fn clean_test_project() {
    let current_dir = env::current_dir().expect("Failed to get current directory");
    let test_project_dir = current_dir.join("test_project").clone();

    if test_project_dir.exists() {
        println!("Cleaning test_project directory: {:?}", test_project_dir);
        match fs::remove_dir_all(&test_project_dir) {
            Ok(_) => println!("Successfully removed test_project directory"),
            Err(e) => eprintln!("Failed to remove test_project directory: {}", e),
        }
    }
}
