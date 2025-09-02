use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Initialize the tracing subscriber with configurable verbosity
///
/// # Arguments
/// * `verbose` - If true, sets default log level to debug; otherwise info
///
/// This function respects the RUST_LOG environment variable if set,
/// otherwise uses the default based on the verbose flag.
pub fn init_subscriber(verbose: bool) {
    // Check if RUST_LOG is already set
    let env_filter = if std::env::var("RUST_LOG").is_ok() {
        // Use the environment variable if it's set
        EnvFilter::from_default_env()
    } else {
        // Set default based on verbose flag
        let default_level = if verbose { "debug" } else { "info" };
        EnvFilter::new(default_level)
    };

    // Initialize the subscriber
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(env_filter)
        .init();
}

/// Initialize the tracing subscriber with a custom filter
///
/// # Arguments
/// * `filter` - A custom filter string (e.g., "info", "debug", "trace")
pub fn init_subscriber_with_filter(filter: &str) {
    let env_filter = EnvFilter::new(filter);
    
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(env_filter)
        .init();
}