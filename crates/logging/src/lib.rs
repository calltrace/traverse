use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Initialize the tracing subscriber with configurable verbosity
///
/// # Arguments
/// * `verbose` - If true, sets default log level to debug; otherwise info
///
pub fn init_subscriber(verbose: bool) {
    let env_filter = if std::env::var("RUST_LOG").is_ok() {
        EnvFilter::from_default_env()
    } else {
        let default_level = if verbose { "debug" } else { "info" };
        EnvFilter::new(default_level)
    };
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