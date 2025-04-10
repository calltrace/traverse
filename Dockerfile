# Stage 1: Build the application
# Use a specific slim Rust version for smaller image size and reproducibility
FROM rust:1.77-slim-bullseye as builder

# Set the working directory within the container
WORKDIR /usr/src/app

# Install essential build tools (like C compiler for tree-sitter's cc dependency)
# Debian bullseye slim images might not have build-essential by default
RUN apt-get update && apt-get install -y --no-install-recommends build-essential && rm -rf /var/lib/apt/lists/*

# Copy the Cargo manifests and lock file first
# This allows Docker to cache the dependency layer if these files don't change
COPY Cargo.toml Cargo.toml
COPY Cargo.lock Cargo.lock

# Copy workspace member manifests (adjust paths if your structure differs)
# Assuming a standard workspace layout in the 'crates' directory
COPY crates/graph/Cargo.toml ./crates/graph/Cargo.toml
COPY crates/mermaid/Cargo.toml ./crates/mermaid/Cargo.toml
COPY crates/language/Cargo.toml ./crates/language/Cargo.toml
# Add other crates if they exist, e.g.:
# COPY crates/traverse-cli/Cargo.toml ./crates/traverse-cli/Cargo.toml

# Create dummy source files needed to build dependencies only
# This prevents needing the full source code just for dependency resolution/caching
RUN mkdir -p src/bin && echo "fn main() {}" > src/bin/sol2cg.rs
# If you have library crates defined in the workspace, create dummy lib.rs too
RUN mkdir crates/graph/src && echo "// dummy" > crates/graph/src/lib.rs
RUN mkdir crates/mermaid/src && echo "// dummy" > crates/mermaid/src/lib.rs
RUN mkdir crates/language/src && echo "// dummy" > crates/language/src/lib.rs
# Add for other crates as needed

# Build *only* the dependencies to cache them
# This command might fetch and compile dependencies for the whole workspace
RUN cargo build --release --bin sol2cg
# Clean up dummy files and build artifacts related to them to keep the layer clean
RUN rm -rf src crates target/release/.fingerprint target/release/build target/release/deps/sol2cg*

# Copy the entire project source code
# This includes the actual src/bin/sol2cg.rs and all crate sources
COPY . .

# Build the application binary, leveraging the cached dependencies
# Force removal of potentially outdated build script artifacts before the final build
RUN rm -rf target/release/build/
RUN cargo build --release --bin sol2cg

# Stage 2: Create the final runtime image
# Use a minimal base image like Debian Slim for the final stage
FROM debian:bullseye-slim

# Create a non-root user and group for security best practices
RUN groupadd --system --gid 1001 appgroup && \
    useradd --system --uid 1001 --gid 1001 --shell /sbin/nologin appuser

# Copy the compiled binary from the builder stage to the final image
# Place it in a standard location like /usr/local/bin
COPY --from=builder /usr/src/app/target/release/sol2cg /usr/local/bin/sol2cg

# Ensure the binary is executable
RUN chmod +x /usr/local/bin/sol2cg

# Switch to the non-root user
USER appuser

# Set the entrypoint for the container to run the binary
ENTRYPOINT ["/usr/local/bin/sol2cg"]

# Optional: Set default command arguments if the binary expects any
# CMD ["--help"]

