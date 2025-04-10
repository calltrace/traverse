# Stage 1: Build the application
# Use a specific slim Rust version for smaller image size and reproducibility
FROM rust:1.83-slim-bullseye as builder

# Set the working directory within the container
WORKDIR /usr/src/app

# Install essential build tools (like C compiler for tree-sitter's cc dependency)
# Debian bullseye slim images might not have build-essential by default
RUN apt-get update && apt-get install -y --no-install-recommends build-essential && rm -rf /var/lib/apt/lists/*

# Copy the Cargo manifests and lock file first
# This allows Docker to cache the dependency layer if these files don't change
COPY Cargo.toml Cargo.toml
COPY Cargo.lock Cargo.lock

# Copy workspace member manifests
COPY crates/backend/Cargo.toml ./crates/backend/Cargo.toml
COPY crates/compiler/Cargo.toml ./crates/compiler/Cargo.toml
COPY crates/core/Cargo.toml ./crates/core/Cargo.toml
COPY crates/frontend/Cargo.toml ./crates/frontend/Cargo.toml
COPY crates/graph/Cargo.toml ./crates/graph/Cargo.toml
COPY crates/ir/Cargo.toml ./crates/ir/Cargo.toml
COPY crates/language/Cargo.toml ./crates/language/Cargo.toml
COPY crates/mermaid/Cargo.toml ./crates/mermaid/Cargo.toml
COPY crates/sol2seq/Cargo.toml ./crates/sol2seq/Cargo.toml
COPY crates/traverse-cli/Cargo.toml ./crates/traverse-cli/Cargo.toml
COPY crates/tree-sitter-traverse/Cargo.toml ./crates/tree-sitter-traverse/Cargo.toml
COPY crates/mermaid/Cargo.toml ./crates/mermaid/Cargo.toml
# Create dummy source files needed to build dependencies only
# This prevents needing the full source code just for dependency resolution/caching
RUN mkdir -p src/bin && echo "fn main() {}" > src/bin/sol2cg.rs
# Create dummy lib.rs files for all workspace crates
RUN mkdir -p crates/backend/src && echo "// dummy" > crates/backend/src/lib.rs
RUN mkdir -p crates/compiler/src && echo "// dummy" > crates/compiler/src/lib.rs
RUN mkdir -p crates/core/src && echo "// dummy" > crates/core/src/lib.rs
RUN mkdir -p crates/frontend/src && echo "// dummy" > crates/frontend/src/lib.rs
RUN mkdir -p crates/graph/src && echo "// dummy" > crates/graph/src/lib.rs
RUN mkdir -p crates/ir/src && echo "// dummy" > crates/ir/src/lib.rs
RUN mkdir -p crates/language/src && echo "// dummy" > crates/language/src/lib.rs
RUN mkdir -p crates/mermaid/src && echo "// dummy" > crates/mermaid/src/lib.rs
RUN mkdir -p crates/sol2seq/src && echo "// dummy" > crates/sol2seq/src/lib.rs
RUN mkdir -p crates/traverse-cli/src/bin && echo "fn main() {}" > crates/traverse-cli/src/bin/sol2cg.rs
RUN mkdir -p crates/tree-sitter-traverse/bindings/rust && echo "// dummy" > crates/tree-sitter-traverse/bindings/rust/lib.rs
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

