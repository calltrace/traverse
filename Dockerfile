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
COPY crates/cli/Cargo.toml ./crates/cli/Cargo.toml
COPY crates/codegen/Cargo.toml ./crates/codegen/Cargo.toml
COPY crates/graph/Cargo.toml ./crates/graph/Cargo.toml
COPY crates/language/Cargo.toml ./crates/language/Cargo.toml
COPY crates/logging/Cargo.toml ./crates/logging/Cargo.toml
COPY crates/mermaid/Cargo.toml ./crates/mermaid/Cargo.toml
COPY crates/solidity/Cargo.toml ./crates/solidity/Cargo.toml
# Create dummy source files needed to build dependencies only
# This prevents needing the full source code just for dependency resolution/caching
# Create dummy lib.rs files for all workspace crates
RUN mkdir -p crates/cli/src/bin && echo "fn main() {}" > crates/cli/src/bin/sol2cg.rs
RUN mkdir -p crates/codegen/src && echo "// dummy" > crates/codegen/src/lib.rs
RUN mkdir -p crates/graph/src && echo "// dummy" > crates/graph/src/lib.rs
RUN mkdir -p crates/language/src && echo "// dummy" > crates/language/src/lib.rs
RUN mkdir -p crates/logging/src && echo "// dummy" > crates/logging/src/lib.rs
RUN mkdir -p crates/mermaid/src && echo "// dummy" > crates/mermaid/src/lib.rs
RUN mkdir -p crates/solidity/src && echo "// dummy" > crates/solidity/src/lib.rs
# Add for other crates as needed

# Build *only* the dependencies to cache them
# This command might fetch and compile dependencies for the whole workspace
RUN cargo build --release --bin sol2cg
# Clean up dummy files and build artifacts related to them to keep the layer clean
RUN rm -rf src crates target/release/.fingerprint target/release/build target/release/deps/sol2cg* target/release/deps/storage-trace*

# Copy the entire project source code
# This includes the actual src/bin/sol2cg.rs and all crate sources
COPY . .

# Build the application binaries, leveraging the cached dependencies
# Force removal of potentially outdated build script artifacts before the final build
RUN rm -rf target/release/build/
RUN cargo build --release --bin sol2cg
RUN cargo build --release --bin sol2test
RUN cargo build --release --bin sol2bnd
RUN cargo build --release --bin storage-trace
RUN cargo build --release --bin sol-storage-analyzer

# Stage 2: Individual runtime images for each tool

# === sol2cg image ===
FROM debian:bullseye-slim as sol2cg
RUN groupadd --system --gid 1001 appgroup && \
    useradd --system --uid 1001 --gid 1001 --shell /sbin/nologin appuser
COPY --from=builder /usr/src/app/target/release/sol2cg /usr/local/bin/sol2cg
RUN chmod +x /usr/local/bin/sol2cg
USER appuser
ENTRYPOINT ["/usr/local/bin/sol2cg"]

# === sol2test image ===
FROM debian:bullseye-slim as sol2test
RUN groupadd --system --gid 1001 appgroup && \
    useradd --system --uid 1001 --gid 1001 --shell /sbin/nologin appuser
COPY --from=builder /usr/src/app/target/release/sol2test /usr/local/bin/sol2test
RUN chmod +x /usr/local/bin/sol2test
USER appuser
ENTRYPOINT ["/usr/local/bin/sol2test"]

# === sol2bnd image ===
FROM debian:bullseye-slim as sol2bnd
RUN groupadd --system --gid 1001 appgroup && \
    useradd --system --uid 1001 --gid 1001 --shell /sbin/nologin appuser
COPY --from=builder /usr/src/app/target/release/sol2bnd /usr/local/bin/sol2bnd
RUN chmod +x /usr/local/bin/sol2bnd
USER appuser
ENTRYPOINT ["/usr/local/bin/sol2bnd"]

# === storage-trace image ===
FROM debian:bullseye-slim as storage-trace
RUN groupadd --system --gid 1001 appgroup && \
    useradd --system --uid 1001 --gid 1001 --shell /sbin/nologin appuser
COPY --from=builder /usr/src/app/target/release/storage-trace /usr/local/bin/storage-trace
RUN chmod +x /usr/local/bin/storage-trace
USER appuser
ENTRYPOINT ["/usr/local/bin/storage-trace"]

# === sol-storage-analyzer image ===
FROM debian:bullseye-slim as sol-storage-analyzer
RUN groupadd --system --gid 1001 appgroup && \
    useradd --system --uid 1001 --gid 1001 --shell /sbin/nologin appuser
COPY --from=builder /usr/src/app/target/release/sol-storage-analyzer /usr/local/bin/sol-storage-analyzer
RUN chmod +x /usr/local/bin/sol-storage-analyzer
USER appuser
ENTRYPOINT ["/usr/local/bin/sol-storage-analyzer"]

# === all-in-one image (backward compatibility) ===
FROM debian:bullseye-slim as all
RUN groupadd --system --gid 1001 appgroup && \
    useradd --system --uid 1001 --gid 1001 --shell /sbin/nologin appuser
COPY --from=builder /usr/src/app/target/release/sol2cg /usr/local/bin/sol2cg
COPY --from=builder /usr/src/app/target/release/sol2test /usr/local/bin/sol2test
COPY --from=builder /usr/src/app/target/release/sol2bnd /usr/local/bin/sol2bnd
COPY --from=builder /usr/src/app/target/release/storage-trace /usr/local/bin/storage-trace
COPY --from=builder /usr/src/app/target/release/sol-storage-analyzer /usr/local/bin/sol-storage-analyzer
RUN chmod +x /usr/local/bin/sol2cg /usr/local/bin/sol2test /usr/local/bin/sol2bnd /usr/local/bin/storage-trace /usr/local/bin/sol-storage-analyzer
USER appuser
# Default to sol2cg for backward compatibility
ENTRYPOINT ["/usr/local/bin/sol2cg"]

