#!/bin/bash

# Script to publish Traverse crates to crates.io in dependency order
# Usage: ./scripts/publish-crates.sh [--dry-run] [crate-name]
# Examples:
#   ./scripts/publish-crates.sh --dry-run           # Dry-run all crates
#   ./scripts/publish-crates.sh --dry-run traverse-logging  # Dry-run single crate
#   ./scripts/publish-crates.sh                     # Publish all crates

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo_step() {
    echo -e "${GREEN}[PUBLISH]${NC} $1"
}

echo_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

echo_warn() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

DRY_RUN=""
if [ "$1" == "--dry-run" ]; then
    DRY_RUN="--dry-run"
    echo "Running in dry-run mode..."
    echo ""
    echo_warn "Note: During dry-run, dependencies between traverse crates may fail"
    echo_warn "because they haven't been published to crates.io yet."
    echo_warn "This is expected. The actual publish will work correctly."
    echo ""
fi

# Verify we're in the right directory
if [ ! -f "Cargo.toml" ] || [ ! -d "crates" ]; then
    echo_error "Please run this script from the root of the Traverse repository"
    exit 1
fi

# Crates in dependency order
CRATES=(
    "traverse-logging"
    "traverse-solidity"
    "traverse-mermaid"
    "traverse-graph"
    "traverse-codegen"
    "traverse-cli"
)

# Function to publish a crate
publish_crate() {
    local crate=$1
    local crate_dir=""
    
    # Map crate name to directory
    case $crate in
        "traverse-logging")  crate_dir="crates/logging" ;;
        "traverse-solidity") crate_dir="crates/solidity" ;;
        "traverse-mermaid")  crate_dir="crates/mermaid" ;;
        "traverse-graph")    crate_dir="crates/graph" ;;
        "traverse-codegen")  crate_dir="crates/codegen" ;;
        "traverse-cli")      crate_dir="crates/cli" ;;
        *)
            echo_error "Unknown crate: $crate"
            exit 1
            ;;
    esac
    
    echo_step "Publishing $crate from $crate_dir..."
    
    # Publish the crate
    cd "$crate_dir"
    if cargo publish $DRY_RUN --allow-dirty; then
        echo_step "Successfully published $crate"
    else
        echo_error "Failed to publish $crate"
        exit 1
    fi
    cd ../..
    
    # Wait a bit between publishes to ensure crates.io has indexed the previous crate
    if [ -z "$DRY_RUN" ] && [ "$crate" != "traverse-cli" ]; then
        echo_step "Waiting 30 seconds for crates.io to index $crate..."
        sleep 30
    fi
}

# Check if specific crate was requested
SINGLE_CRATE=""
for arg in "$@"; do
    if [[ "$arg" != "--dry-run" ]]; then
        SINGLE_CRATE="$arg"
    fi
done

# Main publishing loop
if [ -n "$SINGLE_CRATE" ]; then
    echo_step "Publishing single crate: $SINGLE_CRATE"
    publish_crate "$SINGLE_CRATE"
else
    echo_step "Starting Traverse crates publishing..."
    echo_step "Publishing order: ${CRATES[*]}"
    
    for crate in "${CRATES[@]}"; do
        publish_crate "$crate"
    done
fi

echo_step "All crates published successfully!"

if [ -z "$DRY_RUN" ]; then
    echo_step "Users can now install Traverse tools with:"
    echo "  cargo install traverse-cli"
fi