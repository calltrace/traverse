#!/bin/bash

# Build Docker images for Traverse CLI tools
# This script builds individual Docker images for each CLI tool

set -e  # Exit on error

echo "Building Docker images for Traverse CLI tools..."
echo "================================================"

# Color codes for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Base image name
BASE_NAME="traverse"

# Array of tools to build
TOOLS=(
    "sol2cg"
    "sol2test"
    "sol2bnd"
    "storage-trace"
    "sol-storage-analyzer"
    "all"
)

# Build each image
for tool in "${TOOLS[@]}"; do
    echo -e "${BLUE}Building ${tool}...${NC}"
    docker build --target "${tool}" -t "${BASE_NAME}:${tool}" .
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Successfully built ${BASE_NAME}:${tool}${NC}"
    else
        echo "✗ Failed to build ${tool}"
        exit 1
    fi
    echo ""
done

# Tag the 'all' image as 'latest' for backward compatibility
echo -e "${BLUE}Tagging ${BASE_NAME}:all as ${BASE_NAME}:latest...${NC}"
docker tag "${BASE_NAME}:all" "${BASE_NAME}:latest"

echo ""
echo "================================================"
echo -e "${GREEN}All images built successfully!${NC}"
echo ""
echo "Available images:"
docker images | grep "^${BASE_NAME}" | awk '{printf "  - %s:%s (%s)\n", $1, $2, $7}'
echo ""
echo "Usage examples:"
echo "  docker run -v \$(pwd):/work ${BASE_NAME}:sol2cg /work/contract.sol"
echo "  docker run -v \$(pwd):/work ${BASE_NAME}:sol2test /work/contract.sol"
echo "  docker run ${BASE_NAME}:sol2bnd --help"
echo "  docker run ${BASE_NAME}:storage-trace --help"
echo "  docker run ${BASE_NAME}:sol-storage-analyzer --help"