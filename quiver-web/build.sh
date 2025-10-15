#!/bin/bash
set -e

# Change to the script's directory
cd "$(dirname "$0")"

echo "Building Quiver WASM..."

# Build the WASM module (from workspace root)
cargo build --target wasm32-unknown-unknown --release

# Generate bindings
wasm-bindgen \
    --target no-modules \
    --out-dir www/pkg \
    --out-name quiver \
    ../target/wasm32-unknown-unknown/release/quiver_web.wasm
