#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "Building Quiver WASM..."

# Build the WASM module
cargo build --target wasm32-unknown-unknown --release -p quiver-web

# Generate bindings
wasm-bindgen \
    --target web \
    --out-dir pkg \
    ../target/wasm32-unknown-unknown/release/quiver_web.wasm
