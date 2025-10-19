#!/bin/bash
set -e
cd "$(dirname "$0")"

wasm-pack build --target bundler --out-dir pkg
