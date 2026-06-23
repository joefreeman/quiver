#!/bin/bash
set -e
cd "$(dirname "$0")"

# Build the wasm-bindgen compiler/runtime bindings.
wasm-pack build --target bundler --out-dir pkg

# Bundle the tree-sitter grammar (WASM) and highlight query into the package, so web consumers
# get syntax-highlighting assets from this one dependency — no separate package, no committed
# binaries downstream. The WASM build needs Docker (or a local emscripten).
(cd ../tree-sitter && npx tree-sitter build --wasm --docker -o tree-sitter-quiver.wasm)
cp ../tree-sitter/tree-sitter-quiver.wasm pkg/tree-sitter-quiver.wasm
cp ../tree-sitter/queries/highlights.scm pkg/quiver-highlights.scm

# wasm-pack rewrites pkg/package.json each build with a fixed `files` list; add the grammar
# assets so they're included when published.
node -e '
  const fs = require("node:fs");
  const path = "pkg/package.json";
  const pkg = JSON.parse(fs.readFileSync(path, "utf8"));
  for (const file of ["tree-sitter-quiver.wasm", "quiver-highlights.scm"]) {
    if (!pkg.files.includes(file)) pkg.files.push(file);
  }
  fs.writeFileSync(path, `${JSON.stringify(pkg, null, 2)}\n`);
'
