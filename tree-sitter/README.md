# tree-sitter-quiver

A [tree-sitter](https://tree-sitter.github.io/) grammar for the Quiver language,
providing syntax highlighting, code folding and structural selection in editors that
embed tree-sitter (Neovim, Helix, Zed, Emacs, GitHub, …).

It lives in the main Quiver repository (rather than a standalone `tree-sitter-quiver`
repo) so that grammar changes land in the same commit as the syntax changes they track,
and so CI can guard the two against drifting apart. It can be split out later with
`git subtree split` if independent publishing is ever wanted.

## Layout

| Path | Purpose |
| --- | --- |
| `grammar.js` | The grammar definition (the source of truth). |
| `src/` | The generated parser (`parser.c` etc.) — **committed** so consumers build without the CLI. |
| `queries/highlights.scm` | Syntax-highlighting captures. |
| `queries/indents.scm` | Indent rules (Helix/Aether `@indent`/`@outdent` dialect). |
| `test/corpus/` | `tree-sitter test` cases pinning the parse tree for representative programs. |
| `scripts/check-corpus.sh` | Parses every real `.qv` file in the repo; fails on any syntax error. |

## Developing

```sh
npm install                 # install the tree-sitter CLI (dev dependency)
npx tree-sitter generate    # regenerate src/ from grammar.js (commit the result)
npx tree-sitter test        # run the corpus tests in test/corpus/
./scripts/check-corpus.sh   # parse std/*.qv and examples/*.qv, failing on errors
```

After any change to `grammar.js`, run `tree-sitter generate` and commit the updated
`src/`. CI verifies that `src/` is in sync, that the corpus tests pass, and that every
`.qv` source file in the repository still parses.

## Relationship to the canonical parser

The canonical Quiver parser is the `nom` parser in
[`quiver-compiler/src/parser.rs`](../quiver-compiler/src/parser.rs); this grammar is a
parallel implementation for editor tooling. It aims to accept every valid Quiver program
(verified against all `.qv` files and the programs embedded in the test suite). For
performance and error-tolerance it is intentionally a little more permissive than the
canonical parser in a few places where a stricter rule would not change highlighting —
for example it does not enforce that hexadecimal binaries have an even number of digits.

### Newlines

Quiver separates top-level statements with newlines (or `;`), and a newline also stops
juxtaposition application (`f x`) from spanning lines. The grammar therefore treats
newlines as significant tokens (spaces, tabs and comments remain `extras`) and permits
them explicitly at continuation points — after `~>`, `,`, `|`, `=>`, `=`, and inside
brackets. This keeps the whole grammar in `grammar.js` with no C external scanner.
