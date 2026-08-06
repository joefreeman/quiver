# Changelog

## [Unreleased]

## [0.4.0] - 2026-08-05

- Calling is now explicit: a bare callable is always called, wherever it appears — in a chain, a tuple field, a call argument — and `&` references one without calling it (`%list.map [xs, &double]`). Application is written with a space (`%num.add [3, 4]`) rather than glued brackets.
- Tail calls — and so recursion — are now written `^`, and pins `&` (the two swapped roles); a `;` or newline separates steps where a comma did.
- Types take a leading apostrophe (`'int`, `'point`); aliases are defined with `=` (`'point = Point[x: 'int, y: 'int]`), may be declared in any scope, and refer to themselves recursively with `^` (`'list<'t> = Nil | Cons['t, ^]`).
- Every step of a sequence starts from the same block input rather than the previous step's result, and a step that yields nil short-circuits the rest: a chain pipes, a sequence restarts and can fail. This, with branching blocks, is the language's control flow.
- A match evaluates to `Ok`/nil rather than the matched value, so `=P` doubles as a guard. Matching gained alternation (`=(A | B)`), type tests (`='int`), ascribed bindings (`=('int)n`), pins over access paths (`=&p.x`, `=&$x`), and star and partial patterns (`* = p`, `Point(x)`); a tuple pattern's name must now correspond exactly to the value's.
- Every process now has observable state — `?p` samples the argument its root function was most recently entered with, compile-checked from the spawn site — and declared process types grant capabilities explicitly: `@'msg -> 'r ?'state` spells what may be sent, awaited and sampled.
- Processes own the processes they spawn: when one ends, its subtree is torn down with it. `%proc` (`detach`, `kill`, `link`, `track`) adjusts lifetimes, and `%sup` supervises with restart strategies and dynamic membership.
- Select takes a tuple of sources racing awaits, receives, timeouts and streams (`![sock, &control, 5000]`); a block written directly after a select filters candidate messages, leaving rejected ones in the mailbox.
- Added effect-based I/O — files, the filesystem, TCP, DNS — where failure is a value: a failed operation answers nil carrying `IoError[kind, message]` under `:error`, propagating by ordinary short-circuiting. Resources (`\File`, `\TcpSocket`, …) have a single owning process, move when sent, and close when their owner ends; sockets, listeners and byte streams are fallible select sources with pull-based backpressure.
- Added dialects: `%mod{ … }` hands the braced text to a module at compile time and splices in the code it returns. `%list{ 1, 2, 3 }`, `%dict{ "a" => 1 }`, `%json{ … }` and the HTML dialects all work this way — ordinary modules built from `%parse` combinators, returning the `%meta` IR.
- Added live views: `%html{ … }` markup with a node tree and renderer, and `%html/live{ … }` layering typed `on:*` bindings (click, keyboard and mouse events, with per-binding options like `on:input[300]` debounce) and navigation on top. A view is a process; the server re-renders on events and patches the browser over a websocket.
- Added an HTTP stack: `%http/server` (HTTPS via a `tls:` option), `%http/client` for `http://` and `https://`, signed-cookie `%http/session`, and `%http/websocket`. `%tls` upgrades a connected socket in place — everything built over sockets runs over TLS unchanged — and `%pem` decodes certificate PEM, so `serve` takes Let's Encrypt output directly.
- Added annotations: typed key/value metadata attached to tuples and functions (`:doc "…"`), read with a glued `:key`, and invisible to matching and equality. On top of them: `:pre`/`:post` contracts enforced in debug builds, and failure provenance — a debug build stamps each fresh nil with the site that produced it.
- Added step assertions: `//=> P` asserts a step's value against a pattern, checked in debug builds, making the `//=> result` documentation convention executable — and `quiv test <file.md>` runs the Quiver code embedded in a Markdown document and checks its assertions, so a document's examples are tested by running it.
- Added a numeric tower of arbitrary-precision integers, exact rationals and surds; `%num` operations answer nil instead of erroring (e.g. divide by zero).
- Function parameters gained omittable field labels (`[(x): 'int]` — the argument may state the label or stay positional), per-field defaults (`mode: 'mode = R`), labelled arguments in any order, and inferred types for `#{ … }` literals in call arguments; `$$` reaches the enclosing function's parameter from a nested literal. The standard library adopts labels throughout.
- Tuple literals gained field punning — `(x, y)` builds `[x: 1, y: 2]` from variables, referencing callables rather than calling them — and spreads take any access path as their source (`$conn[..., buf: 0x]`, `[...a.inner, y: 3]`).
- Types gained intersections (`'readable & 'writable`), explicit application (`f<'int>`), and spreads (`Post[...'entity, title: '%str]`).
- Added string interpolation (`"hello {name}"`) and multi-line strings.
- Added `%data`, the codec for Quiver's data notation: `encode` renders any data value as text, and `decode<'t>` parses it driven by the expected type, so decoding can never produce a shape the program does not already contain.
- Added `%ref` (unique opaque identifiers), `%vec` (packed numeric vectors), `%dict` maps keyed by arbitrary data values, and much else: the standard library now spans thirty-five modules (see the guide).
- Modules are now ordinary files whose value is computed at compile time (deterministic and identity-free; a program's own top level runs at boot, with the full runtime), routed by a `quiver.toml` manifest, with types reachable as `'%mod`. Compiled modules are cached as content-addressed artifacts, so unchanged imports — the standard library included — link near-instantly.
- The runtime is substantially faster and leaner: binaries own their bytes (building a list of 16,000 strings went from effectively never finishing to 0.13s, and cross-worker sends no longer copy payloads), module references and repeated constructions are hoisted and shared (~9x fewer allocations in library-heavy loops), and completed processes are garbage-collected.
- Added tooling: a tree-sitter grammar, an LSP server, a `quiv format` formatter, and a REPL that shows documentation and types, survives Ctrl-C, and imports modules near-instantly.
- `%time` and `%random` now work in the browser, backed by the host's clock and entropy.
- Removed the equality and `not` operators; matching covers both.

## [0.3.0] - 2025-10-31

### Added

- Added support for matching against types with the pin operator (`... ~> ^int`, `... ~> ^(A[int] | B[bin])`).
- Added support for referencing processes by ID in the REPL with `@123`.

### Changed

- The value of a match expression (`... ~> =x` or `x = ...`) now returns the value itself if the match is successful (and nil otherwise).
- A send expression (`... ~> p`) now returns the process.
- Added support for multiple REPL instances in an environment.
- Various updates to the web API.
- Made 'int' and 'bin' reserved names, and updated the compiler to store variables and type aliases in a single bindings map.
- Updated syntax for defining types to use single colon (e.g., `t : int | bin`).
- Some 'math' standard library functions (e.g., division) now return nil (`[]`) instead of causing runtime errors.
- Updated the select operator to separate sources by commas (e.g., `!(p1, f, 1000)`).
- Added support for shorthand for spawning processes (`@int { ... }`) and defining receive functions (`!int`).
- Improved detail and formatting of parser errors.

### Fixed

- Fixed pin matching with partial (`x = 1, A[x: 1] ~> ^A(x)`).

## [0.2.1] - 2025-10-25

### Fixed

- Fixed using current time in WASM build.
- Fixed typing of web REPL interface.
- Fixed race condition when message is received whilst spawning.

## [0.2.0] - 2025-10-25

### Added

- Added hexadecimal (`0x...`) and binary (`0b...`) integer literal notation.
- Added support for partial type definitions.
- Added parameterised types for type aliases and functions (e.g., `list<t>`, `#<t>t -> t`).

### Changed

- Replaced tuple update syntax with spread operator (`...`) for more flexible/intuitive merging/updating of tuples for both values and types.
- Replaced `type` keyword with `::` syntax (e.g., `point :: Point[x: int, y: int]`).
- Changed syntax for calling built-ins to `__add__`.
- Replaced receive/await operators with a more general 'select' operation (`!(...)`) for awaiting multiple processes, receiving messages, and supporting timeouts.
- Generalised support for using the ripple operator outside of tuple creation.

### Fixed

- Function type compatibility now supports variance (covariant results, contravariant parameters).

## [0.1.0] - 2025-10-20

### Added

- Initial release.

[unreleased]: https://github.com/joefreeman/quiver/compare/v0.4.0...HEAD
[0.4.0]: https://github.com/joefreeman/quiver/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/joefreeman/quiver/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/joefreeman/quiver/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/joefreeman/quiver/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/joefreeman/quiver/releases/tag/v0.1.0
