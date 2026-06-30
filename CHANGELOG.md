# Changelog

## [Unreleased]

A large release centred on a syntax overhaul; most 0.3.0 programs need updating.

- Application is now argument-first: a function consumes the value to its left (`5 double`, `[3, 4] num.add`); `~>` is an optional synonym for the space.
- Calling is now explicit: a bare callable is always called, and `&` references one without calling it (`[xs, &double] map`); tail calls and recursion use `^`.
- Types take a leading apostrophe (`'int`, `'point`), and aliases use `=` (`'point = Point[x: 'int, y: 'int]`).
- Expressions now thread the result through each step; a step that yields nil short-circuits the rest.
- A match or binding now evaluates to `Ok`/nil rather than the matched value, so `=...` acts as a guard; matching also gained alternatives, type-ascribed bindings (`(T)x`), and intersections (`'a & 'b`).
- Added a numeric tower of arbitrary-precision integers, rationals, and surds; `num` operations return nil instead of erroring (e.g. divide by zero).
- Added effect-based I/O for files, sockets, and DNS.
- Expanded the standard library (`num`, `int`, `binary`, `string`, `list`, `iter`, `range`, `file`, `path`, `dict`, `ref`).
- Added tooling: a tree-sitter grammar, an LSP server, and a `quiv format` formatter (checked in CI).
- Added multi-line strings and string interpolation (`"hello {name}"`).
- Added module loading via a manifest, inline imports, and per-module default types (`'%mod`).
- Changed process syntax: spawning takes an explicit init argument, and `select` takes a tuple of sources.
- Removed the equality and `not` operators.

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

[unreleased]: https://github.com/joefreeman/quiver/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/joefreeman/quiver/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/joefreeman/quiver/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/joefreeman/quiver/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/joefreeman/quiver/releases/tag/v0.1.0
