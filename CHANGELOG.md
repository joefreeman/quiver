# Changelog

## [Unreleased]

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

[unreleased]: https://github.com/joefreeman/quiver/compare/v0.2.1...HEAD
[0.2.1]: https://github.com/joefreeman/quiver/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/joefreeman/quiver/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/joefreeman/quiver/releases/tag/v0.1.0
