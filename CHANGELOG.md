# Changelog

## [Unreleased]

### Added

- Hexadecimal (`0x...`) and binary (`0b...`) integer literal notation.
- Support for partial type definitions.
- Parameterised types for type aliases and functions (e.g., `list<t>`, `#<t>t -> t`).

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

[unreleased]: https://github.com/joefreeman/quiver/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/joefreeman/quiver/releases/tag/v0.1.0
