# Changelog

## [Unreleased]

### Added

- Hexadecimal (`0x...`) and binary (`0b...`) integer literal notation.
- Support for partial type definitions.

### Changed

- Replaced tuple update syntax with spread operator (`...`) for more flexible/intuitive merging/updating of tuples.
- Replaced `type` keyword with `::` syntax (e.g., `point :: Point[x: int, y: int]`).

### Fixed

- Function type compatibility now supports variance (covariant results, contravariant parameters).

## [0.1.0] - 2025-10-20

### Added

- Initial release.

[unreleased]: https://github.com/quiverlang/quiver/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/quiverlang/quiver/releases/tag/v0.1.0
