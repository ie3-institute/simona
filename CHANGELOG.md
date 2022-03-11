# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Changed
- Improving code readability in EvcsAgent by moving FreeLotsRequest to separate methods
- Re-organizing test resources into their respective packages [#105](https://github.com/ie3-institute/simona/issues/105)
- BREAKING: Using snapshot version of PSDM
- Simplified PrimaryServiceProxy due to changes in PSDM [#120](https://github.com/ie3-institute/simona/issues/120)

### Fixed
- Location of `vn_simona` test grid (was partially in Berlin and Dortmund)
- Let `ParticipantAgent` die after failed registration with secondary services (prevents stuck simulation)
- Support for three winding transformers
  - Handle incoming slack voltage accordingly
  - Allow multiple sub grid gates at one node (also allows multiple two winding transformers at one node)
  - Perform power flow calculation in highest grid, if a three winding transformer is apparent
  - Write out results

[Unreleased]: https://github.com/ie3-institute/simona/compare/a14a093239f58fca9b2b974712686b33e5e5f939...HEAD
