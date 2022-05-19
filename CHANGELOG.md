# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Implement SQL source for primary data [#34](https://github.com/ie3-institute/simona/issues/34), [#101](https://github.com/ie3-institute/simona/issues/101)

### Changed
- Re-organizing test resources into their respective packages [#105](https://github.com/ie3-institute/simona/issues/105)
- BREAKING: Using snapshot version of PSDM
- Simplified PrimaryServiceProxy due to changes in PSDM [#120](https://github.com/ie3-institute/simona/issues/120)
- Improved handling of weights and their sum in determination of weather data [#173](https://github.com/ie3-institute/simona/issues/173)
- Improving code readability in EvcsAgent by moving FreeLotsRequest to separate methods [#19](https://github.com/ie3-institute/simona/issues/19)
- Sending termination message to external simulation on expected and unexpected shutdowns of SIMONA [#35](https://github.com/ie3-institute/simona/issues/35)
- Improved implementation of `RefSystemParser` [#212](https://github.com/ie3-institute/simona/issues/212)
- Removed Gradle task puml2png (Converting Plantuml to png / svg files) since it is no longer needed  [#230](https://github.com/ie3-institute/simona/issues/230)
- Harmonized configuration of csv parameters [#149](https://github.com/ie3-institute/simona/issues/149)

### Fixed
- Location of `vn_simona` test grid (was partially in Berlin and Dortmund)
- Let `ParticipantAgent` die after failed registration with secondary services (prevents stuck simulation)
- Fix default resolution of weather source wrapper [#78](https://github.com/ie3-institute/simona/issues/78)

[Unreleased]: https://github.com/ie3-institute/simona/compare/a14a093239f58fca9b2b974712686b33e5e5f939...HEAD
