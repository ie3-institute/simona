# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Fixed
- Fix power exchange between grids
  - Consolidate request replies for different sub grid gates in one message
  - Await and send responses for distinct pairs of sender reference and target node

[Unreleased]: https://github.com/ie3-institute/simona
