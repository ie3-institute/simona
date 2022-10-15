# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Implement SQL source for primary data [#34](https://github.com/ie3-institute/simona/issues/34), [#101](https://github.com/ie3-institute/simona/issues/101)
- Relevant scientific papers have been added to the documentation [#139](https://github.com/ie3-institute/simona/issues/139)
- Add troubleshooting section to Users guide [#160](https://github.com/ie3-institute/simona/issues/160)
- Added Kafka sink for results [#24](https://github.com/ie3-institute/simona/issues/24)
- Added Kafka sink for runtime events, re-implemented RuntimeEventListener in akka typed [#242](https://github.com/ie3-institute/simona/issues/242)
- Added listeners to DBFS tests to check the result output and check the handling of failed power flows [#269](https://github.com/ie3-institute/simona/issues/269)
- Instantiation of Heat Pump Agents [#253](https://github.com/ie3-institute/simona/issues/253)
- Output of accompanying thermal result models

### Changed
- Re-organizing test resources into their respective packages [#105](https://github.com/ie3-institute/simona/issues/105)
- BREAKING: Using snapshot version of PSDM and PSU
- Simplified PrimaryServiceProxy due to changes in PSDM [#120](https://github.com/ie3-institute/simona/issues/120)
- Improved handling of weights and their sum in determination of weather data [#173](https://github.com/ie3-institute/simona/issues/173)
- Improving code readability in EvcsAgent by moving FreeLotsRequest to separate methods [#19](https://github.com/ie3-institute/simona/issues/19)
- Ignore dependabot snapshot dependencies [#27](https://github.com/ie3-institute/simona/issues/27)
- Sending termination message to external simulation on expected and unexpected shutdowns of SIMONA [#35](https://github.com/ie3-institute/simona/issues/35)
- Change transformer calculation since changes in PSDM [#99](https://github.com/ie3-institute/simona/issues/99)
- Adapt to changed PvInputModel of PSDM (elevationAngle) [#100](https://github.com/ie3-institute/simona/issues/100)
- Consolidate csv parameterization in config [#149](https://github.com/ie3-institute/simona/issues/149)
- Change weather scheme to COSMO [PR#154](https://github.com/ie3-institute/simona/pull/154)
- Adapt documentation to changed simonaAPI [#191](https://github.com/ie3-institute/simona/issues/191)
- Implementing a new plugin framework for external simulations [#195](https://github.com/ie3-institute/simona/issues/195)
- Improved implementation of `RefSystemParser` [#212](https://github.com/ie3-institute/simona/issues/212)
- Include missing images into Documentation [#151](https://github.com/ie3-institute/simona/issues/151)
- Changing the export methode for diagrams [#156](https://github.com/ie3-institute/simona/issues/156)
- Change references implementation in Documentation to bibtex [#174](https://github.com/ie3-institute/simona/issues/174) 
- Update Model descriptions (Documentation) [#122](https://github.com/ie3-institute/simona/issues/122)
- Changes of Getting Started Section (Documentation) [#124](https://github.com/ie3-institute/simona/issues/124) 
- Update gradle [#176](https://github.com/ie3-institute/simona/issues/176)
- Setting java version to 17 [#58](https://github.com/ie3-institute/simona/issues/58)
- Made SimonaConfig.BaseRuntimeConfig serializable [#36](https://github.com/ie3-institute/simona/issues/36)
- Adapt to new simonaAPI snapshot [#95](https://github.com/ie3-institute/simona/issues/95)
- Update Sphinx to 4.5.0 as well as extensions [#214](https://github.com/ie3-institute/simona/issues/214)
- Improved code quality in and around DBFS algorithm [#265](https://github.com/ie3-institute/simona/issues/265)
- Adapt test to new PowerSystemUtils snapshot  [#294](https://github.com/ie3-institute/simona/issues/294)
- Simplified ParticipantConfigUtil [#273](https://github.com/ie3-institute/simona/issues/273)
- Consolidated and enhanced SimScheduler tests [#285](https://github.com/ie3-institute/simona/issues/285)
- Renaming sub-package directories [#141](https://github.com/ie3-institute/simona/issues/141)
- Updated authors in AUTHORS.md [#301](https://github.com/ie3-institute/simona/issues/301)
- Added faster data structures to SimScheduler [#282](https://github.com/ie3-institute/simona/issues/282)
- Adaption of abbreviations in PVModel and adjacent classes to naming convention [#326](https://github.com/ie3-institute/simona/issues/326)
- Fixed Latex equations [#264](https://github.com/ie3-institute/simona/issues/264)
- Documentation of the simulation configuration [#334](https://github.com/ie3-institute/simona/issues/334)
- Use `ThermalGrid` to calculate thermal environment of a heat pump [#315](https://github.com/ie3-institute/simona/issues/315)

### Fixed
- Location of `vn_simona` test grid (was partially in Berlin and Dortmund) [#72](https://github.com/ie3-institute/simona/issues/72)
- Let `ParticipantAgent` die after failed registration with secondary services (prevents stuck simulation) [#76](https://github.com/ie3-institute/simona/issues/76)
- Fix default resolution of weather source wrapper [#78](https://github.com/ie3-institute/simona/issues/78)
- Fix invalid thread allocation in GridAgent [#111](https://github.com/ie3-institute/simona/issues/111)
- Fixed config auto-generation [#130](https://github.com/ie3-institute/simona/issues/130)
- Fixed genConfigSample gradle task[#148](https://github.com/ie3-institute/simona/issues/148)
- Fixed some unreachable code [#167](https://github.com/ie3-institute/simona/issues/167)
- Fix treatment of non-InitializeTrigger triggers in initialization within SimScheduler [#237](https://github.com/ie3-institute/simona/issues/237)
- Fix breaking SIMONA caused by introducing temperature dependant load profiles in PSDM [#255](https://github.com/ie3-institute/simona/issues/255)
- Respect for voltage angle in DBFS slack voltage exchange protocol [#69](https://github.com/ie3-institute/simona/issues/69)
- Adapted to changed time series interfaces in PSDM [#296](https://github.com/ie3-institute/simona/issues/296)
- Fix handling of multiple connections between subgrids [#22](https://github.com/ie3-institute/simona/issues/22)
  - Consolidate request replies for different sub grid gates in one message
  - Await and send responses for distinct pairs of sender reference and target node
- Removed deprecations from `CsvGridSource` and added `TestGridFactory` [#304](https://github.com/ie3-institute/simona/issues/304)
- Fixed config of vn_146_lv_small [#290](https://github.com/ie3-institute/simona/issues/290)

### Removed
- Remove workaround for tscfg tmp directory [#178](https://github.com/ie3-institute/simona/issues/178)
- Removed Gradle task puml2png (Converting Plantuml to png / svg files) since it is no longer needed  [#228](https://github.com/ie3-institute/simona/issues/228)
- Remove RocketChat notification from Jenkinsfile [#234](https://github.com/ie3-institute/simona/issues/234)

[Unreleased]: https://github.com/ie3-institute/simona/compare/a14a093239f58fca9b2b974712686b33e5e5f939...HEAD
