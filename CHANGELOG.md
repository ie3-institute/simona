# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Add safety factor sRated calculation [#629](https://github.com/ie3-institute/simona/issues/629)
- Re-implemented ResultEventListener in akka typed [#343](https://github.com/ie3-institute/simona/issues/343)
- Add additional test cases from references for PvModelTest [#590](https://github.com/ie3-institute/simona/issues/590)
- Instantiation of Heat Pump Agents [#253](https://github.com/ie3-institute/simona/issues/253)
- Output of accompanying thermal result models
- Added JDK installation, Scala Plugin + SDK in usersguide [#324](https://github.com/ie3-institute/simona/issues/324)
- Squants scalatest matchers [#715](https://github.com/ie3-institute/simona/issues/715)
- Energy Management capabilities:
  - Added capability of SystemParticipants to handle flexibility [#308](https://github.com/ie3-institute/simona/issues/308)
  - Added smart charging logic [#31](https://github.com/ie3-institute/simona/issues/31) and flex calculation in `EvcsAgent` [#332](https://github.com/ie3-institute/simona/issues/332)
- Enhance output quotes of `RunSimona` [#743](https://github.com/ie3-institute/simona/issues/743)
- Printing logs of failed tests [#747](https://github.com/ie3-institute/simona/issues/747)
- Models for measurements within the grid structure [#89](https://github.com/ie3-institute/simona/issues/89)
- Config possibility for transformer control groups [#90](https://github.com/ie3-institute/simona/issues/90)
- Implemented scaling of all relevant input parameters [#764](https://github.com/ie3-institute/simona/issues/764)
- Consider scaling factor with flex options [#734](https://github.com/ie3-institute/simona/issues/734)
- Implementation of Energy Management Agents [#204](https://github.com/ie3-institute/simona/issues/204)

### Changed
- Adapted to changed data source in PSDM [#435](https://github.com/ie3-institute/simona/issues/435)
- Improved initialization of weather based agents [#145](https://github.com/ie3-institute/simona/issues/145)
- Changed from ComparableQuantity to squants fast and typesafe calculations [#490](https://github.com/ie3-institute/simona/issues/490)
- Changed from ComparableQuantity to squants in power flow [#554](https://github.com/ie3-institute/simona/issues/554)
- Reduce log level on missing diffuse irradiance [#629](https://github.com/ie3-institute/simona/issues/629)
- Updated to gradle 8.4 [#648](https://github.com/ie3-institute/simona/issues/648)
- Introducing new scheduling infrastructure:
  - Two-parted scheduler in akka typed [#378](https://github.com/ie3-institute/simona/issues/378)
  - Adapting to simonaAPI 0.3.0 (adapted message protocol)
  - Schedule lock [#651](https://github.com/ie3-institute/simona/issues/651)
  - New scheduling protocol [#650](https://github.com/ie3-institute/simona/issues/650)
  - Small improvements to the code [#696](https://github.com/ie3-institute/simona/issues/696)
- Replaced akka with pekko [#641](https://github.com/ie3-institute/simona/issues/641)
- Use `ThermalGrid` to calculate thermal environment of a heat pump [#315](https://github.com/ie3-institute/simona/issues/315)
- Enable windows path as config parameters [#549](https://github.com/ie3-institute/simona/issues/549)
- Unified consideration of scaling factor when simulating system participants [#81](https://github.com/ie3-institute/simona/issues/81)
- Small improvements in `ResultEventListener` [#738](https://github.com/ie3-institute/simona/issues/738)
- Converting `SimonaSim` to pekko typed/terminating SimonSim when initialization fails [#210](https://github.com/ie3-institute/simona/issues/210)
- Converting the `GridAgent` and the `DBFSAlgorithm` to `pekko typed` [#666](https://github.com/ie3-institute/simona/issues/666)
- Validation of grid will throw exception instead of just logging errors [#463](https://github.com/ie3-institute/simona/issues/463)

### Fixed
- Removed a repeated line in the documentation of vn_simona config [#658](https://github.com/ie3-institute/simona/issues/658)
- Removed version number "2.0" from the logo printed to console [#642](https://github.com/ie3-institute/simona/issues/642)
- Fixed PV Model documentation [#684](https://github.com/ie3-institute/simona/issues/684), [#686](https://github.com/ie3-institute/simona/issues/686)
- Removed `CsvDataSourceAdapter` workaround [#702](https://github.com/ie3-institute/simona/issues/702)
- Logging wrong duration in the first simulation hour [#705](https://github.com/ie3-institute/simona/issues/705)
- Fixed some compiler warnings [#657](https://github.com/ie3-institute/simona/issues/657)
- Fixing false negative in ref system voltage validation [#706](https://github.com/ie3-institute/simona/issues/706)
- Fixing randomly failing test in `RuntimeEventListenerSpec` etc. [#709](https://github.com/ie3-institute/simona/issues/709)

## [3.0.0] - 2023-08-07

### Added
- Implement SQL source for primary data [#34](https://github.com/ie3-institute/simona/issues/34), [#101](https://github.com/ie3-institute/simona/issues/101)
- Relevant scientific papers have been added to the documentation [#139](https://github.com/ie3-institute/simona/issues/139)
- Add troubleshooting section to Users guide [#160](https://github.com/ie3-institute/simona/issues/160)
- Added Kafka sink for results [#24](https://github.com/ie3-institute/simona/issues/24)
- Added Kafka sink for runtime events, re-implemented RuntimeEventListener in akka typed [#242](https://github.com/ie3-institute/simona/issues/242)
- Added listeners to DBFS tests to check the result output and check the handling of failed power flows [#269](https://github.com/ie3-institute/simona/issues/269)
- Added DBFS test with participant load and added testing for FinishGridSimulationTrigger [#281](https://github.com/ie3-institute/simona/issues/281)
- Added Dependabot updates to sphinx/readthedocs dependencies [#448](https://github.com/ie3-institute/simona/issues/448)
- Check for grid validity with PSDM `ValidationUtils` [#460](https://github.com/ie3-institute/simona/issues/460)
- Enhancing dev's guide [#363](https://github.com/ie3-institute/simona/issues/363)
- Support PF calculation with closed switches [#474](https://github.com/ie3-institute/simona/issues/474)

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
- Adapted to changes of Quantity units in PSU and PSDM [#419](https://github.com/ie3-institute/simona/pull/419)
- Adapted entry in Done message and deleted parallel window [#159](https://github.com/ie3-institute/simona/issues/159)
- Added ConfigFailFast check for invalid dateTime configuration [#344](https://github.com/ie3-institute/simona/issues/344)
- Changed simulation duration format [#429](https://github.com/ie3-institute/simona/issues/429)
- Updated sphinx dependencies and fixed sphinx warnings [#444](https://github.com/ie3-institute/simona/issues/444)
- Updated authors in AUTHORS.md and README.md [#452](https://github.com/ie3-institute/simona/issues/452)
- Updating `CONTRIBUTING.md` [#201](https://github.com/ie3-institute/simona/issues/201)
- Speeding up additionalActivationTicks in participant's BaseStateData [#421](https://github.com/ie3-institute/simona/pull/421)
- Changed format of example grid `vn_simona` [#216](https://github.com/ie3-institute/simona/issues/216)
- Renamed ChpData to ChpRelevantData [#494](https://github.com/ie3-institute/simona/issues/494)
- Updated gradle to 8.2.1, cleaned up `build.gradle` and `Jenkinsfile` [#572](https://github.com/ie3-institute/simona/issues/572)

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
- Removed grid config for vn_146_lv_small [#290](https://github.com/ie3-institute/simona/issues/290)
- Adapted to changes of EvcsInput in PSDM [#377](https://github.com/ie3-institute/simona/pull/377)
- Fix breaking SIMONA caused by changes in simonaAPI [#384] (https://github.com/ie3-institute/simona/issues/384)
- Fixed awaiting departed EVs in ExtEvDataService [#392](https://github.com/ie3-institute/simona/issues/392)
- Fixed missing ModelBaseStateData generation for random load profiles [#399](https://github.com/ie3-institute/simona/issues/399)
- Fixed non-random first days of random load profiles [#401](https://github.com/ie3-institute/simona/issues/401)
- Fixed groovy formatting [#110](https://github.com/ie3-institute/simona/issues/110)
- Fixed configuration reference in user's guide [#224](https://github.com/ie3-institute/simona/issues/224)
- Fixed ResultEventListener exiting too early with high volumes of results [#350](https://github.com/ie3-institute/simona/issues/350)
- Fixed tests that unreliably fail [#359](https://github.com/ie3-institute/simona/issues/359)
- Support for three winding transformers  [#63](https://github.com/ie3-institute/simona/issues/63)
  - Handle incoming slack voltage accordingly
  - Allow multiple sub grid gates at one node (also allows multiple two winding transformers at one node)
  - Perform power flow calculation in highest grid, if a three winding transformer is apparent
  - Write out results
- Fixed broken layout in RTD documentation [#500](https://github.com/ie3-institute/simona/issues/500)
- Corrected tests in RefSystemTest [#560](https://github.com/ie3-institute/simona/issues/560)
- Take log file event filters from `logback.xml` when defining the run log appender [#108](https://github.com/ie3-institute/simona/issues/108)
- Fix rendering of references in documentation [#505](https://github.com/ie3-institute/simona/issues/505)

### Removed
- Remove workaround for tscfg tmp directory [#178](https://github.com/ie3-institute/simona/issues/178)
- Removed Gradle task puml2png (Converting Plantuml to png / svg files) since it is no longer needed  [#228](https://github.com/ie3-institute/simona/issues/228)
- Remove RocketChat notification from Jenkinsfile [#234](https://github.com/ie3-institute/simona/issues/234)
- Removed one-jar gradle plugin [#564](https://github.com/ie3-institute/simona/issues/564)

[Unreleased]: https://github.com/ie3-institute/simona/compare/3.0.0...HEAD
[3.0.0]: https://github.com/ie3-institute/simona/compare/a14a093239f58fca9b2b974712686b33e5e5f939...3.0.0
