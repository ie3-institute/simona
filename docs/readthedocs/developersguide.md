(developersguide)=

# Developer’s Guide

The SIMONA repository can be found [on Github](https://github.com/ie3-institute/simona).

```{contents}
---
local:
```

## Contributing code
If you intend to produce some lines of code, pick an issue and get some hands on! For any questions feel
free to contact us (see [`README.md`](https://github.com/ie3-institute/simona/blob/dev/README.md) for contact information).

### Branching and handing in pull requests
We try to follow a branch naming strategy of the form `<initials>/#<issueId>-<description>`.
If for example [Prof. Dr. rer. hort. Klaus-Dieter Brokkoli](https://www.instagram.com/prof_broccoli/) would like to add some work on node models reported in issue #4711, he would open a branch `kb/#4711-extendingNodeModels`.
Before opening it for review, please make sure the automated checks have succeeded and the code quality is up to our standard.

## Finalising your pull request

Before a PR can be merged, some automated tests and a manual review have to be completed successfully.
There can be one or more reviewers per PR, although at least one is required, and one or multiple review rounds.
We also encourage you to create pull requests early in your development cycle which gives others an opportunity to observe and/or provide feedback in real time. 
When you are ready for a review, invite one or more reviewers through the pull request.
In short, mergeable PRs have to meet our standards in several areas:
- Automated checks
  - [Jenkins](https://simona.ie3.e-technik.tu-dortmund.de/ci/job/ie3-institute/job/simona/) run succeeds, i.e. 
    - the code needs to be properly formatted (`gradle spotlessApply`)
    - the code needs to compile
    - all tests need to succeed
  - [SonarQube](https://simona.ie3.e-technik.tu-dortmund.de/sonar/dashboard?id=edu.ie3%3Asimona) run succeeds, i.e. 
    - no new code smells are found 
    - code coverage is sufficient
  - If the project uses readthedocs documentation, the sphinx compilation needs to succeed
  - Other code checks such as Codacy and sonatype (exceptions can be made for some types of warnings)
- Manual audits
  - Changes made to the code have to be reflected within all types of documentation, i.e.
    - readthedocs for long form documentation
    - ScalaDoc/JavaDoc for interface documentation
    - code commentary on crucial parts of the code
  - Code quality should be sufficient, i.e.

There can be exceptions to these rules, which have to be approved by the reviewer.

### Tests

Ensure the proper function of your code by [test driven development (TDD)](https://www.guru99.com/test-driven-development.html).
We have good experiences using [ScalaTest](https://www.scalatest.org/) and [Spock](http://spockframework.org/) as testing frameworks.

Run `gradle test` or comment `!test` in your PR, which automatically queues a run in our CI.

### Coding conventions

Before marking PRs as ready to review, please make sure to deliver code that follows these basic guidelines:
- imports should be optimized in all changed classes
- there should be no unreachable or commented out code
- variables and methods should have meaningful names
- if possible, use [immutable objects](https://en.wikipedia.org/wiki/Immutable_object).
- the code should follow a clear structure

## Protocols

```{toctree}
---
maxdepth: 1
---
protocols
```
