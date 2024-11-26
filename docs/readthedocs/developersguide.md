(developersguide)=

# Developer’s Guide

The SIMONA repository can be found [on GitHub](https://github.com/ie3-institute/simona).

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
    - The code needs to be properly formatted (`gradle spotlessApply`)
    - The code needs to compile
    - All tests need to succeed
  - [SonarQube](https://simona.ie3.e-technik.tu-dortmund.de/sonar/dashboard?id=edu.ie3%3Asimona) run succeeds, i.e. 
    - No new code smells are found 
    - Code coverage is sufficient
  - If the project uses readthedocs documentation, the sphinx compilation needs to succeed
  - Other code checks such as Codacy and sonatype (exceptions can be made for some types of warnings)
- Manual audits
  - Changes made to the code have to be reflected within all types of documentation, i.e.
    - Readthedocs for long form documentation
    - ScalaDoc/JavaDoc for interface documentation
    - Code commentary on crucial parts of the code
  - All vital parts of the new code need to be covered by tests (see {ref}`developersguide:tests`)
  - Code quality should be sufficient (see {ref}`developersguide:coding conventions`)

There can be exceptions to these rules, which have to be approved by the reviewer.

### Tests

We have good experiences using [ScalaTest](https://www.scalatest.org/) and [Spock](http://spockframework.org/) as testing frameworks.
Please do not only try to achieve a high _line coverage_, but also aim at covering as many [_branches_](https://en.wikipedia.org/wiki/Code_coverage) as possible.
In order to execute _all_ available tests, execute `gradle test` or comment `!test` on your PR, which automatically queues a run in our CI.

When practical, we like to use [test driven development (TDD)](https://www.guru99.com/test-driven-development.html):
It can pay off to write a failing test first for new functionality to be implemented, then implement and alter the actual functionality until the tests pass.

### Coding conventions

The existing code base is not perfect. Still, we strive to continually improve code quality and in turn lower the entry barrier to working with SIMONA.
Generally, when making changes to the code, we try to follow the _scouts' rule_: We leave the campsite tidier than we found it.

Before marking PRs as ready to review, please make sure to deliver code that follows these basic guidelines:
- There should be no unreachable or commented out code.
- Variables and methods should have meaningful names.
- The code should follow a clear structure.
- Instead of using very specialized or shortened syntax, rather stick with well-known or easy to grasp forms of code.
- Imports should be optimized in all changed classes (most of all, there should be no unused imports).
- Avoid usage of `null` in Scala and Java.
- Separate classes that hold data from classes that perform functionality.

Furthermore, there are some functional programming paradigms that we like to follow:
- If possible, use [immutable objects](https://en.wikipedia.org/wiki/Immutable_object).
- Write pure methods, avoid [side effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science)). Sending a message (which happens asynchronously) can be considered a side effect too and should be separated from code that is dealing with logic.
- Use higher-order functions on data structures such as [`map`](https://en.wikipedia.org/wiki/Map_(higher-order_function)), `fold`, `flatten` etc. instead of loops.

These guidelines do not intend to be exhaustive. Feel free to extend them with rules that are yet missing.

## Protocols

```{toctree}
---
maxdepth: 1
---
protocols
```
