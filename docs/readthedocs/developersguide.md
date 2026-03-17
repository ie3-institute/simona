(developersguide)=

# Developer’s Guide

The SIMONA repository can be found [on GitHub](https://github.com/ie3-institute/simona).

```{contents}
---
local:
```

## Contributing code

If you intend to produce some lines of code, pick an issue and get some hands on!
For any questions feel free to contact us (see [`README.md`](https://github.com/ie3-institute/simona/blob/dev/README.md) for contact information).

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
  - GitHub Actions run succeeds, i.e. 
    - The code needs to be properly formatted (`gradle spotlessApply`)
    - The code needs to compile
    - All tests need to succeed
  - [SonarQube](https://simona.ie3.e-technik.tu-dortmund.de/sonar/dashboard?id=edu.ie3%3Asimona) run succeeds, i.e. 
    - No new code smells are found 
    - Code coverage is sufficient
  - If the project uses ReadTheDocs documentation, the sphinx compilation needs to succeed
  - Other code checks such as Codacy and Sonatype (exceptions can be made for some types of warnings)
- Manual audits
  - The PR is linked to some issue documenting the bug, task, or enhancement.
  - Changes made to the code have to be reflected within all types of documentation, i.e.
    - ReadTheDocs for long form documentation
    - ScalaDoc/JavaDoc for interface documentation
    - Code commentary on crucial parts of the code
  - All vital parts of the new code need to be covered by tests (see [](#tests))
  - Code quality should be sufficient (see [](#coding-conventions))

There can be exceptions to these rules, which have to be approved by the reviewer.

### Tests

We have good experiences using [ScalaTest](https://www.scalatest.org/) ([Spock](http://spockframework.org/) in other projects) as testing frameworks.
Please do not only try to achieve a high _line coverage_, but test where it makes sense.
For crucial parts, aim at covering as many [_branches_](https://en.wikipedia.org/wiki/Code_coverage) as possible.
In order to execute _all_ available tests, execute `gradle test`.

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

## API compatibility policy

We follow a simple, strict policy for public API (interfaces) and method lifecycle to keep releases predictable for downstream users and integrations:

- Major releases (x -> x+1) include big changes: new architecture, redesigned APIs, etc.
- Minor releases (x.y -> x.y+1) include additive changes: new features or extensions.
- Patch versions (x.y.z -> x.y.z+1). These may only contain bug fixes and non-API behavioral fixes, but no breaking changes.

- No method deprecations: We avoid introducing deprecations. Instead, if an API must be changed, the change is scheduled for a minor or major release where breaking changes are allowed. This allows a simple maintainance without supporting a long deprecation period and keeps the codebase cleaner.

## Reviewing and merging pull requests
Reviewers should check that the PR meets the criteria outlined in the "Finalising your pull request" section above, including automated checks and manual review. If the PR is ready to merge, reviewers can approve it and merge it into the dev branch. If there are open issues that need to be addressed, reviewers should provide clear feedback to the contributor and request changes before approval.
 
### Reviewer checklist
- Does the PR add `@deprecated` or similar deprecation notes? If yes: ask for rationale and explicit approval; prefer scheduling the change for a minor/major release instead.
- Is the new code is properly documented and / or are there any necessary follow-up tasks necessary that are excluded from the PR for reason. These should be tracked at least by creating a new issue for them.


## Protocols

```{toctree}
---
maxdepth: 1
---
protocols
```

## Release Process

We're following the git-flow approach to release new versions. The following steps are necessary to went through for a release:

### Release Checklist
- Does the release change any public types, method signatures, or class visibility? If yes: ensure the release will be minor/major release.
- For patch branches: confirm the version bump is a patch, and check that only bug fixes / internal refactors are present.

### Pre-Release
1. Update gradle to latest version: `./gradlew wrapper --gradle-version=<version> --distribution-type=bin`
2. Create a new *issue* with the new release version (e.g. Release 2.1.0)
3. Milestone and link PRs to it:
   1. Create a new Milestone for this release version if it doesn't exist already
   2. Search all PRs that are newer than the last release. This query might be helpful `is:closed is:pr no:milestone -author:app/dependabot`
   3. Add all of these PRs to the Milestone
4. Create a new branch based from MAIN:
   1. Name it `rel/YOUR_INITIALS/#ISSUE_NUMBER-release_RELEASE_VERSION where
      - YOUR_INITIALS would be `kb` for [Prof. Dr. rer. hort. Klaus-Dieter Brokkoli](https://www.instagram.com/prof_broccoli/)
      - ISSUE_NUMBER is the number of the issue created above
      - RELEASE_Version would be e.g. 2.1.0
   2. Merge dev-branch into the rel-Branch
5. Update the version number:
   1. Update the `version.properties` to new version number
   2. Update `CITATION.cff`
      - Adapt new version number
      - Adapt release date
   3. Update the version number in readthedocs
      - Adapt the version number in the `conf.py` file
      - Adapt the version number in `Getting Started`.
6. Adapt the *changelog*:
   1. Change headline from `Unreleased` into the new version (e.g. 2.1.0)
   2. Add a new `Unreleased` section
   3. Adapt the from-till dates of the versions in the bottom part of the changelog
7. Push the PR
   - Keep in mind that you would like to merge into main, not dev.
8. Get a Reviewer who Merge your release PR

### Release on Maven Central
9. After your Branch has been merged, one of the Repository-Admins needs to confirm the release within MavenCentral.
   1. Visit [Maven Central Publishing](https://central.sonatype.com/publishing/deployments). If everything worked fine, the new release should be available under 'Deployments'. Else, check the CI-Pipeline for any errors.
   2. Finally check the Deployment:
      - Are all necessary files there?
      - Is the deployment valid?
   3. If so, publish. Else, choose 'drop'.
10. Final steps at GitHub
   1. Create a new Tags and create the Release also there
      1. Hint: IntelliJ -> Git -> Select 'Main-Branch' -> Choose commit
      2. Push Tags to GitHub
   2. Create a new release with the new tag version and the change description (Copy from changelog and adapt accordingly if necessary)
   3. Increment MinorVersion of dev branch
      - Adapt `version.properties` by using gradle task `./gradlew incrementMinor`
      - Use Force Push to overrule branch protection
   4. Merge back the main branch into dev
