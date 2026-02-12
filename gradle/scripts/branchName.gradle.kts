tasks.register("checkBranchName") {
  doLast {
    if (!project.hasProperty("branchName")) {
      throw GradleException("Error: Missing required property 'branchName'.")
    }

    val branchName = project.property("branchName") as String

    val patterns = listOf(
      Regex("^(developer|develop|dev)$"),
      Regex(".*rel/.*"),
      Regex("^dependabot/.*$"),
      Regex(".*hotfix/\\pL{2}/#\\d+.*"),
      Regex(".*main"),
      Regex("^[a-z]{2}/#[0-9]+(?:-.+)?$")
    )

    val isValid = patterns.any { pattern -> branchName.matches(pattern) }

    if (!isValid) {
      throw GradleException("Error: Check Branch name format (e.g., ps/#1337-FeatureName). Current branch name is $branchName.")
    }

    println("Branch name is $branchName")
  }
}
