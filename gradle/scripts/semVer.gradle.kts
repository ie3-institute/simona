// tasks for semantic versioning using semver-gradle https://github.com/ethauvin/semver-gradle

tasks.register("currentVersion") {
  doLast {
    val semverMap = project.extra["semver"] as Map<String, Any>
    val version = semverMap["semver"] as String
    println(version)
  }
}

tasks.register("devVersion") {
  doFirst {
    val major = project.extra["semver.major"]
    val minor = project.extra["semver.minor"]
    println("$major.$minor-SNAPSHOT")
  }
}
