// tasks for semantic versioning using semver-gradle https://github.com/ethauvin/semver-gradle

tasks.register("currentVersion") {
  doFirst {
    println(project.extra["semver.semver"])
  }
}

tasks.register("devVersion") {
  doFirst {
    val major = project.extra["semver.major"]
    val minor = project.extra["semver.minor"]
    println("$major.$minor-SNAPSHOT")
  }
}
