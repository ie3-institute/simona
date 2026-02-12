// tasks for semantic versioning using semver-gradle https://github.com/ethauvin/semver-gradle

import net.thauvin.erik.gradle.semver.SemverExtension

tasks.register("currentVersion") {
  doFirst {
    val semver = project.extensions.getByType(SemverExtension::class.java)
    println(semver.semver)
  }
}

tasks.register("devVersion") {
  doFirst {
    val semver = project.extensions.getByType(SemverExtension::class.java)
    println("${semver.major}.${semver.minor}-SNAPSHOT")
  }
}
