// Enforces the correct Java version, as some parts of the project may malfunction under a wrong version
// If this task fails, try changing your JAVA_HOME to the required version
tasks.register("checkJavaVersion") {
  group = "Verification"
  description = "Enforces correct Java version"

  doLast {
    val foundVersion = JavaVersion.current()
    val requiredVersion = project.extra["javaVersion"] as JavaVersion
    if (foundVersion != requiredVersion) {
      throw IllegalStateException("Wrong Java version: required is " +
        requiredVersion + ", but found " + foundVersion)
    }
  }
}
tasks.named("compileJava") {
  dependsOn("checkJavaVersion")
}
