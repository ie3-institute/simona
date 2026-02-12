tasks.named<Test>("test") {
  useJUnitPlatform {
    includeEngines("scalatest")
  }

  testLogging {
    events("skipped", "failed")
    exceptionFormat = org.gradle.api.tasks.testing.logging.TestExceptionFormat.FULL
  }
}

// test task performance improvements, see -> https://docs.gradle.org/current/userguide/performance.html
tasks.withType<Test> {
  // No parallel execution in order to avoid problems with insufficient computation resources/timeouts.
  // Sequential execution might be even faster, as some limited tests have shown.
  maxParallelForks = 1
  forkEvery = 100
}

tasks.withType<JavaCompile> {
  options.isFork = true
  options.isIncremental = true
}

tasks.withType<ScalaCompile> {
  options.isFork = true
  options.isIncremental = true
}
