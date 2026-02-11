/* Maven publish - start */

tasks.register("sourcesJar", Jar::class.java) {
    archiveClassifier.set("sources")
    from(sourceSets["main"].allJava)
}

tasks.register("javadocJar", Jar::class.java) {
    dependsOn(tasks.named("javadoc", Javadoc::class.java))
    archiveClassifier.set("javadoc")
    from(tasks.named<Javadoc>("javadoc").get().destinationDir)
}

if (project.hasProperty("user") && project.hasProperty("password") && project.hasProperty("deployVersion")) {
    // snapshot version differs from normal version
    val versionString = project.property("deployVersion") as String

    publishing {
        publications {
            create<MavenPublication>("mavenJava") {
                versionMapping {
                    // resolves dynamic versioning to current version number
                    usage("java-api") {
                        fromResolutionOf("runtimeClasspath")
                    }
                    usage("java-runtime") {
                        fromResolutionResult()
                    }
                }
                pom {
                    description.set("simona - an agent-based power system simulation")
                    name.set("simona")
                    url.set("https://github.com/ie3-institute/simona")
                    organization {
                        name.set("Institute of Energy Systems, Energy Efficiency and Energy Economics (ie3)/TU Dortmund University")
                        url.set("https://www.ie3.tu-dortmund.de/")
                    }
                    issueManagement {
                        system.set("GitHub")
                        url.set("https://github.com/ie3-institute/simona/issues")
                    }
                    licenses {
                        license {
                            name.set("BSD 3-Clause License")
                            url.set("https://github.com/ie3-institute/simona/blob/master/LICENSE")
                        }
                    }
                    developers {
                        developer {
                            organization.set("Institute of Energy Systems, Energy Efficiency and Energy Economics (ie3)/TU Dortmund University")
                            organizationUrl.set("https://ie3.etit.tu-dortmund.de")
                        }
                    }
                    scm {
                        connection.set("scm:git:git://github.com/ie3-institute/simona.git")
                        developerConnection.set("scm:git:ssh://github.com:ie3-institute/simona.git")
                        url.set("https://github.com/ie3-institute/simona")
                    }
                }

                removeTestDependenciesFromPom(pom)
                groupId = group.toString()
                artifactId = "simona"
                version = versionString

                from(components["java"])
                artifact(tasks.named("sourcesJar"))
                artifact(tasks.named("javadocJar"))
            }
        }
        repositories {
            maven {
                val releasesRepoUrl = "https://s01.oss.sonatype.org/service/local/staging/deploy/maven2/"
                val snapshotsRepoUrl = "https://s01.oss.sonatype.org/content/repositories/snapshots/"
                url = if (versionString.endsWith("SNAPSHOT")) snapshotsRepoUrl else releasesRepoUrl
                credentials {
                    username = project.property("user") as String
                    password = project.property("password") as String
                }
            }
        }
        signing {
            useInMemoryPgpKeys(
                findProperty("signingKey") as String,
                findProperty("signingPassword") as String
            )
            sign(publications["mavenJava"])
        }
    }

    tasks.named("generatePomFileForMavenJavaPublication") {
        destination = layout.buildDirectory.file("generated-pom.xml").get().asFile
    }
}

fun removeTestDependenciesFromPom(pom: MavenPom) {
    pom.withXml {
        val root = asNode()
        // eliminate test-scoped dependencies (no need in maven central POMs)
        root.dependencies.removeAll { dep ->
            dep.scope == "test"
        }
    }
}

/* Maven publish - end */
