// spotless is a code formatter

spotless {
    val ie3LicHead = """
/*
 * © $YEAR. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */
"""

    //sets a license header, removes unused imports and formats conforming to the google java format
    java {
        removeUnusedImports() // removes any unused imports
        googleJavaFormat()
        licenseHeader(ie3LicHead)
    }

    /* cf. https://github.com/diffplug/spotless/tree/master/plugin-gradle */
    groovyGradle {
        // same as groovy, but for .gradle (defaults to '*.gradle')
        target("*.gradle", "*.gradle.kts", "gradle/scripts/*.gradle", "gradle/scripts/*.gradle.kts")
        greclipse()
        leadingTabsToSpaces(2)
    }

    //sets a license header, removes unused imports and formats conforming to the scala fmt formatter
    scala {
        scalafmt().configFile(".scalafmt.conf")
        licenseHeader(ie3LicHead, "package.*\\n")
    }

    // removes unnecessary whitespace, indents with tabs and ends on new line for gradle, md and gitignore files and config-XMLs
    format("misc") {
        target("**/.gitignore", "configs/**")
        trimTrailingWhitespace()
        leadingSpacesToTabs()
        endWithNewline()
    }

    /* Formats markdown files, just like the other misc files, but without trimming trailing white spaces (nested
     * enumerations) */
    format("md") {
        target("**/*.md")
        leadingTabsToSpaces(2)
        endWithNewline()
    }
}
