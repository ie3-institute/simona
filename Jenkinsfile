#!groovy

/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

////////////////////////////////
// general config values
////////////////////////////////

/* project configuration */
String javaVersionId = 'jdk-17' // id that matches the java tool with the java version that should be used set as jenkins property

/* git configuration */
String projectName = 'simona' // name of the repository, is case insensitive
String orgName = 'ie3-institute' // name of the github organization
String gitCheckoutUrl = "git@github.com:$orgName/${projectName}.git"
String sshCredentialsId = '19f16959-8a0d-4a60-bd1f-5adb4572b702' // id that matches the ssh credentials to interact with the git set as jenkins property

/* ci configuration */
String sonarqubeProjectKey = 'edu.ie3:simona' // sonarqube project key, case-sensitive

/* maven central configuration */
String mavenCentralCredentialsId = '87bfb2d4-7613-4816-9fe1-70dfd7e6dec2' // id that matches the maven central credentials set as jenkins property
String mavenCentralSignKeyFileId = 'dc96216c-d20a-48ff-98c0-1c7ba096d08d' // id that matches the maven central sign key file set as jenkins property
String mavenCentralSignKeyId = 'a1357827-1516-4fa2-ab8e-72cdea07a692' // id that matches the maven central sign key id set as jenkins property

/**
 * pipeline configuration
 */

/* setup pipeline properties */
// dev and main need manual deploy capabilities
if (env.BRANCH_NAME == "main" || env.BRANCH_NAME == "dev") {
  constantBranchesProps()
} else {
  // all other branches need trigger capabilities for PRs
  temporaryBranchesProps()
}

/**
 * pipeline
 */

node {
  ansiColor('xterm') {
    try {
      // set java version
      setJavaVersion(javaVersionId)

      // determine branch name that should be checked out
      net.sf.json.JSONObject prJsonObj = getPRJsonObj(orgName, projectName, env.CHANGE_ID)
      String currentBranchName = prJsonObj == null ? env.BRANCH_NAME : prJsonObj.head.ref
      String targetBranchName = prJsonObj == null ? null : prJsonObj.base.ref

      /* prs from forks require a special handling*/
      String headGitCheckoutUrl = prJsonObj == null ? gitCheckoutUrl : prJsonObj.head.repo.ssh_url

      // checkout scm
      String commitHash = ""
      stage('checkout') {
        // commit hash from scm checkout
        // https://www.theserverside.com/blog/Coffee-Talk-Java-News-Stories-and-Opinions/Complete-Jenkins-Git-environment-variables-list-for-batch-jobs-and-shell-script-builds
        commitHash = gitCheckout(projectName, headGitCheckoutUrl, currentBranchName, sshCredentialsId).GIT_COMMIT
      }

      // set build display name
      String displayNameBranchName = prFromFork() ? prJsonObj.head.repo.full_name : currentBranchName
      currentBuild.displayName = determineDisplayName(displayNameBranchName, commitHash, orgName, projectName)

      if (currentBranchName == "main") {
        stage('handle dev pr') {
          // normally main pipeline is only triggered by merge of release or hotfixes OR manually triggered
          // if manually triggered for deploy, no PR should be created
          if (params.deploy != "true") {
            // Disabled for now
            // handleDevPr(sshCredentialsId, orgName, projectName, currentBranchName)
          }
        }
      }

      // version check
      stage('version check') {
        // version check can only be executed, if target branch is known (derived from a PR)
        if (targetBranchName == "main" || targetBranchName == "dev") {
          if (checkVersion(currentBranchName, targetBranchName, projectName, projectName, gitCheckoutUrl, headGitCheckoutUrl, sshCredentialsId) != 0)
            error "Version check failed! See log for version differences."
        } else if (targetBranchName == null) {
          // if this branch is the dev branch, we can still do version check to compare if dev and main have the same semnatic version
          if (env.BRANCH_NAME == "dev") {
            if (checkVersion(currentBranchName, "main", projectName, projectName, gitCheckoutUrl, headGitCheckoutUrl, sshCredentialsId) != 0)
              error "Version check failed! See log for version differences."
          } else {
            println "No PR for branch '$currentBranchName' exists. Cannot check versioning! Please create a PR to enable version check."
          }
        } else {
          error "Target branch name '$targetBranchName' for merging is not supported! Please select either 'dev' or 'main' as " +
              "target branch for merging!"
        }
      }

      // Build the project
      stage('build') {
        gradle('clean assemble', projectName)
      }

      // test the project
      stage('run tests') {

        sh 'java -version'

        gradle('--refresh-dependencies spotlessCheck test', projectName)

        sh(script: """set +x && cd $projectName""" + ''' set +x; ./gradlew javadoc''', returnStdout: true)
      }

      // sonarqube analysis & quality gate
      stage('sonarqube') {
        String sonarqubeCurrentBranchName = prFromFork() ? prJsonObj.head.repo.full_name : currentBranchName // forks needs to be handled differently
        String sonarqubeCmd = determineSonarqubeGradleCmd(sonarqubeProjectKey, sonarqubeCurrentBranchName, targetBranchName, orgName, projectName, projectName)
        withSonarQubeEnv() {
          // will pick the global server connection from jenkins for sonarqube
          gradle(sonarqubeCmd, projectName)
        }
        timeout(time: 1, unit: 'HOURS') {
          // Just in case something goes wrong, pipeline will be killed after a timeout
          def qg = waitForQualityGate() // Reuse taskId previously collected by withSonarQubeEnv
          if (qg.status != 'OK') {
            error "Pipeline aborted due to quality gate failure: ${qg.status}"
          }
        }
      }

      // post processing
      stage('post processing') {

        // publish reports
        publishReports(projectName)

      }

    } catch (Exception e) {
      // set build result to failure
      currentBuild.result = 'FAILURE'

      // publish reports even on failure
      publishReports(projectName)

      // print exception
      Date date = new Date()
      println("[ERROR] [${date.format("dd/MM/yyyy")} - ${date.format("HH:mm:ss")}] " + e)
    }

  }
}


/**
 * pipeline stages - all methods require node context
 */

def setJavaVersion(String javaVersionId) {
  env.JAVA_HOME = "${tool javaVersionId}"
  env.PATH = "${env.JAVA_HOME}/bin:${env.PATH}"
}

/* git interaction */

def gitCheckout(String relativeTargetDir, String gitCheckoutUrl, String branch, String sshCredentialsId) {
  checkout([
    $class                           : 'GitSCM',
    branches                         : [[name: branch]],
    doGenerateSubmoduleConfigurations: false,
    extensions                       : [
      [$class: 'RelativeTargetDirectory', relativeTargetDir: relativeTargetDir]
    ],
    submoduleCfg                     : [],
    userRemoteConfigs                : [
      [credentialsId: sshCredentialsId, url: gitCheckoutUrl]
    ]
  ])
}

def handleDevPr(String sshCredentialsId, String orgName, String projectName, String currentBranchName) {
  // get the latest merge string from git to derive latest commit hash + latest merge branch name
  String gitLogLatestMergeString = ""
  withCredentials([
    sshUserPrivateKey(credentialsId: sshCredentialsId, keyFileVariable: 'sshKey')
  ]) {
    // cleanup to prepare repo
    sh(script:
    "set +x && cd $projectName && " +
    "ssh-agent bash -c \"set +x && ssh-add $sshKey; " +
    "git branch | grep -v \"$currentBranchName\" | xargs git branch -D;" + // deletes all local branches except main
    "git fetch && git checkout $currentBranchName && git pull\"", returnStdout: false)

    gitLogLatestMergeString = sh(script: "cd $projectName && set +x && git log --merges -n 1", returnStdout: true)

  }

  // only create pr if the last merge has been a hotfix or a release branch merge
  boolean isHotfix = gitLogLatestMergeString.find("Merge pull request .+ from .*hotfix/\\pL{2}/#\\d+.*") ? true : false
  boolean isRelease = gitLogLatestMergeString.find("Merge pull request .+ from .*rel/\\pL{2}/#\\d+.*") ? true : false

  if (isHotfix || isRelease) {

    // try to get hotfix first, if this fails try to get release
    String latestMergeBranchName = ""
    if (isHotfix) {
      String hotfixRegex = "hotfix/\\pL{2}/#\\d+.*"
      latestMergeBranchName = (gitLogLatestMergeString.find(hotfixRegex).trim() =~ hotfixRegex)[0]
    } else {
      String relRegex = "rel/\\pL{2}/#\\d+.*"
      latestMergeBranchName = (gitLogLatestMergeString.find(relRegex).trim() =~ relRegex)[0]
    }

    // get the latest merge commit sha
    String latestMergeCommitSHA = gitLogLatestMergeString.find("Merge: .* .*\n").trim().split(" ")[2]

    // create new branch with same name as before + hand in a pull request for dev branch,
    // if the branch already exists catch the exception because then we can just go on for a PR
    try {
      withCredentials([
        sshUserPrivateKey(credentialsId: sshCredentialsId, keyFileVariable: 'sshKey')
      ]) {
        sh(script: "set +x && cd $projectName && " +
        "ssh-agent bash -c \"set +x && ssh-add $sshKey; " +
        "git fetch && git checkout $currentBranchName && git pull && " +
        "git checkout -b $latestMergeBranchName $latestMergeCommitSHA && " +
        "git push --set-upstream origin $latestMergeBranchName\"")

      }
    } catch (Exception e) {
      println "No need to create a new branch. Can reuse old one."
    }

    // create PR on dev branch
    withCredentials([
      string(credentialsId: 'SimServCIDeveloperAccessTokenForWebhooks', variable: 'SimServCIToken')
    ]) {
      String issueId = (latestMergeBranchName =~ ".*(#\\d+).*")[0][1]
      String prMessage = "CI generated PR for branch `$latestMergeBranchName` after merging it into `main`.\\n" +
          "Please review, adapt and merge it into `dev`.\\nResolves $issueId."
      String curlCmd = "set +x && " +
          "curl -s -X POST -u johanneshiry:$SimServCIToken -H \"Accept: application/vnd.github.v3+json\"" +
          " https://api.github.com/repos/$orgName/$projectName/pulls" +
          " -d '{ \"title\": \"$latestMergeBranchName for dev\", \"body\": \"$prMessage\", \"head\": \"$latestMergeBranchName\", \"base\": \"dev\"," +
          "\"draft\":\"true\"}'"
      sh(script: curlCmd, returnStdout: true)
    }

    // switch back to main branch for further processing
    sh(script: "set +x && cd $projectName && git checkout $currentBranchName")
  }
}

def createAndPushTagOnMain(String projectName, String sshCredentialsId) {
  String tagBranchName = 'main'

  String projectVersion =
      sh(returnStdout: true, script: "set +x && cd ${projectName}; ./gradlew -q printVersion").trim()

  try {
    withCredentials([
      sshUserPrivateKey(credentialsId: sshCredentialsId, keyFileVariable: 'sshKey')
    ]) {
      // set tagging mail and name in git config
      sh(script: "set +x && cd $projectName && " +
      "git config user.email 'johannes.hiry@tu-dortmund.de' && " +
      "git config user.name 'Johannes Hiry'", returnStdout: false)

      // cleanup repo and tag it afterwards
      sh(script:
      "set +x && cd $projectName && " +
      "ssh-agent bash -c \"set +x && ssh-add $sshKey; " +
      "git branch | grep -v \"$tagBranchName\" | xargs git branch -D; " + // deletes all local branches except tagBranchName
      "git fetch && git checkout $tagBranchName && git pull && " +
      "git tag -m 'Release version $projectVersion.' $projectVersion && " +
      "git push origin --tags" +
      "\"", returnStdout: false)
    }
  } catch (Exception e) {
    println "Error when creating tag on main branch! Exception: $e"
  }
}

def deployJavaDocs(String projectName, String sshCredentialsId, String gitCheckoutUrl) {

  try {
    withCredentials([
      sshUserPrivateKey(credentialsId: sshCredentialsId, keyFileVariable: 'sshKey')
    ]) {
      // set mail and name in git config
      sh(script: "set +x && cd $projectName && " +
      "git config user.email 'johannes.hiry@tu-dortmund.de' && " +
      "git config user.name 'Johannes Hiry'", returnStdout: false)

      // create a temporary repo in the javadocs folder and push the updated javadocs to api-docs branch
      sh(script: "set +x && cd $projectName && " +
      "rm -rf tmp-api-docs && mkdir tmp-api-docs && cd tmp-api-docs && " +
      "ssh-agent bash -c \"set +x && ssh-add $sshKey; " +
      "git init && git remote add origin $gitCheckoutUrl && " +
      "git config user.email 'johannes.hiry@tu-dortmund.de' && " +
      "git config user.name 'Johannes Hiry' && " +
      "git fetch --depth=1 origin api-docs && " +
      "git checkout api-docs && " +
      "cd .. && ./gradlew javadoc && " +
      "cp -R build/docs/javadoc/* tmp-api-docs && " +
      "cd tmp-api-docs &&" +
      "git add --all && git commit -m 'updated api-docs' && git push origin api-docs:api-docs" +
      "\"",
      returnStdout: false)
    }
  } catch (Exception e) {
    println "Error when deploying javadocs! Exception: $e"
  }

}

/* gradle */

def gradle(String command, String relativeProjectDir) {
  env.JENKINS_NODE_COOKIE = 'dontKillMe' // this is necessary for the Gradle daemon to be kept alive

  // switch directory to be able to use gradle wrapper
  sh(script: """set +x && cd $relativeProjectDir""" + ''' set +x; ./gradlew ''' + """$command""", returnStdout: true)
}

def determineSonarqubeGradleCmd(String sonarqubeProjectKey, String currentBranchName, String targetBranchName, String orgName, String projectName, String relativeGitDir) {
  String prBaseBranch = targetBranchName == null ? "dev" : targetBranchName
  switch (currentBranchName) {
    case "main":
      return "sonarqube -Dsonar.branch.name=main -Dsonar.projectKey=$sonarqubeProjectKey"
      break
    case "dev":
      String[] branchVersion = gradle("-q currentVersion", relativeGitDir).toString().split('\\.')
      Integer major = branchVersion[0].toInteger()
      Integer minor = branchVersion[1].toInteger()
      String projectVersion = "${major}.${minor}-SNAPSHOT"
      return "sonarqube -Dsonar.projectVersion=${projectVersion} -Dsonar.projectKey=$sonarqubeProjectKey"
      break
    default:
      String gradleCommand = "sonarqube -Dsonar.projectKey=$sonarqubeProjectKey"
    // if this branch has a PR, the sonarqube cmd needs to be adapted
      if (env.CHANGE_ID == null) {
        // no PR exists
        return gradleCommand + " -Dsonar.branch.name=${currentBranchName}"
      } else {
        // PR exists, adapt cmd accordingly
        return gradleCommand + " -Dsonar.pullrequest.branch=${currentBranchName} -Dsonar.pullrequest.key=${env.CHANGE_ID} " +
            "-Dsonar.pullrequest.base=$prBaseBranch -Dsonar.pullrequest.github.repository=${orgName}/${projectName} " +
            "-Dsonar.pullrequest.provider=Github"
      }
      break
  }
}

def determineDisplayName(String currentBranchName, String commitHash, String orgName, String projectName) {

  String displayName = ""
  if (currentBranchName == "main" || currentBranchName == "dev") {
    // main and dev are always merge branches
    def jsonObject = getGithubCommitJsonObj(commitHash, orgName, projectName)
    displayName = "${jsonObject.commit.message} (${currentBuild.displayName})"
  } else {
    displayName = currentBranchName + " (" + currentBuild.displayName + ")"
  }

  return displayName
}

def publishReports(String relativeProjectDir) {
  // publish test reports

  // publish scalatest reports for main project only (currently the only one with scala sources!)
  publishHTML([allowMissing: false, alwaysLinkToLastBuild: true, escapeUnderscores: false, keepAll: true, reportDir: relativeProjectDir + '/build/reports/tests/test', reportFiles: 'index.html', reportName: "${relativeProjectDir}_scala_tests_report", reportTitles: ''])

  // publish scapegoat src report for main project only
  publishHTML([allowMissing: false, alwaysLinkToLastBuild: true, escapeUnderscores: false, keepAll: true, reportDir: relativeProjectDir + '/build/reports/scapegoat/src', reportFiles: 'scapegoat.html', reportName: "${relativeProjectDir}_scapegoat_src_report", reportTitles: ''])

  // publish scapegoat testsrc report for main project only
  publishHTML([allowMissing: false, alwaysLinkToLastBuild: true, escapeUnderscores: false, keepAll: true, reportDir: relativeProjectDir + '/build/reports/scapegoat/testsrc', reportFiles: 'scapegoat.html', reportName: "${relativeProjectDir}_scapegoat_testsrc_report", reportTitles: ''])

  // scoverage report dir
  publishHTML([allowMissing: false, alwaysLinkToLastBuild: true, escapeUnderscores: false, keepAll: true, reportDir: relativeProjectDir + '/build/reports/scoverageTest', reportFiles: 'scoverage.xml', reportName: "${relativeProjectDir}_scoverage_report", reportTitles: ''])
}

def prFromFork() {
  return env.CHANGE_FORK != null
}

/**
 * utility functions - methods that does not require node context
 */

/* properties */

def constantBranchesProps() {
  properties([
    parameters(
    [
      string(defaultValue: '', description: '', name: 'deploy', trim: true)
    ]),
    [$class: 'ThrottleJobProperty', categories: [], limitOneJobWithMatchingParams: false, maxConcurrentPerNode: 0, maxConcurrentTotal: 0, paramsToUseForLimit: '', throttleEnabled: true, throttleOption: 'project']
  ])
}

def temporaryBranchesProps() {
  properties(
      [
        pipelineTriggers([
          issueCommentTrigger('.*!test.*')
        ])
      ])
}

/* git interaction */

def getGithubCommitJsonObj(String commit_sha, String orgName, String repoName) {
  def jsonObj = readJSON text: curlByCSHA(commit_sha, orgName, repoName)
  return jsonObj
}

def curlByCSHA(String commit_sha, String orgName, String repoName) {

  def curlUrl = "curl -s https://api.github.com/repos/" + orgName + "/" + repoName + "/commits/" + commit_sha
  String jsonResponseString = sh(script: curlUrl, returnStdout: true)

  return jsonResponseString
}

def getGithubPRJsonObj(String prId, String orgName, String repoName) {
  def jsonObj = readJSON text: curlByPR(prId, orgName, repoName)
  return jsonObj
}

def curlByPR(String prId, String orgName, String repoName) {
  def curlUrl = "set +x && curl -s https://api.github.com/repos/" + orgName + "/" + repoName + "/pulls/" + prId
  String jsonResponseString = sh(script: curlUrl, returnStdout: true)
  return jsonResponseString
}

def getPRJsonObj(String orgName, String projectName, String changeId) {
  if (changeId == null) {
    return null
  } else {
    // PR exists, curl the api and retrieve target branch
    return getGithubPRJsonObj(changeId, orgName, projectName)
  }
}


def checkVersion(String branchName, String targetBranchName, String relativeGitDir,
    String projectName,
    String baseGitCheckoutUrl,
    String headGitCheckoutUrl,
    String sshCredentialsId) {
  // get current branch type
  // if headGitCheckoutUrl is set (= pr from fork), this branch type is always treated as a feature branch
  String branchType = prFromFork() ? "feature" : getBranchType(branchName)
  if (branchType == null) {
    println "Cannot derive branch type from current branch with name '$branchName'."
    return -1
  }

  // compare the version
  /// save the current version string
  String[] currentVersion = gradle("-q currentVersion", relativeGitDir).toString().split('\\.')

  /// switch to the comparison branch, this is always the base git checkout url
  gitCheckout(projectName, baseGitCheckoutUrl, targetBranchName, sshCredentialsId)
  String[] targetBranchVersion = gradle("-q currentVersion", relativeGitDir).toString().split('\\.')

  if (compareVersionParts(branchType, currentVersion, getBranchType(targetBranchName), targetBranchVersion) != 0) {
    // comparison failed
    return -1
  } else {
    // switch back to current branch. Select url depending on if this is a fork or not
    gitCheckout(projectName, headGitCheckoutUrl, branchName, sshCredentialsId)
    return 0
  }
}

def compareVersionParts(String sourceBranchType, String[] sourceBranchVersion, String targetBranchType, String[] targetBranchVersion) {

  switch (sourceBranchType) {
    case "hotfix":
      if (targetBranchType == "main") {
        boolean major = sourceBranchVersion[0].toInteger() == targetBranchVersion[0].toInteger()
        boolean minor = sourceBranchVersion[1].toInteger() == targetBranchVersion[1].toInteger()
        boolean patch = (sourceBranchVersion[2].toInteger() == targetBranchVersion[2].toInteger() + 1)

        if (major && minor && patch) {
          return 0
        } else {
          println "Hotfix branch versioning is invalid in comparison to main branch versioning. " +
              "Only mainBranch.patchVersion + 1 is allowed for hotfix branch!\n" +
              "hotfixVersion: ${sourceBranchVersion[0]}.${sourceBranchVersion[1]}.${sourceBranchVersion[2]}\n" +
              "mainVersion: ${targetBranchVersion[0]}.${targetBranchVersion[1]}.${targetBranchVersion[2]}"
          return -1
        }

      } else if (targetBranchType == "dev") {

        boolean major = sourceBranchVersion[0].toInteger() == targetBranchVersion[0].toInteger()
        boolean minor = sourceBranchVersion[1].toInteger() == targetBranchVersion[1].toInteger()
        boolean patch = (sourceBranchVersion[2].toInteger() == 0 && targetBranchVersion[2].toInteger() == 0)

        if (major && minor && patch) {
          return 0
        } else {
          println "Hotfix branch versioning is invalid in comparison to dev branch versioning. " +
              "Major and minor version must be equal and patch version must be 0.\n" +
              "hotfixVersion: ${sourceBranchVersion[0]}.${sourceBranchVersion[1]}.${sourceBranchVersion[2]}\n" +
              "devVersion: ${targetBranchVersion[0]}.${targetBranchVersion[1]}.${targetBranchVersion[2]}"
          return -1
        }

      } else {
        // invalid branch type for hotfix merge
        return -1
      }
      break
    case "feature":
      if (targetBranchType == "dev") {
        // no change in semVer allowed
        boolean major = sourceBranchVersion[0].toInteger() == targetBranchVersion[0].toInteger()
        boolean minor = sourceBranchVersion[1].toInteger() == targetBranchVersion[1].toInteger()
        boolean patch = (sourceBranchVersion[2].toInteger() == 0 && targetBranchVersion[2].toInteger() == 0)

        if (major && minor && patch) {
          return 0
        } else {
          println "Feature branch versioning differs from dev branch versioning. This is not allowed!\n" +
              "featureVersion: ${sourceBranchVersion[0]}.${sourceBranchVersion[1]}.${sourceBranchVersion[2]}\n" +
              "devVersion: ${targetBranchVersion[0]}.${targetBranchVersion[1]}.${targetBranchVersion[2]}"
          return -1
        }

      } else {
        // invalid branch type for feature merge
        println "Invalid target branch type '$targetBranchType' for feature branch. Feature branches can only" +
            "be merged into dev branch!"
        return -1
      }
      break
    case "release":
      if (targetBranchType == "main" || targetBranchType == "dev") {
        Integer targetMajor = targetBranchVersion[0].toInteger()
        Integer targetMinor = targetBranchVersion[1].toInteger()

        Integer sourceMajor = sourceBranchVersion[0].toInteger()
        Integer sourceMinor = sourceBranchVersion[1].toInteger()

        boolean validCheck1 = targetMajor == sourceMajor && targetMinor + 1 == sourceMinor
        boolean validCheck2 = targetMajor + 1 == sourceMajor

        // patch version always needs to be 0
        boolean patchValid = sourceBranchVersion[2].toInteger() == 0

        if ((validCheck1 || validCheck2) && patchValid) {
          return 0
        } else {
          println "Release branch versioning does not fit to main branch versioning!\nA release should increase " +
              "either major or minor version and reset patch version to 0.\n" +
              "releaseVersion: ${sourceBranchVersion[0]}.${sourceBranchVersion[1]}.${sourceBranchVersion[2]}\n" +
              "${targetBranchType == "main" ? "mainVersion" : "devVersion"}: ${targetBranchVersion[0]}.${targetBranchVersion[1]}.${targetBranchVersion[2]}"
          return -1
        }
      } else {
        // invalid target branch type for release merge
        println "Merging release branch into branch type '$targetBranchType' is not supported! Realease branches" +
            "can only be merged into main or dev branch!"
        return -1
      }
    // major == major OR major == major + 1, minor == minor OR minor == minor + 1, patch == patch == 0
      break
    case "dev":
    // target branch type can only be main branch
      if (targetBranchType == "main") {
        // only major and minor version parts need to be checked
        Integer targetMajor = targetBranchVersion[0].toInteger()
        Integer targetMinor = targetBranchVersion[1].toInteger()

        Integer sourceMajor = sourceBranchVersion[0].toInteger()
        Integer sourceMinor = sourceBranchVersion[1].toInteger()

        boolean validCheck1 = targetMajor == sourceMajor && targetMinor + 1 == sourceMinor
        boolean validCheck2 = targetMajor + 1 == sourceMajor

        // patch version always needs to be 0
        boolean patchValid = sourceBranchVersion[2].toInteger() == 0

        if ((validCheck1 || validCheck2) && patchValid) {
          return 0
        } else {
          println "Dev branch versioning does not fit to main branch versioning! Must increase either major or minor version!\n" +
              "devVersion: ${sourceBranchVersion[0]}.${sourceBranchVersion[1]}.${sourceBranchVersion[2]}\n" +
              "mainVersion: ${targetBranchVersion[0]}.${targetBranchVersion[1]}.${targetBranchVersion[2]}"
          return -1
        }
      } else {
        // invalid branch type for dev branch version comparison
        println "Invalid branch type '$targetBranchType' to be compared with dev branch. Dev branch version " +
            "can only be compared with main branch type!"
        return -1
      }
      break
    default:
      return -1
      break
  }

}

def getBranchType(String branchName) {
  def dev_pattern = "^(developer|develop|dev)\$"
  def release_pattern = ".*rel/.*"
  def feature_pattern = "^\\pL{2}/#\\d+.*"
  def dependabot_pattern = "^dependabot/.*\$"
  def hotfix_pattern = ".*hotfix/\\pL{2}/#\\d+.*"
  def main_pattern = ".*main"
  if (branchName =~ feature_pattern || branchName =~ dependabot_pattern) {
    return "feature"
  } else if (branchName =~ release_pattern) {
    return "release"
  } else if (branchName =~ main_pattern) {
    return "main"
  } else if (branchName.toLowerCase() =~ dev_pattern) {
    return "dev"
  } else if (branchName =~ hotfix_pattern) {
    return "hotfix"
  } else {
    return null
  }
}
