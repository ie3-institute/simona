/* All task to deploy simona on a dedicated server. To use deployment you need to setup a deploy.properties in the
 * root folder of the project first. sample.deploy.properties can be used as a blueprint.
 * */

/**
 * Task to run a simona simulation on a dedicated server
 */
tasks.register("deployAndRun") {
    doFirst {
        // load deploy properties
        val props = Properties()
        file("deploy.properties").inputStream().use { props.load(it) }

        // validate properties file
        validateProps(props)

        // setup session data
        val sessionData = mapOf(
            "name" to props.getProperty("server.name"),
            "host" to props.getProperty("server.host"),
            "user" to props.getProperty("server.user"),
            "identity" to file(props.getProperty("server.idFile"))
        )

        val tmp_dir_path = "/home/" + props.getProperty("server.user") + "/00_simona_tmp/"

        // upload required shell script into /home/<user>/simona_tmp and run it
        ssh.run {
            session(sessionData) {
                println("Preparing dirs ...")
                // remove dir first
                executeCommand("rm -rf $tmp_dir_path")
                // create dir
                executeCommand("mkdir $tmp_dir_path")

                println("Uploading script ...")
                // upload scripts
                put(from = project.file("sh/init-run.sh"), into = tmp_dir_path)

                println("Adjusting script permissions ...")
                executeCommand("chmod +x ${tmp_dir_path}init-run.sh")

                println("Executing script ...")
                execute("tmux new-session -d ${tmp_dir_path}init-run.sh -b ${props.getProperty("git.branch")} -c ${props.getProperty("git.commit")} -s ${props.getProperty("simona.config")}")
                // remove tmp dir
                executeCommand("rm -rf $tmp_dir_path")
            }
        }
    }
}

/**
 * Validate deploy.properties structure, does not check for valid configuration parameter values
 */
fun validateProps(props: Properties) {
    // server config keys
    listOf(
        "server.name",
        "server.host",
        "server.user",
        "server.idFile"
    ).forEach {
        if (!props.containsKey(it))
            validateException(it)
    }

    // git config keys
    listOf("git.branch", "git.commit").forEach {
        if (!props.containsKey(it))
            validateException(it)
    }

    // simona run config keys
    listOf("simona.config").forEach {
        if (!props.containsKey(it))
            validateException(it)
    }
}

fun validateException(key: String) {
    throw RuntimeException("Missing server deployment property key '$key'")
}
