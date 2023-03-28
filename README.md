<p align="center">
<img src="docs/logo/logo_tightcrop_transparent.png" width="200px" alt="simona logo" align="center"/>
</p>

# SIMONA
[![Build Status](https://simona.ie3.e-technik.tu-dortmund.de/ci/buildStatus/icon?job=ie3-institute%2Fsimona%2Fdev)](https://simona.ie3.e-technik.tu-dortmund.de/ci/job/ie3-institute/job/simona/job/dev/)
[![Quality Gate Status](https://simona.ie3.e-technik.tu-dortmund.de/sonar/api/project_badges/measure?project=edu.ie3%3Asimona&metric=alert_status)](https://simona.ie3.e-technik.tu-dortmund.de/sonar/dashboard?id=edu.ie3%3Asimona)
[![codecov](https://codecov.io/gh/ie3-institute/simona/branch/main/graph/badge.svg?token=pDg4Pbbp9L)](https://codecov.io/gh/ie3-institute/simona)
[![Documentation Status](https://readthedocs.org/projects/simona/badge/?version=latest)](https://simona.readthedocs.io/en/latest/?badge=latest)
[![License](https://img.shields.io/github/license/ie3-institute/simona)](https://github.com/ie3-institute/simona/blob/main/LICENSE)
[![Maven Central](https://img.shields.io/maven-central/v/com.github.ie3-institute/simona.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22com.github.ie3-institute%22%20AND%20a:%22simona%22)

The agent-based simulation environment SIMONA provides a simulation toolbox to run and implement large-scale agent-based
electricity grid simulations with focus on distribution grids. As a result, close-to-reality time series are
generated from various system participants and grid assets that can be used to analyze a given power grid.
Application cases are for example distribution grid planning purposes but also flexibility analysis or coupled
sector interdependency analysis. The framework contains several out-of-the-box available models for a wide variety of grid participants as well as their
operational behavior.

More information are provided in the project's [documentation](http://simona.readthedocs.io/).

## Usage (Docker)

To build the Docker image using the Dockerfile, you need to provide a build argument version and optionally snapshotSuffix if applicable.

```
docker build --build-arg version=2.1.0 --build-arg snapshotSuffix=-SNAPSHOT -t simona .
```
This command will create a Docker image named simona.

To run the Docker container, you need to mount an input directory from your host machine to the /input directory inside the container.

For Linux and macOS systems, use the following command:

```
docker run -v `realpath input`:/input --rm simona
```
For Windows systems, provide the absolute path to the input directory manually and escape the backslashes (\). Replace <path-to-input> with the actual path to the input directory on your system:

```
docker run -v <path-to-input>:/input --rm simona
```
This command will run the container with the input directory mounted, execute the simona Java application, and remove the container after the execution is finished.

## Contribution
SIMONA is part of several ongoing research projects and will be part of future research projects. Hence, the codebase
is continuously under development from different perspectives, needs and developers.

We invite everyone to use SIMONA for their own research or for usage in a research project. If you use SIMONA for your
own projects or research, please provide a reference to this repository. Furthermore, if you publish your scientific work
please give appropriate credit by citing one of the introduction papers of SIMONA. 

We're also happy for any feedback and contributions. For details on how to contribute, please take a look at the
CONTRIBUTING.md file in the root directory of this repository.

## Questions
For all SIMONA related questions please feel free to contact people involved in the development and maintenance of SIMONA.
For the moment, these people are:

- Feismann, Daniel - [daniel.feismann@tu-dortmund.de](mailto:daniel.feismann@tu-dortmund.de)
- Peter, Sebastian - [sebastian.peter@tu-dortmund.de](mailto:sebastian.peter@tu-dortmund.de)
- Oberließen, Thomas - [thomas.oberliessen@tu-dortmund.de](mailto:thomas.oberliessen@tu-dortmund.de)
- Sen Sarma, Debopama - [debopama-sen.sarma@tu-dortmund.de](mailto:debopama-sen.sarma@tu-dortmund.de)
- Bao, Johannes - [johannes.bao@tu-dortmund.de](mailto:johannes.bao@tu-dortmund.de)
- Hohmann, Julian - [julian.hohmann@tu-dortmund.de](mailto:julian.hohmann@tu-dortmund.de)
- Kittl, Chris - [chris.kittl@tu-dortmund.de](mailto:chris.kittl@tu-dortmund.de)
- Hiry, Johannes - [johannes.hiry@tu-dortmund.de](mailto:johannes.hiry@tu-dortmund.de)
