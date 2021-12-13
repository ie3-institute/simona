FROM openjdk:8-jre-slim

# USAGE:
# build with ARG version and if applicable with ARG snapshot suffix
    # e.g.: docker build --build-arg version=1.0 --build-arg snapshotSuffix=-SNAPSHOT -t simona .
# run by mounting directory
    # e.g. docker run -v `realpath inputData`:/inputData --rm simona

ARG version
# snapshot suffix for jar files is "-SNAPSHOT"
ARG snapshotSuffix=""

ENV jarFile="simona-${version}${snapshotSuffix}-all.jar"

RUN mkdir exec
RUN mkdir inputData

# copy simona fat jar into container
COPY build/libs/$jarFile exec/
# inputData is mounted upon running
VOLUME /inputData

ENTRYPOINT ["sh", "-c", "java -cp exec/${jarFile} edu.ie3.simona.main.RunSimonaStandalone --config=inputData/vn_simona/vn_simona.conf"]