FROM openjdk:8-jre-slim

# USAGE:
# build with ARG version and if applicable with ARG snapshot suffix
    # e.g.: docker build --build-arg version=1.0 --build-arg snapshotSuffix=-SNAPSHOT -t simona .
# run by mounting directory
    # e.g. docker run -v `realpath inputData`:/inputData --rm simona
        # note: this does not work for windows so you have to enter the absolute path manually and escape the \'s

ARG version
# snapshot suffix for jar files is "-SNAPSHOT"
ARG snapshotSuffix=""

ENV jarFile="simona-${version}${snapshotSuffix}-all.jar"
ENV config=""

RUN mkdir exec
RUN mkdir input

# copy simona fat jar into container
COPY build/libs/$jarFile exec/
# inputData is mounted upon running
VOLUME /input

ENTRYPOINT ["sh", "-c", "java -jar exec/${jarFile} --config=${config}"]