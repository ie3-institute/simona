#!/bin/bash

VOLUME_PATH=$1
EXPOSED_PORT=$2
SERVER_IP=$3
DB_NAME=$4
RUN_NAME=$(echo "$5" | tr -dc '[a-zA-Z0-9]\n\r')

ABSOLUT_DIR_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

DOCKER_HASH=$(docker run \
  -d \
  --rm \
  -v "$VOLUME_PATH":/var/lib/influxdb \
  -p "$EXPOSED_PORT":8086 \
  -e INFLUXDB_DB="$DB_NAME" \
  --name "$RUN_NAME"_influxDb \
  influxdb:1.8.3)

"$ABSOLUT_DIR_PATH"/wait-for.sh "$SERVER_IP":"$EXPOSED_PORT" -- echo "$DOCKER_HASH"