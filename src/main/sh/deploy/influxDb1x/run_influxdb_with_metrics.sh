#!/bin/bash

# todo parameter check + help
# passed in parameters
RUN_NAME=$1              # the name of the simona run
SERVER_IP=$2             # the server ip this script is executed on, e.g. 192.168.2.10
INFLUXDB_EXPOSED_PORT=$3 # the port the influxdb should be exposed to, e.g. 9000
GRAFANA_ROOT_URL=$4      # the root url of the grafana instance, required to be in sync with nginx, e.g. http://192.168.2.10/simona/$RUN_NAME
BASE_PATH=$5             # the base path where the database files should be placed
INFLUX_DB_NAME=$6     # name of the influxdb that should be used

# derived parameters
INFLUXDB_VOLUME_PATH=$BASE_PATH/influxdb_data_$RUN_NAME

# switch to the dir of this script
CALL_DIR=$(pwd)
cd "$(dirname "$( realpath "$0" )")" || return

echo >&2 'Starting SIMONA database InfluxDB with metrics environment ...'

echo >&2 "- Starting InfluxDB @ port $INFLUXDB_EXPOSED_PORT ..."
# start influxdb with custom port mapping
INFLUXDB_DOCKER_HASH=$(./sh/start_docker_influxdb.sh "$INFLUXDB_VOLUME_PATH" "$INFLUXDB_EXPOSED_PORT" "$SERVER_IP" "$INFLUX_DB_NAME" "$RUN_NAME")
echo >&2 "- InfluxDB: $INFLUXDB_DOCKER_HASH"
echo >&2 "- InfluxDB @ port $INFLUXDB_EXPOSED_PORT up and running!"

echo >&2 '- Starting Grafana ...'
# start grafana with custom port mapping
## setup provisioning first
GRAFANA_DATA_PATH=$BASE_PATH/grafana_data_$RUN_NAME
mkdir -p "$GRAFANA_DATA_PATH"
cp -R grafana/provisioning "$GRAFANA_DATA_PATH"

GRAFANA_PROVISIONING_PATH="$GRAFANA_DATA_PATH"provisioning/

## modify provisioning datasource.yml with influxdb connection parameters
sed -i.bak "s/influxdb:8086/$SERVER_IP:$INFLUXDB_EXPOSED_PORT/" "$GRAFANA_PROVISIONING_PATH"datasources/datasource.yml && rm "$GRAFANA_PROVISIONING_PATH"datasources/datasource.yml.bak
sed -i.bak "s/database: simona/database: $INFLUX_DB_NAME/" "$GRAFANA_PROVISIONING_PATH"datasources/datasource.yml && rm "$GRAFANA_PROVISIONING_PATH"datasources/datasource.yml.bak

## create grafana persistent data path
GRAFANA_STORAGE_PATH=$(pwd)/$GRAFANA_DATA_PATH/data_storage
mkdir -p "$GRAFANA_STORAGE_PATH"
chmod o+w "$GRAFANA_STORAGE_PATH"

## eventually start the container
GRAFANA_DOCKER_HASH=$(./sh/start_docker_grafana.sh 01_runs/"$RUN_NAME"grafana "$GRAFANA_ROOT_URL" "$GRAFANA_PROVISIONING_PATH" "$GRAFANA_STORAGE_PATH" "$RUN_NAME")
echo >&2 "- Grafana: $GRAFANA_DOCKER_HASH"
echo >&2 '- Grafana up and running!'
echo >&2 'SIMONA database InfluxDB with metrics environment started!'

echo "$INFLUXDB_DOCKER_HASH":"$GRAFANA_DOCKER_HASH"

cd "$CALL_DIR" || return