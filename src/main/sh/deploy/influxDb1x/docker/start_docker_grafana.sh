#!/bin/bash

VIRTUAL_HOST_NAME=$1
GRAFANA_ROOT_URL=$2
PROVISIONING_PATH=$3
STORAGE_PATH=$4
RUN_NAME=$(echo "$5" | tr -dc '[a-zA-Z0-9]\n\r')

docker run \
  -d \
  --rm \
  -v "$PROVISIONING_PATH":/etc/grafana/provisioning \
  -v "$STORAGE_PATH":/var/lib/grafana \
  -e VIRTUAL_HOST="$VIRTUAL_HOST_NAME" \
  -e GF_SERVER_ROOT_URL="$GRAFANA_ROOT_URL" \
  -e GF_SERVER_SERVE_FROM_SUB_PATH=true \
  --name "$RUN_NAME"_grafana \
  grafana/grafana:latest
