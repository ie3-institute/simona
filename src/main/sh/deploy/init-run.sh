#!/bin/bash

# requires the following tools serverside installed:
# - curl
# - java
# - scala
# - nginx or apache (for web logs)

# server specific config parameters
SERVER_IP=129.217.187.155
SERVER_WEB_LOG_URI="http://$SERVER_IP/simona/"

SIMONA_BASE_PATH='/srv/simona/'
SIMONA_KEY_PATH=${SIMONA_BASE_PATH}'00_key/'
SIMONA_RUN_DIR='01_runs/'
SIMONA_RUN_PATH=${SIMONA_BASE_PATH}${SIMONA_RUN_DIR}

GIT_CLONE_URL='git@git.ie3.e-technik.tu-dortmund.de:SIMONACrew/SIMONA.git'
GIT_ID='deployKey'

ROCKET_CHAT_TOKEN=$(cat ${SIMONA_KEY_PATH}"rocketChatToken")
ROCKET_CHAT_URL='https://rocketchat.ie3.e-technik.tu-dortmund.de/hooks/Cn7xQyKRzAeFptETK/'${ROCKET_CHAT_TOKEN}

### parameter checks ###
script="init-run.sh"
# Declare the number of mandatory args
margs=3

# Common functions - BEGIN
function example() {
  echo -e "example: $script -b jh/#123-test-run -c 92e8b8bea18e8f8a62f6397e05d369acaba0e3b7 -s input/samples/vn_simona/vn_simona.conf"
}

function usage() {
  echo -e "usage: $script -b <gitBranchName> -c <commitHash> -s <filepath>"
}

function help() {
  usage
  echo -e "MANDATORY:"
  echo -e "  -b, --git_branch <git branch name> branch that should be used\n"
  echo -e "  -c, --git_commit <commitHash> commit hash that should be used"
  echo -e "  -s, --simona_config <filepath> file path to the simona config that should be executed"
  #  echo -e "OPTION:"
  #  echo -e "  -o0, --optional1        The desc of the optional0 boolean parameter"
  #  echo -e "  -o1, --optional2   VAL  The desc of the optional1 String  parameter"
  echo -e "  -h,  --help             Prints this help\n"
  example
}

# notify rocket chat instance
function notify_rocket_chat() {
  msg=$1

  # create data payload
  data=$(printf '{"text":"%s"}' "$msg")

  # fire
  curl -s -X POST -H 'Content-Type: application/json' --data "${data}" "${ROCKET_CHAT_URL}"
}

# Ensures that the number of passed args are at least equals
# to the declared number of mandatory args.
# It also handles the special case of the -h or --help arg.
function margs_precheck() {
  if [ $2 ] && [ $1 -lt $margs ]; then
    if [ $2 == "--help" ] || [ $2 == "-h" ]; then
      help
      exit
    else
      usage
      example
      notify_rocket_chat ":booom: number of passed args for script $script is invalid!"
      exit 1 # error
    fi
  fi
}

# Ensures that all the mandatory args are not empty
function margs_check() {
  if [ $# -lt $margs ]; then
    usage
    example
    notify_rocket_chat ":booom: missing mandatory args in script $script.\n$(usage)\n$(example)"
    exit 1 # error
  fi
}
# Common functions - END

# Main
margs_precheck $# $1

GIT_BRANCH=
GIT_COMMIT=
SIMONA_CONFIG=
#oarg0="false"
#oarg1="default"

# Args while-loop
while [ "$1" != "" ]; do
  case $1 in
  -b | --git_branch)
    shift
    GIT_BRANCH=$1
    ;;
  -c | --git_commit)
    shift
    GIT_COMMIT=$1
    ;;
  -s | --simona_config)
    shift
    SIMONA_CONFIG=$1
    ;;
    #  -o0 | --optional0)
    #    oarg0="true"
    #    ;;
    #  -o1 | --optional1)
    #    shift
    #    oarg1=$1
    #    ;;
  -h | --help)
    help
    exit
    ;;
  *)
    echo "$script: illegal option $1"
    usage
    example
    notify_rocket_chat ":booom: illegal option '$1' in script $script.\n$(usage)\n$(example)"
    exit 1 # error
    ;;
  esac
  shift
done

# Pass here your mandatory args for check
# shellcheck disable=SC2086
margs_check $GIT_BRANCH $GIT_COMMIT $SIMONA_CONFIG

### start script
START_DATE_TIME=$(date --iso-8601=seconds | tr : _)
START_USER=$(whoami)

# define run dir
RUN_DIR=${START_DATE_TIME}'_'${START_USER}'/'
RUN_PATH=${SIMONA_RUN_PATH}${RUN_DIR}

# create run dir
echo "creating run dir" "${RUN_PATH}"
mkdir -p "$RUN_PATH" # todo maybe include config name

notify_rocket_chat ":my_little_pony: run started ...\n*branch:* ${GIT_BRANCH} \n*commit:* ${GIT_COMMIT}\n \
*config:* ${SIMONA_CONFIG}\n \
*run_dir:* ${RUN_PATH}\n \
*run_dir_uri:* ${SERVER_WEB_LOG_URI}${SIMONA_RUN_DIR}${RUN_DIR}\n \
*log:* ${SERVER_WEB_LOG_URI}${SIMONA_RUN_DIR}${RUN_DIR}run.log"

# setup logging in run dir
exec 3>&1 4>&2
trap 'exec 2>&4 1>&3' 0 1 2 3
exec 1>"$RUN_PATH"'run.log' 2>&1
#set -x # log each cmd

# copy deploy key to make it usable
echo "############## Starting simona run init for branch ${GIT_BRANCH} and commit ${GIT_COMMIT} ##############"
echo "preparing deployment key ..."
mkdir -p ${SIMONA_KEY_PATH}"$START_USER"
cp ${SIMONA_KEY_PATH}$GIT_ID ${SIMONA_KEY_PATH}"$START_USER"
chmod 600 ${SIMONA_KEY_PATH}"$START_USER/"$GIT_ID
echo "deployment key ready!"

# checkout code
echo "starting checkout ..."
# shellcheck disable=SC2164
cd "${RUN_PATH}"

ssh-agent bash -c "ssh-add ${SIMONA_KEY_PATH}""$START_USER"/"$GIT_ID; git clone ${GIT_CLONE_URL} --branch ${GIT_BRANCH} simona; cd simona; git checkout ${GIT_COMMIT}"
success=$?
if [[ $success -eq 0 ]]; then
  echo "checkout complete!"
else
  echo "checkout failed!"
  notify_rocket_chat ":booom: run init failed. git checkout failed!\n*branch:* ${GIT_BRANCH} \n*commit:* ${GIT_COMMIT}"
  exit 1
fi

# setup docker db env if required
if grep -q influxDb1x "${RUN_PATH}simona/$SIMONA_CONFIG"; then
  # allocate a random port number between 5000 and 6000 for the influxDb
  #  read -r LOWER_PORT UPPER_PORT < /proc/sys/net/ipv4/ip_local_port_range || exit -1
  LOWER_PORT=5000
  UPPER_PORT=6000
  while :; do
    PORT="$(shuf -i $LOWER_PORT-$UPPER_PORT -n 1)"
    ss -lpn | grep -q ":$PORT " || break
  done

  # create folder holding the db
  DB_PATH="${RUN_PATH}docker_output"
  mkdir -p "$DB_PATH"

  # set database name for influxdb
  INFLUX_DB_NAME=simona

  # startup influxDb1x docker container
  IFS=': ' read -r -a HASHES <<<"$("$RUN_PATH"simona/sh/influxDb1x/run_influxdb_with_metrics.sh \
    "$RUN_DIR" \
    "$SERVER_IP" \
    "$PORT" \
    ${SERVER_WEB_LOG_URI}${SIMONA_RUN_DIR}"${RUN_DIR}"grafana \
    "$DB_PATH" \
    $INFLUX_DB_NAME)"
  INFLUXDB_DOCKER_HASH="${HASHES[0]}"
  GRAFANA_DOCKER_HASH="${HASHES[1]}"

  # replace variables in config url, port, database
  echo "Replacing influxDb1x config values in simona config ..."
  tArgs="simona.output.sink.influxDb1x.url=http://$SERVER_IP,simona.output.sink.influxDb1x.port=$PORT,simona.output.sink.influxDb1x.database=$INFLUX_DB_NAME"

  echo "New config parameters: tArgs=$tArgs"

  # notify rocket chat again
  notify_rocket_chat ":asdf: InfluxDb with metrics for run $RUN_DIR started!\n \
  *influxDb-Port:* $PORT \n \
  *grafana:* ${SERVER_WEB_LOG_URI}${SIMONA_RUN_DIR}${RUN_DIR}grafana"
fi

# assemble jar
echo "starting simona assembling ..."
# shellcheck disable=SC2164
cd "$RUN_PATH""simona"
./gradlew assemble

echo "############## Run Init Complete ##############"

# start simulation run
echo "############## Starting SIMONA simulation ##############"
chmod 777 sh/run-simona-cmd.sh
if [ -z ${tArgs+x} ]; then
  ./sh/run-simona-cmd.sh --config="$SIMONA_CONFIG";
else
  ./sh/run-simona-cmd.sh --config="$SIMONA_CONFIG" --tArgs="$tArgs";
fi

# shutdown docker container (if any)
if [ -n "${INFLUXDB_DOCKER_HASH+x}" ]; then docker kill "$INFLUXDB_DOCKER_HASH"; fi
if [ -n "${GRAFANA_DOCKER_HASH+x}" ]; then docker kill "$GRAFANA_DOCKER_HASH"; fi

echo "############## SIMONA simulation terminated ##############"
notify_rocket_chat ":dancing_toad: run terminated!\n \
*branch:* ${GIT_BRANCH}\n \
*commit:* ${GIT_COMMIT}\n \
*config:* ${SIMONA_CONFIG}\n \
*run_dir:* ${RUN_PATH}\n \
*run_dir_uri:* ${SERVER_WEB_LOG_URI}${SIMONA_RUN_DIR}${RUN_DIR}\n \
*log:* ${SERVER_WEB_LOG_URI}${SIMONA_RUN_DIR}${RUN_DIR}run.log"
