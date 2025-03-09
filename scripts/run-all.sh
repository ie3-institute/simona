#!/bin/bash
set -euo pipefail

rm -f versions.env

./get_versions.sh

source versions.env

./version_check.sh
