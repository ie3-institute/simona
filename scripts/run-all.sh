#!/bin/bash
set -euo pipefail

rm -f versions.env

scripts/get_versions.sh

source versions.env

scripts/version_check.sh
