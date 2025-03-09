#!/bin/bash
set -euo pipefail

rm -f versions.env

chmod +x scripts/get_versions.sh
chmod +x scripts/version_check.sh

scripts/get_versions.sh

source versions.env

scripts/version_check.sh
