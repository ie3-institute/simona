#!/bin/bash

set -e  # Exit on error

# Ensure the script is executed from the repository root
cd "$(dirname "$0")/.."

PR_VERSION="${PR_VERSION}"
DEV_VERSION="${DEV_VERSION}"
MAIN_VERSION="${MAIN_VERSION}"
BASE_BRANCH="${BASE_BRANCH}"

echo "PR_VERSION=$PR_VERSION"
echo "DEV_VERSION=$DEV_VERSION"
echo "MAIN_VERSION=$MAIN_VERSION"
echo "BASE_BRANCH=$BASE_BRANCH"

bash /scripts/branch_type.sh

# Version Checking Logic
if [ "$BASE_BRANCH" = "dev" ]; then
  echo "PR into dev => applying dev rules"
  if [ "$DEV_VERSION" = "$PR_VERSION" ]; then
    echo "OK: dev version == PR version"
    exit 0
  else
    if [ "$MAIN_VERSION" = "$DEV_VERSION" ]; then
      if [[ "$PR_VERSION" > "$DEV_VERSION" ]]; then
        echo "Bumping dev"
        ./gradlew incrementMinor
        exit 0
      else
        echo "FAIL: dev == main, but PR version is NOT greater than dev."
        exit 1
      fi
    else
      echo "FAIL: dev != PR version, and dev != main."
      exit 1
    fi
  fi

elif [ "$BASE_BRANCH" = "main" ]; then
  if [[ "$PR_VERSION" > "$MAIN_VERSION" ]]; then
    echo "OK: PR version is greater than main version"
    exit 0
  else
    echo "FAIL: PR version is NOT greater than main version"
    exit 1
  fi

else
  echo "Base branch is '$BASE_BRANCH'; version check skipped"
  exit 0
fi
