#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "$SCRIPT_DIR/branch_type.sh"

semver_gt() {
  IFS='.' read -r major1 minor1 patch1 <<< "$1"
  IFS='.' read -r major2 minor2 patch2 <<< "$2"

  # Compare major version
  if [ "$major1" -gt "$major2" ]; then
    return 0
  elif [ "$major1" -lt "$major2" ]; then
    return 1
  fi

  # Compare minor version
  if [ "$minor1" -gt "$minor2" ]; then
    return 0
  elif [ "$minor1" -lt "$minor2" ]; then
    return 1
  fi

  # Compare patch version
  if [ "$patch1" -gt "$patch2" ]; then
    return 0
  else
    return 1
  fi
}

# Version Checking Logic
if [ "$BASE_BRANCH" = "dev" ]; then
  echo "PR into dev => applying dev rules"
  if [ "$DEV_VERSION" = "$PR_VERSION" ]; then
    echo "OK: dev version == PR version"
    exit 0
  else
    if [ "$MAIN_VERSION" = "$DEV_VERSION" ]; then
      if semver_gt "$PR_VERSION" "$DEV_VERSION"; then
        echo "Bump dev"
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
  if semver_gt "$PR_VERSION" "$MAIN_VERSION"; then
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

rm -f versions.env
echo "Version Check: OK!"
