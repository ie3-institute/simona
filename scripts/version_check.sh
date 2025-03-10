#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "========================="
echo "LOADED ENV VARIABLES:"
echo "PR_VERSION: $PR_VERSION"
echo "DEV_VERSION: $DEV_VERSION"
echo "MAIN_VERSION: $MAIN_VERSION"
echo "BASE_BRANCH: $BASE_BRANCH"
echo "========================="

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
    echo "OK: PR version ($PR_VERSION) matches the current dev version ($DEV_VERSION)."
    exit 0
  else
    if [ "$MAIN_VERSION" = "$DEV_VERSION" ]; then
      if semver_gt "$PR_VERSION" "$DEV_VERSION"; then
        echo "OK: Increasing working version in dev from $DEV_VERSION to $PR_VERSION"
        exit 0
      else
        echo "FAIL: Release and working version are $MAIN_VERSION, but PR is not increasing the working version in dev"
        exit 1
      fi
    else
      echo "FAIL: PR version ($PR_VERSION) does not match the current dev version ($DEV_VERSION)."
      echo "Regular PRs must not update the working version. The working version should only change in controlled updates."
      exit 1
    fi
  fi

elif [ "$BASE_BRANCH" = "main" ]; then
  echo "PR into main => applying main rules"
  if semver_gt "$PR_VERSION" "$MAIN_VERSION"; then
    echo "OK: PR version ($PR_VERSION) is greater than the current main version ($MAIN_VERSION)."
    exit 0
  else
    echo "FAIL: PR version ($PR_VERSION) is NOT greater than the current main version ($MAIN_VERSION)."
    echo "A new release must have a higher version than the existing main version."
    exit 1
  fi

else
  echo "Skipping version check: Base branch is '$BASE_BRANCH'. No version enforcement required."
  exit 0
fi
