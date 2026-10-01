#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

REPO_URL=$(git config --get remote.origin.url)
export REPO_URL
echo "REPO_URL=$REPO_URL" >> $GITHUB_ENV

parse_version() {
    local SOURCE=$1
    local PROPS MAJOR MINOR PATCH

    PROPS=$(tr -d '\r')
    MAJOR=$(sed -n 's/^version\.major=//p' <<< "$PROPS")
    MINOR=$(sed -n 's/^version\.minor=//p' <<< "$PROPS")
    PATCH=$(sed -n 's/^version\.patch=//p' <<< "$PROPS")

    if [[ ! "$MAJOR.$MINOR.$PATCH" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
        echo "ERROR: Invalid version in version.properties of $SOURCE: '$MAJOR.$MINOR.$PATCH'" >&2
        exit 1
    fi

    echo "$MAJOR.$MINOR.$PATCH"
}

echo "Fetching current version of PR..."
PR_VERSION=$(parse_version "PR" < version.properties)
echo "PR_VERSION=$PR_VERSION"
echo "export PR_VERSION=$PR_VERSION" >> versions.env
echo "PR_VERSION=$PR_VERSION" >> "$GITHUB_ENV"

get_branch_version() {
    local BRANCH_NAME=$1
    local BRANCH_VERSION

    git fetch --quiet origin "+refs/heads/$BRANCH_NAME:refs/remotes/origin/$BRANCH_NAME"

    echo "Fetching version from $BRANCH_NAME branch..."

    BRANCH_VERSION=$(git show "origin/$BRANCH_NAME:version.properties" | parse_version "$BRANCH_NAME")

    echo "${BRANCH_NAME^^}_VERSION=$BRANCH_VERSION"
    echo "export ${BRANCH_NAME^^}_VERSION=$BRANCH_VERSION" >> versions.env
    echo "${BRANCH_NAME^^}_VERSION=$BRANCH_VERSION" >> "$GITHUB_ENV"
}


get_branch_version "dev"
get_branch_version "main"

echo "Get Versions: OK!"
