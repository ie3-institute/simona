#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "Fetching current version of PR..."
PR_VERSION=$(./gradlew -q currentVersion)
echo "PR_VERSION=$PR_VERSION"
export "PR_VERSION=$PR_VERSION"
echo "PR_VERSION=$PR_VERSION" >> "$GITHUB_ENV"

get_branch_version() {
    local BRANCH_NAME=$1
    local DIR_NAME="${BRANCH_NAME}-branch"

    git clone --depth 1 --branch "$BRANCH_NAME" "$REPO_URL" "$DIR_NAME"
    cd "$DIR_NAME"

    echo "Fetching version from $BRANCH_NAME branch..."
    BRANCH_VERSION=$(./gradlew -q currentVersion)
    cd ..

    echo "${BRANCH_NAME^^}_VERSION=$BRANCH_VERSION"
    export "${BRANCH_NAME^^}_VERSION=$BRANCH_VERSION"
    echo "${BRANCH_NAME^^}_VERSION=$BRANCH_VERSION" >> "$GITHUB_ENV"

    rm -rf "$DIR_NAME"
}


get_branch_version "dev"
get_branch_version "main"

echo "Get Versions: OK!"