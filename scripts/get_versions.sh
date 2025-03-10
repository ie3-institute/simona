#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")/.."

REPO_URL=$(git config --get remote.origin.url)
export REPO_URL
echo "REPO_URL=$REPO_URL" >> $GITHUB_ENV

echo "Fetching current version of PR..."
PR_VERSION=$(./gradlew -q currentVersion)
echo "PR_VERSION=$PR_VERSION"
echo "export PR_VERSION=$PR_VERSION" >> versions.env
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
    echo "export ${BRANCH_NAME^^}_VERSION=$BRANCH_VERSION" >> versions.env
    echo "${BRANCH_NAME^^}_VERSION=$BRANCH_VERSION" >> "$GITHUB_ENV"

    rm -rf "$DIR_NAME"
}


get_branch_version "dev"
get_branch_version "main"

echo "Get Versions: OK!"
