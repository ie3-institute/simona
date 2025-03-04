#!/usr/bin/env bash
set -euo pipefail

if [ -z "${branch_name:-}" ]; then
  echo "Error: branch_name variable is not set."
  exit 1
fi


pattern_dev='^(developer|develop|dev)$'
pattern_release='.*rel/.*'
pattern_dependabot='^dependabot/.*'
pattern_hotfix='.*hotfix/.*'
pattern_main='.*main'
pattern_feature='^[a-z]{2}/#[0-9]+(-.+)?$'

branch_type="unknown"

if [[ "$branch_name" =~ $pattern_dev ]]; then
  branch_type="dev"
elif [[ "$branch_name" =~ $pattern_release ]]; then
  branch_type="release"
elif [[ "$branch_name" =~ $pattern_dependabot ]]; then
  branch_type="dependabot"
elif [[ "$branch_name" =~ $pattern_hotfix ]]; then
  branch_type="hotfix"
elif [[ "$branch_name" =~ $pattern_main ]]; then
  branch_type="main"
elif [[ "$branch_name" =~ $pattern_feature ]]; then
  branch_type="feature"
else
  echo "Error:'$branch_name' does not match any pattern."
  exit 1
fi

echo "========================="
echo "Branch type: $branch_type"
echo "branch_type=$branch_type" >> "$GITHUB_ENV"
echo "========================="
