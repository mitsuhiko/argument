#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPT_DIR/..

NEW_VERSION="${1}"

echo "Bumping version: ${NEW_VERSION}"
perl -pi -e "s/\bargument v.*? /argument v$NEW_VERSION /" README.md
perl -pi -e "s/^version = \".*?\"/version = \"$NEW_VERSION\"/" argument/Cargo.toml
perl -pi -e "s/^version = \".*?\"/version = \"$NEW_VERSION\"/" argument-*/Cargo.toml

cargo check --all
