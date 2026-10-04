#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPT_DIR/..

# dependencies need to be published before their dependents
cargo publish -p argument-parser
cargo publish -p argument
cargo publish -p argument-completions
cargo publish -p argument-mangen
