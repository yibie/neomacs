#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <forms-file>" >&2
  exit 2
fi

forms_file="$1"
if [[ ! -f "$forms_file" ]]; then
  echo "forms file not found: $forms_file" >&2
  exit 2
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"

cargo run \
  --manifest-path "$repo_root/rust/neovm-core/Cargo.toml" \
  --example compat_runner \
  -- "$forms_file"
