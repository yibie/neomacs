#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
expected="/nix/store/hql3zwz5b4ywd2qwx8jssp4dyb7nx4cb-emacs-30.2/bin/emacs"

check_script_path() {
  local file="$1"
  local resolved

  resolved="$(awk -F'"' '/^hardcoded_oracle_emacs=/{print $2; exit}' "$file")"
  if [[ -z "$resolved" ]]; then
    echo "missing hardcoded_oracle_emacs in $file" >&2
    exit 1
  fi

  if [[ "$resolved" != "$expected" ]]; then
    echo "hardcoded oracle mismatch in $file" >&2
    echo "  expected: $expected" >&2
    echo "  actual:   $resolved" >&2
    exit 1
  fi
}

check_script_path "$script_dir/run-oracle.sh"
check_script_path "$script_dir/run-ert-allowlist.sh"

echo "oracle default path checks passed"
