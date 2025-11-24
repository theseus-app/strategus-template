#!/usr/bin/env bash
set -uo pipefail

usage() {
  cat <<'USAGE'
Usage: run_debug_rscripts.sh [--vendor=NAME] [--size=NAME] [--runner=R|Rscript]

Runs each R script inside DEBUG_RScripts_<VENDOR>_<SIZE> directories and checks
whether inst/<script-base-name>/ was created after a successful run.
Logs results and generates summary.txt under /log/DEBUG_RScripts_<VENDOR>_<SIZE>.

Examples:
  ./run_debug_rscripts.sh --vendor=OPENAI --size=FLAGSHIP
  ./run_debug_rscripts.sh --vendor=OPENAI
  ./run_debug_rscripts.sh --size=LIGHT
USAGE
}

vendor=""
size=""
runner="Rscript"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --vendor=*) vendor="${1#*=}" ;;
    --vendor) shift; vendor="${1:-}";;
    --size=*) size="${1#*=}" ;;
    --size) shift; size="${1:-}";;
    --runner=*) runner="${1#*=}" ;;
    --runner) shift; runner="${1:-}";;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
  shift
done

to_upper() { printf '%s' "$1" | tr '[:lower:]' '[:upper:]'; }
vendor_filt=$(to_upper "$vendor")
size_filt=$(to_upper "$size")

declare -a targets=()

collect_targets() {
  local pattern=$1
  while IFS= read -r line; do
    line=${line#./}
    targets+=("$line")
  done < <(find . -maxdepth 1 -type d -name "$pattern" | sort)
}

# 🔹 DEBUG_RScripts_* 를 대상으로 찾도록 변경
if [[ -n "$vendor_filt" && -n "$size_filt" ]]; then
  targets=("DEBUG_RScripts_${vendor_filt}_${size_filt}")
elif [[ -n "$vendor_filt" ]]; then
  collect_targets "DEBUG_RScripts_${vendor_filt}_*"
elif [[ -n "$size_filt" ]]; then
  collect_targets "DEBUG_RScripts_*_${size_filt}"
else
  collect_targets "DEBUG_RScripts_*"
fi

if [[ ${#targets[@]} -eq 0 ]]; then
  echo "No matching DEBUG_RScripts_<VENDOR>_<SIZE> directories found." >&2
  exit 1
fi

if [[ ! -d inst ]]; then
  echo "Directory 'inst' not found; expected output folder root." >&2
  exit 1
fi

overall_rc=0
shopt -s nullglob
success_count=0
fail_count=0
declare -a failed_scripts=()

for dir in "${targets[@]}"; do
  dir=${dir#./}
  log_dir="log/${dir}"
  mkdir -p "$log_dir"

  echo "==> Running scripts in $dir"
  echo "Log directory: $log_dir"

  scripts=()
  while IFS= read -r script_path; do
    scripts+=("$script_path")
  done < <(find "$dir" -maxdepth 1 -type f -name '*.R' | sort)

  if [[ ${#scripts[@]} -eq 0 ]]; then
    echo "  No .R files found in $dir"
    overall_rc=1
    continue
  fi

  for script in "${scripts[@]}"; do
    base=$(basename "$script" .R)
    expected_dir="inst/${base}"
    pre_existing=0
    [[ -d "$expected_dir" ]] && pre_existing=1
    pre_created=0
    [[ $pre_existing -eq 0 ]] && mkdir -p "$expected_dir" && pre_created=1

    log_file="${log_dir}/${base}.log"

    echo "  → Running $(basename "$script") | log: $log_file"

    if [[ "$runner" == "R" ]]; then
      cmd_output=$(R --no-save --quiet --slave -f "$script" 2>&1)
      cmd_status=$?
    else
      cmd_output=$("$runner" "$script" 2>&1)
      cmd_status=$?
    fi

    echo "$cmd_output" > "$log_file"

    if [[ $cmd_status -eq 0 ]]; then
      if [[ -d "$expected_dir" ]]; then
        ((success_count++))
        echo "    ✔ Success: $base" | tee -a "$log_file"
      else
        ((fail_count++))
        failed_scripts+=("$base")
        echo "    ⚠ Script succeeded but $expected_dir not found" | tee -a "$log_file"
        overall_rc=1
      fi
    else
      ((fail_count++))
      failed_scripts+=("$base")
      echo "    ✖ Runner command exited with an error (runner: $runner)" | tee -a "$log_file"
      overall_rc=1
    fi
  done
done

total=$((success_count + fail_count))
accuracy=$(awk "BEGIN { if ($total > 0) printf \"%.2f\", ($success_count / $total) * 100; else print 0 }")

# 🔹 summary도 DEBUG_RScripts_* 기준으로
summary_dir="log/DEBUG_RScripts_${vendor_filt}_${size_filt}"
mkdir -p "$summary_dir"
summary_file="${summary_dir}/summary.txt"

{
  echo "===== R Script Execution Summary ====="
  echo "Vendor     : ${vendor_filt:-ALL}"
  echo "Size       : ${size_filt:-ALL}"
  echo "Runner     : ${runner}"
  echo "--------------------------------------"
  echo "Total scripts : $total"
  echo "Success count : $success_count"
  echo "Fail count    : $fail_count"
  echo "Accuracy      : ${accuracy}%"
  echo
  if [[ ${#failed_scripts[@]} -gt 0 ]]; then
    echo "❌ Failed Scripts:"
    for f in "${failed_scripts[@]}"; do
      echo "  - $f"
    done
  else
    echo "✅ All scripts executed successfully."
  fi
  echo "--------------------------------------"
  echo "Logs saved under: $summary_dir"
} | tee "$summary_file"

echo
echo "=== Summary ==="
cat "$summary_file"

exit $overall_rc
