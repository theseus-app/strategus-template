#!/usr/bin/env bash
set -uo pipefail

usage() {
  cat <<'USAGE'
Usage: run_debug_rscripts.sh --vendor=NAME --size=NAME --type=DEFAULT|PRIMARY [--runner=R|Rscript]

Runs each R script inside:
  public/DebugScripts/{type}/{vendor}_{size}
and checks whether inst/<script-base-name>/ was created after a successful run.
Logs results and generates summary.txt under:
  public/ResultDebugScripts/{type}/{vendor}_{size}

Examples:
  ./run_debug_rscripts.sh --vendor=OPENAI --size=FLAGSHIP --type=DEFAULT
  ./run_debug_rscripts.sh --vendor=OPENAI --size=LIGHT --type=PRIMARY
USAGE
}

vendor=""
size=""
type=""
runner="R"   # ✅ 기본 runner는 R

while [[ $# -gt 0 ]]; do
  case "$1" in
    --vendor=*) vendor="${1#*=}" ;;
    --vendor) shift; vendor="${1:-}";;
    --size=*) size="${1#*=}" ;;
    --size) shift; size="${1:-}";;
    --type=*) type="${1#*=}" ;;
    --type) shift; type="${1:-}";;
    --runner=*) runner="${1#*=}" ;;
    --runner) shift; runner="${1:-}";;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
  shift
done

if [[ -z "$vendor" || -z "$size" || -z "$type" ]]; then
  echo "❌ --vendor, --size, --type 는 필수입니다." >&2
  usage
  exit 1
fi

to_lower() { printf '%s' "$1" | tr '[:upper:]' '[:lower:]'; }
to_upper() { printf '%s' "$1" | tr '[:lower:]' '[:upper:]'; }

vendor_filt=$(to_upper "$vendor")
size_filt=$(to_upper "$size")
type_filt=$(to_upper "$type")

vendor_lower=$(to_lower "$vendor_filt")
size_lower=$(to_lower "$size_filt")
type_lower=$(to_lower "$type_filt")

# ✅ 디버그 스크립트가 있는 곳
script_root="public/DebugScripts/${type_lower}/${vendor_lower}_${size_lower}"

# ✅ 실행 결과 및 로그를 남길 곳
result_root="public/ResultDebugScripts/${type_lower}/${vendor_lower}_${size_lower}"
log_dir="${result_root}/logs"

echo "Vendor : $vendor_filt"
echo "Size   : $size_filt"
echo "Type   : $type_filt"
echo "Runner : $runner"
echo "Script dir : $script_root"
echo "Result dir : $result_root"
echo "Log dir    : $log_dir"
echo

if [[ ! -d "$script_root" ]]; then
  echo "❌ Script directory not found: $script_root" >&2
  exit 1
fi

if [[ ! -d inst ]]; then
  echo "Directory 'inst' not found; expected output folder root." >&2
  exit 1
fi

mkdir -p "$log_dir"

overall_rc=0
shopt -s nullglob
success_count=0
fail_count=0
declare -a failed_scripts=()

scripts=()
while IFS= read -r script_path; do
  scripts+=("$script_path")
done < <(find "$script_root" -maxdepth 1 -type f -name '*.R' | sort)

if [[ ${#scripts[@]} -eq 0 ]]; then
  echo "  No .R files found in $script_root"
  exit 1
fi

echo "==> Running debug scripts in $script_root"
echo

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

total=$((success_count + fail_count))
accuracy=$(awk "BEGIN { if ($total > 0) printf \"%.2f\", ($success_count / $total) * 100; else print 0 }")

# ✅ summary.txt를 ResultDebugScripts/{type}/{vendor}_{size} 바로 아래에 저장
summary_dir="$result_root"
mkdir -p "$summary_dir"
summary_file="${summary_dir}/summary.txt"

{
  echo "===== Debug R Script Execution Summary ====="
  echo "Vendor     : ${vendor_filt}"
  echo "Size       : ${size_filt}"
  echo "Type       : ${type_filt}"
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
  echo "Logs saved under: $log_dir"
} | tee "$summary_file"

echo
echo "=== Summary ==="
cat "$summary_file"

exit $overall_rc
