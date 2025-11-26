#!/usr/bin/env bash
set -uo pipefail

usage() {
  cat <<'USAGE'
Usage: run_rscripts.sh --vendor=NAME --size=NAME --type=DEFAULT|PRIMARY [--runner=R|Rscript]

Runs each R script inside:
  ./public/firstScripts/{type}/{vendor_lower}_{size_lower}
and checks whether inst/<script-base-name>/ was created after a successful run.

Logs results and generates summary.txt under:
  ./public/ResultFirstScripts/{type}/{vendor_lower}_{size_lower}

Examples:
  ./run_rscripts.sh --vendor=OPENAI --size=FLAGSHIP --type=DEFAULT
  ./run_rscripts.sh --vendor=OPENAI --size=LIGHT --type=PRIMARY
USAGE
}

vendor=""
size=""
type_arg=""
runner="R"   # 기본 runner는 R

while [[ $# -gt 0 ]]; do
  case "$1" in
    --vendor=*) vendor="${1#*=}" ;;
    --vendor)   shift; vendor="${1:-}";;
    --size=*)   size="${1#*=}" ;;
    --size)     shift; size="${1:-}";;
    --type=*)   type_arg="${1#*=}" ;;
    --type)     shift; type_arg="${1:-}";;
    --runner=*) runner="${1#*=}" ;;
    --runner)   shift; runner="${1:-}";;
    -h|--help)  usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
  shift
done

if [[ -z "$vendor" || -z "$size" || -z "$type_arg" ]]; then
  echo "❌ vendor, size, type 는 모두 필수입니다." >&2
  usage
  exit 1
fi

to_upper() { printf '%s' "$1" | tr '[:lower:]' '[:upper:]'; }
to_lower() { printf '%s' "$1" | tr '[:upper:]' '[:lower:]'; }

vendor_uc=$(to_upper "$vendor")
size_uc=$(to_upper "$size")
type_uc=$(to_upper "$type_arg")

vendor_lc=$(to_lower "$vendor")
size_lc=$(to_lower "$size")
type_lc=$(to_lower "$type_arg")

# 입력 R 스크립트 위치
input_dir="public/firstScripts/${type_lc}/${vendor_lc}_${size_lc}"

# 결과(로그 + summary) 위치
result_root="public/ResultFirstScripts/${type_lc}/${vendor_lc}_${size_lc}"
log_root="${result_root}/logs"

echo "Input R scripts dir : $input_dir"
echo "Result root         : $result_root"
echo "Log root            : $log_root"
echo "Runner              : $runner"
echo

if [[ ! -d "$input_dir" ]]; then
  echo "❌ Input directory not found: $input_dir" >&2
  exit 1
fi

if [[ ! -d inst ]]; then
  echo "❌ Directory 'inst' not found; expected output folder root." >&2
  exit 1
fi

mkdir -p "$log_root"

overall_rc=0
shopt -s nullglob
success_count=0
fail_count=0
declare -a failed_scripts=()

# input_dir 내의 .R 파일들 수집
scripts=()
while IFS= read -r script_path; do
  scripts+=("$script_path")
done < <(find "$input_dir" -maxdepth 1 -type f -name '*.R' | sort)

if [[ ${#scripts[@]} -eq 0 ]]; then
  echo "❌ No .R files found in $input_dir" >&2
  exit 1
fi

echo "==> Running scripts in $input_dir"
echo

for script in "${scripts[@]}"; do
  base=$(basename "$script" .R)
  expected_dir="inst/${base}"

  # inst/<base>가 미리 있는지 체크
  pre_existing=0
  [[ -d "$expected_dir" ]] && pre_existing=1

  # 없으면 미리 만들어두기도 했었는데, 여기서는 R 스크립트가 생성하는지 확인용으로만 사용
  # 필요하면 아래 주석 해제해서 빈 디렉터리라도 만들어둘 수 있음
  # if [[ $pre_existing -eq 0 ]]; then
  #   mkdir -p "$expected_dir"
  # fi

  log_file="${log_root}/${base}.log"

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
      echo "    ✔ Success: $base"
      echo "    ✔ Success: $base" >> "$log_file"
    else
      ((fail_count++))
      failed_scripts+=("$base")
      echo "    ⚠ Script exited 0 but $expected_dir not found"
      echo "    ⚠ Script exited 0 but $expected_dir not found" >> "$log_file"
      overall_rc=1
    fi
  else
    ((fail_count++))
    failed_scripts+=("$base")
    echo "    ✖ Runner command exited with an error (runner: $runner)"
    echo "    ✖ Runner command exited with an error (runner: $runner)" >> "$log_file"
    overall_rc=1
  fi
done

total=$((success_count + fail_count))
accuracy=$(awk "BEGIN { if ($total > 0) printf \"%.2f\", ($success_count / $total) * 100; else print 0 }")

summary_file="${result_root}/summary.txt"
mkdir -p "$result_root"

{
  echo "===== R Script Execution Summary ====="
  echo "Vendor     : ${vendor_uc}"
  echo "Size       : ${size_uc}"
  echo "Type       : ${type_uc}"
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
  echo "Logs saved under: $log_root"
} | tee "$summary_file"

echo
echo "=== Summary ==="
cat "$summary_file"

exit $overall_rc
