#!/usr/bin/env bash
set -euo pipefail

cd -- "$(dirname -- "$(realpath -- "${BASH_SOURCE[0]}")")"

accept=0
if [ "$1" = "--accept" ]; then
	accept=1
	shift
fi

elf=$1
golddir=$2
runner=$3

# FIXME make configurable
STRUCTTAGS=tests/struct_tags.json
OUTPUT_TYPES=(
    gperf
    validation
    kobj-types
    kobj-otype
    kobj-size
)

elf_base=$(basename -- "$elf")

workdir=$(mktemp -d)
echo "Testing $elf in $workdir"
cleanup() {
	rm -rf "$workdir"
}
trap cleanup EXIT

output_args=()
for outtype in "${OUTPUT_TYPES[@]}"; do
	output_args+=("--$outtype-output" "$workdir/$outtype")
done
"$runner" \
	--include-subsystem-list "$STRUCTTAGS" \
	--kernel "$elf" \
	"${output_args[@]}"

if [ "$accept" -eq 1 ]; then
	mkdir "$golddir"
fi

all_good=1
for outtype in "${OUTPUT_TYPES[@]}"; do
	outfile=$workdir/$outtype
	golden=$golddir/$outtype
	if ! cmp "$outfile" "$golden"; then
		if [ "$accept" -eq 1 ]; then
			echo "Accepting changed file $outfile as new golden file at $golden"
			cp "$outfile" "$golden"
		else
			all_good=0
		fi
	fi
done

if [ "$all_good" -eq 1 ]; then
	echo "All outputs correct"
else
	exit 1
fi
