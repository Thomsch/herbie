#!/bin/bash

# exit immediately upon first error
set -e

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# Constants
BRANCHES=branches.config
SEED=2023073
THREADS=4
OUTPUT=reports

# for some reason bad things happen if this occurs in parallel
function fetch_branch {
  branch="$1"
  folder="$2"

  out="$OUTPUT/$folder"
  build="$out/build"

  # Make output directory
  mkdir -p $out

  # Clear build directory
  rm -rf "$build"
  mkdir -p "$build"

  # Copy essential files
  cp -r $MYDIR/.git "$build/"
  cp -r $MYDIR/bench "$build/"
  cp -r $MYDIR/infra "$build/"
  cp -r $MYDIR/src "$build/"
  cp -r $MYDIR/Makefile "$build/"

  pushd $build

  # Branch checkout
  git checkout $branch

  # Temporary patch
  if [[ "$branch" == "using-ruler-nightlies" ]]; then
    sed -i 's/main/update-trig/g' src/syntax/rules.rkt
  fi

  # Weird build
  raco make src/syntax/*.rkt
  raco make src/web/*.rkt
  raco make src/*.rkt

  # Trim benchmark set
  rm bench/demo.fpcore \
     bench/haskell.fpcore \
     bench/regression.fpcore \
     bench/tutorial.fpcore

  popd
}

function do_branch {
  branch="$1"
  folder="$2"

  shift
  shift
  flags="$@"

  out="$OUTPUT/$folder"
  build="$out/build"

  pushd $build

  RECURSE=1 bash infra/run.sh \
    bench \
    "../graphs" \
    --seed $SEED \
    --threads $THREADS \
    $flags

  popd
}

# Actual start

mkdir -p "$OUTPUT"

if [ -z "$NO_BUILD" ]; then
  while read branch; do
    fetch_branch $branch
  done < $BRANCHES
fi

if [ -z "$PARALLEL_SEEDS" ]; then
  while read branch; do
    do_branch $branch
  done < $BRANCHES
else
  # conditionally use parallel
  #
  # Note that Herbie can already use up to # of benchmarks cores,
  # so this probably only makes sense if you have PARALLEL_SEEDS
  # set to something less than # of cores divided by # of benchmarks,
  # i.e., you have a lot of cores. We're not at all careful to get
  # solid timing numbers, but going higher any than that will make
  # any time measurements even less meaningful.

  source $(which env_parallel.bash)
  env_parallel --record-env

  cat $BRANCHES \
    | env_parallel \
        --env _ \
        --jobs "$PARALLEL_SEEDS" \
        --halt now,fail=1 \
        do_branch
fi
