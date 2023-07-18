#!/usr/bin/env bash

# This script is used to draw the cfg of functions in llvm ir using opt and dot.
# Usage: draw_cfg.sh filename.ll

set -e

# Check the number of arguments
if [ $# -ne 1 ]; then
    echo "Usage: draw_cfg.sh filename.ll"
    exit 1
fi

filename=$(realpath $1)
cfg_dir=$(echo $filename | sed 's/\.ll$//g')".cfg"

echo "CFG Output directory: $cfg_dir"

# Create output directory
mkdir -p $cfg_dir

# Draw cfg of each function
pushd $cfg_dir
opt -S -enable-new-pm=0 -dot-cfg $filename > /dev/null
for file in .*.dot; do
    dot -Nfontname=Iosevka -Efontname=Iosevka -Tpng $file -o $(echo $file | sed 's/\.dot$//g')".png"
done
popd

dom_dir=$(echo $filename | sed 's/\.ll$//g')".dom"

echo "DOM Output directory: $dom_dir"

# Create output directory
mkdir -p $dom_dir

# Draw dom of each function
pushd $dom_dir
opt -S -enable-new-pm=0 -dot-dom $filename > /dev/null
for file in *.dot; do
    dot -Nfontname=Iosevka -Efontname=Iosevka -Tpng $file -o $(echo $file | sed 's/\.dot$//g')".png"
done
popd