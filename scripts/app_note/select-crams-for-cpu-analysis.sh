#!/bin/bash
set -e
awk '{gsub("G","",$1); print $1, $2}' ../../data/raw/1000-genomes/filesizes-1000genomes-crams.txt \
  | sort -k1,1n > sizes_numeric_sorted.txt

split -n l/5 sizes_numeric_sorted.txt bin_

for f in bin_*; do
    shuf --random-source=<(yes 42) "$f" | head -n 4
done > stratified_20_samples.txt

awk '{print $2}' stratified_20_samples.txt > ../../data/processed/results/hlamajority/1000genomes-all-samples/selected_crams.txt

rm bin_* sizes_numeric_sorted.txt stratified_20_samples.txt
