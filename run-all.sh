#!/bin/bash
set -e
# this is a master script for reproducing the results in the paper. It will run all the scripts in the correct order.
# requirements:
# output of nf-hlamajority: data/raw/1000-genomes/benchmark-1000genomes-nfhlamajority-local-update-db-exclude-trim-majority-all-samples/ and data/raw/cell-lines/benchmark-cell-lines-all-kourami-3-63-0-majority-vote/ 
# dependencies: docker, bash
# command to put before Rscripts to make sure we use the docker container
docker_prefix="docker run --rm -v $(pwd):/hlamajority-paper/ -w /hlamajority-paper/ kevinr9525/rocker-bioconductor:hlamajority-appnote"
echo "Running all scripts in the correct order to reproduce the results in the paper..."
# downloading the gold standard data for the 1000 genomes samples
echo "Running script 1: download_gourraud.sh"
bash external/mhc_genotyping/scripts/downloads/1kg/gold_standard/download_gourraud.sh
### run script to combine Gourraud and DeBakker gold standard data
echo "Running script 2: combine_1kg_gold_standard.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/combine_1kg_gold_standard.R
echo "Running script 3: process_goldstandard_1kg.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/process_goldstandard_1kg.R
echo "Running script 4: evaluate_predictions_1000genomes_all_samples.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_1000genomes_all_samples.R
## run all for cell lines
echo "Running script 5 for downloading HLA nomenclature: download.sh"
bash external/mhc_genotyping/downloads/HLA_nomenclature/download.sh
echo "Running script 6 for downloading Adams 2005 data: download.sh"
bash external/mhc_genotyping/scripts/downloads/adams_2005/download.sh
echo "Running script 7 for parsing Adams 2005 data: download_parse_tables.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/downloads/adams_2005/download_parse_tables.R
echo "Running script 8 for creating sample names NCI-60 data: create_sample_names_nci60.R"
Rscript external/mhc_genotyping/scripts/create_sample_names_nci60.R
echo "Running script 9 for processing gold standard NCI-60 data: process_goldstandard_nci60.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/process_goldstandard_nci60.R
echo "Running script 10 for evaluating predictions on NCI-60 data: evaluate_predictions_nci60_20260225.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_nci60_20260225.R
echo "Running script 11 for calculating mean file size of 1000 Genomes CRAMs"
$docker_prefix Rscript scripts/app_note/get-average-filesize-crams.R
echo "Running script 12 for selecting CRAMs for CPU analysis"
cd scripts/app_note/
bash select-crams-for-cpu-analysis.sh
cd ../..
echo "Running script 13 for parsing Nextflow execution trace"
$docker_prefix Rscript parse-nextflow-execution-trace-appnote.R
