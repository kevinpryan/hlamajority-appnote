#!/bin/bash
set -e
# this is a master script for reproducing the results in the paper. It will run all the scripts in the correct order.
# requirements:
# output of nf-hlamajority: data/raw/1000-genomes/benchmark-1000genomes-nfhlamajority-local-update-db-exclude-trim-majority-all-samples/ and data/raw/cell-lines/benchmark-cell-lines-all-kourami-3-63-0-majority-vote/ 
# dependencies: docker, bash, md5sum
# command to put before Rscripts to make sure we use the docker container
docker_prefix="docker run --rm   -u $(id -u):$(id -g) -v $(pwd):/hlamajority-paper/ -w /hlamajority-paper/ kevinr9525/rocker-bioconductor:hlamajority-appnote"
# check if the directory data/raw/cell-lines-after-polysolver-change/ exists, if not, chances are the user did not run download-data.sh, so we will run it here 

#######
# DOWNLOAD DATA IF NOT PRESENT
#######

if [ ! -d data/raw/cell-lines-after-polysolver-change/ ]; then
echo "Directory data/raw/cell-lines-after-polysolver-change/ does not exist. Running download-data.sh to download the necessary data..."
bash download-data.sh
else 
echo "Directory data/raw/cell-lines-after-polysolver-change/ exists. Assuming the necessary data is already downloaded."
fi

#####
# PREPARE GOLD STANDARD DATA
#####
echo "Running all scripts in the correct order to reproduce the results in the paper..."
mkdir -p data/processed/1000-genomes/majority/
# downloading the gold standard data for the 1000 genomes samples
if [ ! -f external/mhc_genotyping/downloads/1kg/gold_standard/20140702_hla_diversity.txt ]; then
echo "Running script 1: download_gourraud.sh"
bash external/mhc_genotyping/scripts/downloads/1kg/gold_standard/download_gourraud.sh
fi
### run script to combine Gourraud and DeBakker gold standard data
if [ ! -f external/mhc_genotyping/temp/GourroudAndDeBakker_gold_standard.txt ]; then
echo "Running script 2: combine_1kg_gold_standard.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/combine_1kg_gold_standard.R
fi
if [ ! -f external/mhc_genotyping/data/ggroup_mapping.rds ]; then
echo "running script to create group mapping for 1000 Genomes samples: create_ggroup_mapping.R"
mkdir -p external/mhc_genotyping/data/
$docker_prefix Rscript external/mhc_genotyping/scripts/create_ggroup_mapping.R
fi
if [ ! -f external/mhc_genotyping/data/gold_standard_1kg.rds ]; then
echo "Running script 3: process_goldstandard_1kg.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/process_goldstandard_1kg.R
fi

######
# EVALUATE 1000 GENOMES WES PREDICTIONS
######

if [ ! -f ./data/processed/1000-genomes/majority/1000-genomes-full-stats-hlamajority-majority-vote.csv ]; then
echo "Running script 4: evaluate_predictions_1000genomes_all_samples.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_1000genomes_all_samples.R
fi

######
# PREPARE GOLD STANDARD DATA FOR CELL LINES
#####

## run all for cell lines
if [ ! -f external/mhc_genotyping/downloads/HLA_nomenclature/Allelelist.3440.txt ]; then
echo "Running script 5 for downloading HLA nomenclature: download.sh"
bash external/mhc_genotyping/downloads/HLA_nomenclature/download.sh
fi
if [[ ! -f external/mhc_genotyping/downloads/pub/adams_2005/MHC_I_calls.html || ! -f external/mhc_genotyping/downloads/pub/adams_2005/MHC_II_calls.html ]]; then
echo "Running script 6 for downloading Adams 2005 data: download.sh"
bash external/mhc_genotyping/scripts/downloads/adams_2005/download.sh
fi
if [ ! -f external/mhc_genotyping/downloads/pub/adams_2005/hla_calls.rds ]; then
echo "Running script 7 for parsing Adams 2005 data: download_parse_tables.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/downloads/adams_2005/download_parse_tables.R
fi
if [ ! -f external/mhc_genotyping/data/sample_names_nci60_srx.rds ]; then
echo "Running script 8 for creating sample names NCI-60 data: create_sample_names_nci60.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/create_sample_names_nci60.R
fi

if [ ! -f external/mhc_genotyping/data/gold_standard_nci60.rds ]; then
echo "Running script 9 for processing gold standard NCI-60 data: process_goldstandard_nci60.R"
$docker_prefix Rscript external/mhc_genotyping/scripts/process_goldstandard_nci60.R
fi

#if [ ! -f data/processed/cell-lines/majority/nci-full-stats-hlamajority-majority-vote.csv ]; then
#echo "Running script 10 for evaluating original predictions on NCI-60 data: evaluate_predictions_nci60_20260225.R"
#$docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_nci60_20260225.R
#fi

#if [ ! -f data/processed/cell-lines-v2/majority/nci-full-stats-hlamajority-majority-vote.csv ]; then
#echo "Running script  for evaluating predictions on NCI-60 data: evaluate_predictions_nci60_20260428.R"
#mkdir -p data/processed/cell-lines-v2/majority/
#$docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_nci60_20260428.R
#fi

######
# EVALUATE CELL LINE PREDICTIONS BEFORE AND AFTER POLYSOLVER CHANGE
######

if [ ! -f data/processed/cell-lines-before-polysolver-change/majority/nci-full-stats-hlamajority-majority-vote.csv ]; then
echo "Running script  for evaluating predictions on NCI-60 data: evaluate_predictions_nci60_before_polysolver_change_20260429.R"
mkdir -p data/processed/cell-lines-before-polysolver-change/majority/
$docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_nci60_before_polysolver_change_20260429.R
fi

if [ ! -f data/processed/cell-lines-after-polysolver-change/majority/nci-full-stats-hlamajority-majority-vote.csv ]; then
echo "Running script  for evaluating predictions on NCI-60 data: evaluate_predictions_nci60_after_polysolver_change_20260429.R"
mkdir -p data/processed/cell-lines-after-polysolver-change/majority/
$docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_nci60_after_polysolver_change_20260429.R
fi

#####
# ADDITIONAL ANALYSES FOR APP NOTE
#####
echo "Running script 11 for calculating mean file size of 1000 Genomes CRAMs"
$docker_prefix Rscript scripts/app_note/get-average-filesize-crams.R
echo "Running script 12 for selecting CRAMs for CPU analysis"
cd scripts/app_note/
bash select-crams-for-cpu-analysis.sh
cd ../..
echo "Running script 13 for parsing Nextflow execution trace"
#$docker_prefix Rscript scripts/app_note/parse-nextflow-execution-trace-appnote.R
$docker_prefix Rscript scripts/app_note/parse-nextflow-execution-trace-appnote-20260429.R

#mkdir -p data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/combined-results/
#if [ ! -f data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/combined-results/lens-v1.2-dev-v1.8-results-standardised.csv ]; then
#echo "Running script for aggregating LENS results"
#$docker_prefix Rscript external/mhc_genotyping/scripts/aggregate_results_nci60_lens.R
#fi

#####
#  SELECT SAMPLES FOR WGS BENCHMARK AND EVALUATE RESULTS
####

echo "running script for selecting samples for WGS benchmark"
$docker_prefix Rscript scripts/app_note/identify-failed-samples-wes-decide-wgs-samples.R

echo "running script for evaluating WGS benchmark results: evaluate_predictions_1000genomes_wgs.R"
mkdir -p  data/processed/1000-genomes/wgs-30x-149samples-majority/
$docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_1000genomes_wgs.R

####
#   EVALUTATE RESULTS CELL LINES LENS
####

if [ ! -f data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/combined-results/lens-v1.2-dev-v1.8-results-standardised.csv ]; then
    mkdir -p data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/combined-results/
    echo "running script for aggregating LENS results on NCI-60 cell lines: aggregate_results_nci60_lens.R"
    $docker_prefix Rscript external/mhc_genotyping/scripts/aggregate_results_nci60_lens.R
fi

if [ ! -f data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/nci-full-stats-lens-compare-hlamajority.csv ]; then
    echo "running script for evaluating LENS results on NCI-60 cell lines: evaluate_predictions_nci60_lens.R"
    $docker_prefix Rscript external/mhc_genotyping/scripts/evaluate_predictions_nci60_lens.R
fi
####
# MAKE THESIS FIGURES DEPTH
####

if [ ! -f results/thesis/plots/hlamajority-1000genomes-depth-20260527.svg ]; then
mkdir -p results/thesis/plots/
mkdir -p results/thesis/figures/
    echo "running script for making thesis figure: make-plots-thesis-depth.R"
    $docker_prefix Rscript scripts/thesis/make-plots-thesis-depth.R
fi

if [ ! -f results/thesis/plots/hlamajority-1000genomes-nci-combined-20260525.svg ]; then
echo "running script for making thesis figure: make-plots-thesis-benchmark-results-hlamajority.R"
mkdir -p results/presentation
$docker_prefix Rscript scripts/thesis/make-plots-thesis-benchmark-results-hlamajority.R
fi

if [ ! -f results/thesis/plots/counts-error-types-hlamajority-1000genomes-nci-combined-20260527.svg ]; then
echo "running script for making failure mode plots: make-plots-failure-modes-hlamajority.R"
$docker_prefix Rscript scripts/thesis/make-plots-failure-modes-hlamajority.R
fi

####
#  CARRY OUT RAS MUTATION ANALYSIS
####
####

if [ ! -f data/processed/neoantigen-prediction/mutations-without-nras-q61h.csv ]; then
echo "running script for prepping experiment data: prep-experiment-data-for-nextflow-include-optitype-rna.R"
mkdir -p data/processed/neoantigen-prediction/
$docker_prefix Rscript scripts/thesis/prep-experiment-data-for-nextflow-include-optitype-rna.R
fi

echo "running nf-neoantigen"
cd scripts/thesis/nf-neoantigen/
nextflow run main.nf -profile docker
cd ../../../

if [ ! -f results/thesis/figures/neoantigen-prediction-errors-add-optitype-rna-20260604.pdf ]; then
echo "running script for making neoantigen prediction error plots: make-plots-compare-optitype-rna-lens-hlamajority.R"
$docker_prefix Rscript scripts/thesis/make-plots-compare-optitype-rna-lens-hlamajority.R
fi

if [ ! -f results/app_note/plots/hlamajority-1000genomes-nci-combined-20260529.svg ]; then
echo "running script for making app note plots: make-plots-hlamajority-1000genomes-cell-lines-appnote.R"
$docker_prefix Rscript scripts/app_note/make-plots-hlamajority-1000genomes-cell-lines-appnote.R
fi

if [ ! -f results/app_note/plots/hlamajority-1000genomes-wgs.svg ]; then
echo "running script for making app note supplementary plot: make-plots-hlamajority-wgs-appnote.R"
$docker_prefix Rscript scripts/app_note/make-plots-hlamajority-wgs-appnote.R
fi

