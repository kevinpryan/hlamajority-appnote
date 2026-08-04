# HLA Majority App Note

## nf-hlamajority: An Automated Nextflow Pipeline for Consensus HLA Genotyping in Neoantigen Prediction Workflows

[![DOI](https://zenodo.org/badge/1157271477.svg)](https://doi.org/10.5281/zenodo.19952935)

Code to generate figures from application note for [nf-hlamajority](https://github.com/kevinpryan/nf-hlamajority/tree/main)

Upload outputs of nf-hlamajority to Zenodo and add scripts to download them

To regenerate the figures from the submitted version of the paper (submitted 01/06/2026):

Clone the repository specifying the branch

```
git clone --recurse-submodules https://github.com/kevinpryan/hlamajority-appnote.git
```

Run `bash download-data.sh` to download the nf-hlamajority output data from Zenodo (2.4 GB)

Run `bash run-all-app-note-thesis.sh` to run the downstream processing and generate the figures from the app note and the thesis chapter

Requirements:

- docker
- nextflow

You will find app note Figures 1D,E combined at `results/app_note/plots/hlamajority-1000genomes-nci-combined-20260529.svg`, and the supplementary figure at `results/app_note/plots/hlamajority-1000genomes-wgs.svg`.

Run the Docker images containing all dependencies to run Rscripts as follows:

```
bash launch-container.sh
```

and go to `http://localhost:8787/` on any web browser to open Rstudio, open the directory `/hlamajority-paper/`
