# HLA Majority App Note

## nf-hlamajority: An Automated Nextflow Pipeline for Consensus HLA Genotyping in Neoantigen Prediction Workflows

Code to generate figures from application note for [nf-hlamajority](https://github.com/kevinpryan/nf-hlamajority/tree/main)

Upload outputs of nf-hlamajority to Zenodo and add scripts to download them

To regenerate the figures:

Clone the repository

```
git clone --recurse-submodules https://github.com/kevinpryan/hlamajority-appnote.git
```

Run `run-all.sh` to download the nf-hlamajority output data from Zenodo (2.2 GB) and run the downstream processing requried to generate figures

All Rscripts are run through a Docker image - Docker is required to run this.

```
bash run-all.sh
```

Now generate figures

```
bash make-app-note-figures.sh
```

You will find Figures 1D,E combined at `results/app_note/plots/hlamajority-1000genomes-nci-combined-20260529.svg`


Run the Docker images containing all dependencies to run Rscripts as follows:

```
bash launch-container.sh
```

and go to `http://localhost:8787/` on any web browser to open Rstudio, open the directory `/hlamajority-paper/`

TODO: clean up old versions of figures and plots
