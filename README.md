# HLA Majority App Note

TODO: upload outputs of nf-hlamajority to Zenodo and add scripts to download them

To regenerate the figures:

Clone the repository

```
git clone --recurse-submodules https://github.com/kevinpryan/hlamajority-appnote.git
```

Download the data from Zenodo (approx. 2.3 GB)

```
bash download-data.sh
```

Run `run-all.sh` to run the downstream processing requried to generate figures

All Rscripts are run through a Docker image - Docker is required to run this.

```
bash run-all.sh
```

Generate figures

```
bash make-app-note-figures.sh
```

Run the Docker images containing all dependencies to run Rscripts as follows:

```
bash launch-container.sh
```

and go to `http://localhost:8787/` on any web browser to open Rstudio
