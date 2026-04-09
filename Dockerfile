FROM rocker/tidyverse:4.4

ENV DEBIAN_FRONTEND=noninteractive

RUN install2.r \
rmarkdown \
DT \
ggpubr \
pheatmap \
pvclust \
circlize \
littler \
gplots \
here \
ggpmisc \
gt \
Hmisc \
BiocManager \
UpSetR \
dendextend \
hrbrthemes \
optparse \
readr \
ggthemes \
vroom \
stringr \
data.table \
tibble \
box \
svglite \
fuzzyjoin

RUN sudo apt-get update -y && sudo apt-get install -y --no-install-recommends \
    libglpk-dev \
    libbz2-dev \
    libproj-dev \
    libgdal-dev \
    # Clean up the apt cache to reduce image size
    && sudo rm -rf /var/lib/apt/lists/*

#RUN sudo apt-get update -y && sudo apt-get install -y libglpk-dev libbz2-dev libproj-dev libgdal-dev

#RUN R -e "install.packages(c('proj4', 'ggalt'),dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN Rscript -e "BiocManager::install(c('Biostrings', 'pwalign'))"
#RUN Rscript -e "BiocManager::install(c('maftools', 'biomaRt', 'PCAtools', 'GSVA', 'edgeR', 'GSEABase', 'org.Hs.eg.db', 'rhdf5', 'GOSemSim', 'ComplexHeatmap', 'fgsea', 'clusterProfiler', 'hexbin', 'enrichplot', 'ensembldb', 'vcfR', 'geneplotter', 'goseq', 'liftOver', 'BSgenome', 'Rsamtools', 'GenomicAlignments', 'rtracklayer', 'VariantAnnotation', 'MutationalPatterns'))"

#RUN R -e "options(timeout = 600); remotes::install_github('Townsend-Lab-Yale/cancereffectsizeR@*release', upgrade = 'never', ask = FALSE)" && \
#    R -e "options(timeout = 600); remotes::install_github('Townsend-Lab-Yale/ces.refset.hg38@*release', upgrade = 'never', ask = FALSE')"
