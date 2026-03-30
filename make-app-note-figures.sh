docker_prefix="docker run --rm -v $(pwd):/hlamajority-paper/ -w /hlamajority-paper/ kevinr9525/rocker-bioconductor:hlamajority-appnote"
#$docker_prefix Rscript scripts/app_note/make-plots-hlamajority-1000genomes-appnote.R
#$docker_prefix Rscript scripts/app_note/make-plots-nci60-hlamajority-appnote.R
$docker_prefix Rscript scripts/app_note/make-plots-hlamajority-1000genomes-cell-lines-appnote.R
