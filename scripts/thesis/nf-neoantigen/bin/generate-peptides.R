#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)

sample   <- args[1]
gene     <- args[2]
mutation <- args[3]
out_file <- args[4]

get_protein_sequence <- function(gene) {
  base_url <- "https://rest.uniprot.org/uniprotkb/search"
  
  query <- paste0(
    "gene:", gene,
    "+AND+organism_id:9606+AND+reviewed:true"
  )
  
  url <- paste0(
    base_url,
    "?query=", URLencode(query),
    "&format=fasta"
  )
  
  fasta <- readLines(url, warn = FALSE)
  seq <- fasta[!grepl("^>", fasta)]
  seq <- paste(seq, collapse = "")
  gsub("\\*", "", seq)
}

seq <- get_protein_sequence(gene)

wt   <- substr(mutation, 1, 1)
pos  <- as.numeric(gsub("[^0-9]", "", mutation))
mut  <- substr(mutation, nchar(mutation), nchar(mutation))

seq_vec <- strsplit(seq, "")[[1]]

if (is.na(pos)) stop("Bad mutation: ", mutation)
if (pos > length(seq_vec)) stop("Position out of bounds")
if (seq_vec[pos] != wt) warning("WT mismatch for ", gene, " ", mutation)

seq_vec[pos] <- mut
mut_seq <- paste(seq_vec, collapse = "")

generate_windows <- function(sequence, pos, lengths = 8:11) {
  
  seq_vec <- strsplit(sequence, "")[[1]]
  n <- length(seq_vec)
  
  out <- list()
  
  for (k in lengths) {
    starts <- (pos - k + 1):pos
    starts <- starts[starts > 0 & (starts + k - 1) <= n]
    
    for (s in starts) {
      pep <- paste(seq_vec[s:(s + k - 1)], collapse = "")
      
      out[[length(out) + 1]] <- data.frame(
        peptide = pep,
        length  = k,
        start   = s,
        end     = s + k - 1
      )
    }
  }
  
  dplyr::distinct(do.call(rbind, out))
}

wt_df  <- generate_windows(seq, pos)
mut_df <- generate_windows(mut_seq, pos)

peptides <- wt_df %>%
  inner_join(
    mut_df,
    by = c("length", "start", "end"),
    suffix = c("_wt", "_mut")
  ) %>%
  mutate(
    sample = sample,
    gene = gene,
    mutation = mutation
  ) %>%
  select(sample, gene, mutation,
         peptide_wt, peptide_mut,
         length, start, end)

write.table(
  peptides,
  file = out_file,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
