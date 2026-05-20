# read in data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/nci-full-results-lens-compare-hlamajority.Rds and prepare it for nextflow
library(dplyr)
library(tidyr)
library(purrr)
library(tidyr)

setwd("/hlamajority-paper/external/mhc_genotyping/")

# read in the data
results <- readRDS("../../data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/nci-full-results-lens-compare-hlamajority.Rds")
make_scenarios <- function(df, gene, tool_name) {
  
  df %>%
    mutate(
      Gene = gene,
      Tool = tool_name,
      Sample = rownames(df),
      
      # format alleles for NetMHCpan
      HLA_true = paste0("HLA-", gene, allele1_gold_standard, ",HLA-", gene, allele2_gold_standard),
      HLA_mistyped = paste0("HLA-", gene, allele1_tool, ",HLA-", gene, allele2_tool)
    ) %>%
    dplyr::select(Sample, Gene, Tool, HLA_true, HLA_mistyped)
}

df_A <- results$details$A$hlamajority$metrics$gold_standard_vs_tool_incorrect_calls
df_B <- results$details$B$hlamajority$metrics$gold_standard_vs_tool_incorrect_calls
df_C <- results$details$C$hlamajority$metrics$gold_standard_vs_tool_incorrect_calls

scenarios_A <- make_scenarios(df_A, "A", "hlamajority")
scenarios_B <- make_scenarios(df_B, "B", "hlamajority")
scenarios_C <- make_scenarios(df_C, "C", "hlamajority")
scenarios <- rbind(scenarios_A, scenarios_B, scenarios_C)
colnames(scenarios)[2] <- "HLA_Gene"

df_LENS_A <- results$details$A$`LENS-v1.8-consensus`$metrics$gold_standard_vs_tool_incorrect_calls
df_LENS_B <- results$details$B$`LENS-v1.8-consensus`$metrics$gold_standard_vs_tool_incorrect_calls
df_LENS_C <- results$details$C$`LENS-v1.8-consensus`$metrics$gold_standard_vs_tool_incorrect_calls

scenarios_A_LENS <- make_scenarios(df_LENS_A, "A", "LENS-v1.8-consensus")
scenarios_B_LENS <- make_scenarios(df_LENS_B, "B", "LENS-v1.8-consensus")
scenarios_C_LENS <- make_scenarios(df_LENS_C, "C", "LENS-v1.8-consensus")
scenarios_LENS <- rbind(scenarios_A_LENS, scenarios_B_LENS, scenarios_C_LENS)
colnames(scenarios_LENS)[2] <- "HLA_Gene"

scenarios <- rbind(scenarios, scenarios_LENS)
scenarios$Sample <- gsub(pattern = "/", replacement = "-", x = scenarios$Sample)
scenarios$Sample <- gsub(pattern = " ", replacement = "-", x = scenarios$Sample)
write.table(scenarios, file = "../../data/processed/neoantigen-prediction/scenarios.txt", quote = F, row.names = F, sep = "\t")
get_protein_sequence <- function(gene, organism = "human") {
  
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
  
  if (length(fasta) == 0) {
    stop("No UniProt sequence found for gene: ", gene)
  }
  
  seq <- fasta[!grepl("^>", fasta)]
  seq <- paste(seq, collapse = "")
  seq <- gsub("\\*", "", seq)
  
  return(seq)
}


apply_mutation <- function(sequence, mutation) {
  print("sequence...")
  print(sequence)
  # parse mutation
  wt <- substr(mutation, 1, 1)
  pos <- as.numeric(gsub("[^0-9]", "", mutation))
  mut <- substr(mutation, nchar(mutation), nchar(mutation))
  
  seq_vec <- strsplit(sequence, "")[[1]]
  
  if (seq_vec[pos] != wt) {
    warning(paste("WT mismatch at position", pos, "expected", wt, "found", seq_vec[pos]))
  }
  
  seq_vec[pos] <- mut
  print(paste(seq_vec, collapse = ""))
  paste(seq_vec, collapse = "")
}

generate_windows <- function(seq, pos, k = 9) {
  
  half <- floor(k / 2)
  
  start <- max(1, pos - half)
  end   <- min(nchar(seq), pos + half)
  
  substr(seq, start, end)
}

generate_mutant_peptides <- function(seq, position, lengths = 8:11) {
  
  peptides <- list()
  
  for (k in lengths) {
    
    # ONLY windows that include mutation position
    start_min <- max(1, position - k + 1)
    start_max <- min(position, nchar(seq) - k + 1)
    
    starts <- start_min:start_max
    
    for (s in starts) {
      
      peptides[[length(peptides) + 1]] <- data.frame(
        Peptide = substr(seq, s, s + k - 1),
        Length = k,
        Start = s,
        End = s + k - 1,
        Contains_mutation = (s <= position && position <= s + k - 1)
      )
    }
  }
  
  dplyr::bind_rows(peptides)
}

generate_neoantigens <- function(gene, mutation) {
  
  seq <- get_protein_sequence(gene)
  
  if (is.null(seq) || is.na(seq) || seq == "") {
    stop("No sequence for gene: ", gene)
  }
  
  pos <- as.numeric(gsub("[^0-9]", "", mutation))
  
  if (is.na(pos)) {
    stop("Could not parse mutation: ", mutation)
  }
  
  # apply mutation
  mut_seq <- apply_mutation(seq, mutation)
  
  # ONLY peptides that include the mutation position
  wt_pep <- generate_mutant_peptides(seq, pos)
  mut_pep <- generate_mutant_peptides(mut_seq, pos)
  
  # join WT and mutant by identical genomic window
  merged <- inner_join(
    wt_pep %>% dplyr::rename(WT_peptide = Peptide),
    mut_pep %>% dplyr::rename(MUT_peptide = Peptide),
    by = c("Start", "Length", "End")
  )
  
  if (nrow(merged) == 0) {
    return(data.frame())
  }
  
  merged$Gene <- gene
  merged$Mutation <- mutation
  merged$Position <- pos
  
  merged
}

mutation.df <- data.frame(
  gene = "KRAS",
  mutation = c(
               "G12A",
               "G12C",
               "G12D",
               "G12R",
               "G12S",
               "G12V",
               "G13D",
               "G13R",
               "Q61H"
               )
)
mutation.df.nras <- data.frame(
  gene = "NRAS",
  mutation = c(
    "Q61R",
    "Q61H",
    "Q61K"
  )
)
mutation.df.combined <- rbind(mutation.df, mutation.df.nras)

write.csv(mutation.df.combined, file = "../../data/processed/neoantigen-prediction/mutations.csv", row.names = F, quote = F)
samples <- data.frame(sample = rep("all_samples", nrow(mutation.df.combined)))
mutation.df.combined.samples <- cbind.data.frame(samples, mutation.df.combined)
write.csv(mutation.df.combined.samples, file = "../../data/processed/neoantigen-prediction/mutations.csv", row.names = F, quote = F)

head(peps)

neoantigen_df <- mutation.df.combined %>%
  mutate(peptides = map2(gene, mutation, generate_neoantigens)) %>%
  unnest(peptides)

experiment_df <- scenarios %>%
  crossing(neoantigen_df)

experiment_df
