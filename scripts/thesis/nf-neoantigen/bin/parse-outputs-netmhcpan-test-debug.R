#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

print("dir script is in...")
print(getwd())
args <- commandArgs(trailingOnly = TRUE)
n_args <- length(args)
sample   <- args[1]
#hla_gene     <- args[2]
mutation_gene <- args[2]
mutation <- args[3]
files     <- args[4:(n_args - 2)]
scenarios <- args[n_args - 1]
peptides  <- args[n_args]

if (length(args) < 6){
  stop("incorrect number or arguments")
}

extract_field <- function(x, key) {
  stringr::str_match(
    x,
    paste0(key, "=([^=]+?)(?= \\w+=|$)")
  )[,2]
}

parse_netmhcpan <- function(file) {
  print("file...")
  print(file)
  lines <- readLines(file)
  # keep only lines that look like data rows
  data_lines <- lines[grepl("^\\s*\\d+\\s+HLA", lines)]
  data_lines <- data_lines %>%
    stringr::str_replace("\\s+<=\\s*(WB|SB)\\s*$", "")
  #print(data_lines)
  metadata_line <- lines[grepl("^TYPE=", lines)]
  print("metadata_line")
  print(metadata_line)
  df <- read.table(
    text = data_lines,
    header = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE
  )
  #print(head(df))
  colnames(df) <- c(
    "Pos", "HLA", "Peptide", "Core",
    "Of", "Gp", "Gl", "Ip", "Il",
    "Icore", "Identity",
    "Score_EL", "Rank_EL", "Score_BA", "Rank_BA", "Aff_nM_BindLevel"
  )
  type     <- extract_field(metadata_line, "TYPE")
  print("type...")
  print(type)
  sample   <- extract_field(metadata_line, "SAMPLE")
  print("sample...")
  print(sample)
  gene     <- extract_field(metadata_line, "GENE")
  mutation <- extract_field(metadata_line, "MUT")
  tool     <- extract_field(metadata_line, "TOOL")
  df <- df %>%
    dplyr::select(HLA, Peptide, Score_EL, Rank_EL, Aff_nM_BindLevel, Rank_BA) %>%
    dplyr::mutate(
      Rank_EL = as.numeric(Rank_EL),
      Aff_nM_BindLevel = as.numeric(Aff_nM_BindLevel),
      Type = type,
      Sample = sample,
      Gene = gene,
      Mutation = mutation,
      Tool = tool
    )
  df
}

# functions to compute expected counts of peptides
parse_hla <- function(x) {
  unlist(strsplit(x, ","))
}

expected_counts <- function(scenario_row, n_windows) {
  
  true_alleles <- parse_hla(scenario_row$HLA_true)
  mis_alleles  <- parse_hla(scenario_row$HLA_mistyped)
  
  all_alleles <- union(true_alleles, mis_alleles)
  
  tibble::tibble(
    HLA = all_alleles,
    expected_n = purrr::map_int(all_alleles, function(hla) {
      in_true <- hla %in% true_alleles
      in_mis  <- hla %in% mis_alleles
      
      multiplier <- as.integer(in_true) + as.integer(in_mis)
      multiplier * n_windows
    })
  )
}

check_counts <- function(paired, scenario_row, n_windows = 38) {
  
  expected <- expected_counts(scenario_row, n_windows)
  observed <- paired %>% count(HLA, name = "observed_n")
  check <- expected %>%
    left_join(observed, by = "HLA") %>%
    mutate(
      observed_n = tidyr::replace_na(observed_n, 0),
      ok = expected_n == observed_n
    )
  
  if (!all(check$ok)) {
    print(check)
    stop("Row count mismatch detected")
  }
  message("row counts are correct")
  return(check)
  
}

# read in scenarios.txt
scenarios <- read.table(scenarios, sep = "\t", header = T)
all_results <- purrr::map_dfr(files, parse_netmhcpan)
print("all_results before case_when")
print(head(all_results))


all_results <- all_results %>%
  mutate(
    condition = case_when(
      grepl("WT", Type) ~ "WT",
      grepl("MUT", Type) ~ "MUT"
    ),
    hla_status = case_when(
      grepl("TRUE", Type) ~ "TRUE",
      grepl("MIS", Type) ~ "MIS"
    )
  )

peptides_df <- read.table(file = peptides, sep = "\t", header = T)

print("peptides_df before running distinct...")
print(head(peptides_df))
peptides_df <- peptides_df %>%
  distinct(sample, gene, mutation, start, end, peptide_wt, peptide_mut)

wt <- all_results %>%
  filter(condition == "WT") %>%
  inner_join(
    peptides_df,
    by = c(
           "Gene" = "gene",
           "Mutation" = "mutation"
           )
  ) %>%
  filter(Peptide == peptide_wt)

mut <- all_results %>%
  filter(condition == "MUT") %>%
  inner_join(
    peptides_df,
    by = c(
          #"Sample" = "sample",
           "Gene" = "gene",
           "Mutation" = "mutation"
           )
  ) %>%
  filter(Peptide == peptide_mut)
wt_s <- wt %>%
  slice_min(Rank_EL, n = 1, by = c(Tool, HLA, start, end)) %>%
  dplyr::select(-c(sample))

mut_s <- mut %>%
  slice_min(Rank_EL, n = 1, by = c(Tool, HLA, start, end)) %>% 
  dplyr::select(-c(sample, peptide_wt, peptide_mut))
paired <- wt_s %>%
  inner_join(
    mut_s,
    by = c(
      "Tool",
      "HLA",
      "Sample",
      "Gene",
      "Mutation",
      "start",
      "end",
      #"Type",
      "hla_status"
    ),
    suffix = c("_wt", "_mut")
  )

paired_is_binder <- paired %>% 
                    dplyr::mutate(mut_is_weak_binder = if_else(Rank_EL_mut < 2, TRUE, FALSE),
                                  wt_is_weak_binder = if_else(Rank_EL_wt < 2, TRUE, FALSE),
                                  mut_binder_wt_not_binder = if_else(mut_is_weak_binder & !wt_is_weak_binder, TRUE, FALSE),
                                  mut_is_strong_binder =  if_else(Rank_EL_mut < 0.5, TRUE, FALSE)
                    )

tools_in_results <- unique(paired_is_binder$Tool)
for (current_tool in tools_in_results) {
  message(paste("Checking counts for tool:", current_tool))
  # Filter the paired data for the current tool
  paired_for_check <- paired_is_binder %>% 
    dplyr::filter(Tool == current_tool) %>%
    mutate(HLA = gsub("\\*", "", HLA))
  print("paired_for_check...")
  head(paired_for_check)
  # Find the corresponding scenario for this specific sample and tool
  scenario_for_check <- scenarios %>% 
    dplyr::filter(Sample == sample & Tool == current_tool)
  head("scenario_for_check...")
  head(scenario_for_check)
  # Run the check
  check_counts(paired_for_check, scenario_for_check)
}

paired_is_binder$HLA_gene <- sub("^HLA-([A-Z]+).*", "\\1", paired_is_binder$HLA)
print("paired_is_binder...")
print(paired_is_binder)

# paired_is_binder %>% dplyr::group_by(HLA_gene, Peptide_mut, Tool, Type_mut) %>% mutate(is_fp = "any mut_is_weak_binder is TRUE where hla_status
# is MIS and all mut_is_weak_binder is FALSE where hla_status is TRUE")

results <- paired_is_binder %>% group_by(Sample, Gene, Mutation, Tool, Type_mut, HLA_gene) %>% summarise(nbinders = sum(mut_is_weak_binder))
print("results")
print(results)
# 
# decision <- paired_is_binder %>%
#   mutate(is_binder = mut_is_weak_binder) %>%
#   dplyr::select(Sample, Gene, Mutation, Tool, Peptide_mut, HLA_gene, HLA, Type_mut, is_binder)

decision <- paired_is_binder %>%
  group_by(Sample, Gene, Mutation, Tool, HLA_gene, Peptide_mut) %>%
  summarise(
    
    binds_true = any(mut_is_weak_binder[hla_status == "TRUE"]),
    binds_mis  = any(mut_is_weak_binder[hla_status == "MIS"]),
    strong_binder_true = any(mut_is_strong_binder[hla_status == "TRUE"]),
    strong_binder_mis = any(mut_is_strong_binder[hla_status == "MIS"]),
    true_binding_alleles = paste(
      unique(HLA[hla_status == "TRUE" & mut_is_weak_binder]),
      collapse = ";"
    ),
    
    mis_binding_alleles = paste(
      unique(HLA[hla_status == "MIS" & mut_is_weak_binder]),
      collapse = ";"
    ),
    
    true_non_binding_alleles = paste(
      unique(HLA[hla_status == "TRUE" & !mut_is_weak_binder]),
      collapse = ";"
    ),
    
    mis_non_binding_alleles = paste(
      unique(HLA[hla_status == "MIS" & !mut_is_weak_binder]),
      collapse = ";"
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    binder_gain = binds_mis & !binds_true,
    binder_loss = binds_true & !binds_mis,
    stable      = binds_true == binds_mis,
    strong_binder_gain = strong_binder_mis & !strong_binder_true,
    strong_binder_loss = strong_binder_true & !strong_binder_mis,
    stable_strong_binding_status = strong_binder_true == strong_binder_mis
  )

print("decision...")
decision %>% as.data.frame()
# decision_wide <- decision %>%
#   tidyr::pivot_wider(
#     names_from = Type_mut,
#     values_from = is_binder,
#     values_fill = FALSE
#   )
# print("decision_wide...")
# print(decision_wide)
# fn <- decision_wide %>%
#   dplyr::filter(MUT_TRUE == TRUE & MUT_MIS == FALSE)
# 
# stats <- decision_wide %>%
#   mutate(
#     FP = MUT_MIS == TRUE & MUT_TRUE == FALSE,
#     FN = MUT_TRUE == TRUE & MUT_MIS == FALSE
#   )
# 
# stable <- decision_wide %>%
#   dplyr::filter(MUT_TRUE == MUT_MIS)
# 
summary_stats <- decision %>%
  group_by(Sample, Gene, Mutation, Tool, HLA_gene) %>%
  summarise(
    n_true = sum(binds_true),
    n_mis  = sum(binds_mis),
    n_binder_gain   = sum(binder_gain),
    n_binder_loss   = sum(binder_loss),
    n_stable_binding_status = sum(stable),
    n_strong_binder_gain = sum(strong_binder_gain),
    n_strong_binder_loss = sum(strong_binder_loss),
    .groups = "drop"
  )
# 
outfile <- paste(sample, mutation_gene, mutation, "results_joined.csv", sep = "_")
outfile_decision <- paste(sample, mutation_gene, mutation, "results_decision.csv", sep = "_")
outfile_all_stats <- paste(sample, mutation_gene, mutation, "results_stats_all.csv", sep = "_")
# 
write.csv(paired_is_binder, file = outfile, quote = F, row.names= F)
write.csv(decision, file = outfile_decision, quote = F, row.names= F)
write.csv(summary_stats, file = outfile_all_stats, quote = F, row.names = F)