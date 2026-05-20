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
# folder   <- args[5]
# scenarios <- args[6]
# peptides <- args[7]

if (length(args) < 6){
  stop("incorrect number or arguments")
}

# extract_field <- function(x, key) {
#   str_match(x, paste0(key, "=([^ ]+(?: [^ ]+)*)"))[,2]
# }

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
  ##print("CLLDILDTAGK in df$Peptide")
  #print(df %>% dplyr::filter(Peptide == "CLLDILDTAGK"))
  # type <- str_split_fixed(string = metadata_line, pattern = " ", n = 5)[,1]
  # type <- str_split_fixed(string = type, pattern = "=", n = 2)[,2]
  # sample <- str_split_fixed(string = metadata_line, pattern = " ", n = 5)[,2]
  # sample <- str_split_fixed(string = sample, pattern = "=", n = 2)[,2]
  # gene <- str_split_fixed(string = metadata_line, pattern = " ", n = 5)[,3]
  # gene <- str_split_fixed(string = gene, pattern = "=", n = 2)[,2]
  # mutation <- str_split_fixed(string = metadata_line, pattern = " ", n = 5)[,4]
  # mutation <- str_split_fixed(string = mutation, pattern = "=", n = 2)[,2]
  # tool <- str_split_fixed(string = metadata_line, pattern = " ", n = 5)[,5]
  # tool <- str_split_fixed(string = tool, pattern = "=", n = 2)[,2]
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
  #print("CLLDILDTAGK in df$Peptide after mutate/select")
  #print(df %>% dplyr::filter(Peptide == "CLLDILDTAGK"))
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
#scenarios <- read.table(file = "../../data/processed/neoantigen-prediction/scenarios.txt", sep = "\t", header = T)
scenarios <- read.table(scenarios, sep = "\t", header = T)
#files <- list.files("../../scripts/thesis/nf-neoantigen/work/c8/859c0a343f35104226f32cbfe0daf4", pattern = "\\.out$", full.names = TRUE)
#folder <- "../../scripts/thesis/nf-neoantigen/work/2d/3fc9f6ba037c6c2a56225f6e60aba1/"
#files <- list.files(folder, pattern = "\\.out$", full.names = TRUE)
#print("files...")
#print(files)
# print("contents of files dir")
# print(list.files(folder, full.names = TRUE))
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

#peptides_df <- read.table(file = "/hlamajority-paper/scripts/thesis/nf-neoantigen/work/3c/c2ac69104685f763ce748c98b9469b/NRAS_Q61K_peptides.txt", sep = "\t", header = T)
peptides_df <- read.table(file = peptides, sep = "\t", header = T)

print("peptides_df before running distinct...")
print(head(peptides_df))
peptides_df <- peptides_df %>%
  distinct(sample, gene, mutation, start, end, peptide_wt, peptide_mut)

peptides_df %>% dplyr::filter(peptide_mut == "CLLDILDTAGK")

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
# wt %>% dplyr::filter(peptide_mut == "CLLDILDTAGK")
# 
# all_results %>%   
#   filter(condition == "WT") %>%
#    dplyr::filter(peptide_mut == "CLLDILDTAGK")

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
# mut %>% dplyr::filter(peptide_mut == "CLLDILDTAGK")
# wt %>% dplyr::filter(peptide_mut == "CLLDILDTAGK")
wt_s <- wt %>%
  slice_min(Rank_EL, n = 1, by = c(Tool, HLA, start, end)) %>%
  dplyr::select(-c(sample))

mut_s <- mut %>%
  slice_min(Rank_EL, n = 1, by = c(Tool, HLA, start, end)) %>% 
  dplyr::select(-c(sample, peptide_wt, peptide_mut))
mut_s %>% dplyr::filter(Peptide == "CLLDILDTAGK")
wt_s %>% dplyr::filter(peptide_mut == "CLLDILDTAGK")
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
#paired %>% dplyr::filter(peptide_mut == "CLLDILDTAGK")

# paired %>%
#   count(HLA, Sample, Gene, Mutation, start, end, Type_wt, Type_wt, hla_status) %>%
#   filter(n > 1)
# 
# paired %>%
#   count(HLA, start, end,Type_mut) %>%
#   tidyr::pivot_wider(names_from = Type_mut, values_from = n) %>%
#   filter(is.na(MUT_TRUE) | is.na(MUT_MIS))

paired_is_binder <- paired %>% 
                    dplyr::mutate(mut_is_weak_binder = if_else(Rank_EL_mut < 2, TRUE, FALSE),
                                  wt_is_weak_binder = if_else(Rank_EL_wt < 2, TRUE, FALSE),
                                  mut_binder_wt_not_binder = if_else(mut_is_weak_binder & !wt_is_weak_binder, TRUE, FALSE)
                    )

# paired_is_binder %>% group_by(hla_status) %>% summarise(n_binder = sum(mut_is_weak_binder))

# all_results %>%
#   filter(Peptide %in% c("CLLDILDTAGQ", "CLLDILDTAGK")) %>%
#   count(HLA, Peptide)
# 
# paired %>%
#   distinct(HLA, start, end, peptide_wt, peptide_mut, hla_status) %>%
#   count(hla_status)

tools_in_results <- unique(paired_is_binder$Tool)
#sample <- "A549-ATCC"
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
results <- paired_is_binder %>% group_by(Sample, Gene, Mutation, Tool, Type_mut, HLA_gene) %>% summarise(nbinders = sum(mut_is_weak_binder))
print("results")
print(results)

# binders <- paired_is_binder %>% dplyr::filter(mut_is_weak_binder == TRUE)
# true_binders <- binders %>% dplyr::filter(Type_mut == MUT_TRUE) %>% dplyr::select(Peptide_mut) %>% pull()
# true_binders <- unique(true_binders)
# mis_binders <- binders %>% dplyr::filter(Type_mut == MUT_MIS) %>% dplyr::select(Peptide_mut) %>% pull()
# mis_binders <- unique(mis_binders)
decision <- paired_is_binder %>%
  group_by(Sample, Gene, Mutation, Tool, Peptide_mut, HLA_gene, Type_mut) %>%
  summarise(
    is_binder = any(mut_is_weak_binder),
    .groups = "drop"
  )

print("decision...")
print(head(decision))
decision_wide <- decision %>%
  tidyr::pivot_wider(
    names_from = Type_mut,
    values_from = is_binder,
    values_fill = FALSE
  )
print("decision_wide...")
print(decision_wide)
fn <- decision_wide %>%
  dplyr::filter(MUT_TRUE == TRUE & MUT_MIS == FALSE)

stats <- decision_wide %>%
  mutate(
    FP = MUT_MIS == TRUE & MUT_TRUE == FALSE,
    FN = MUT_TRUE == TRUE & MUT_MIS == FALSE
  )

stable <- decision_wide %>%
  dplyr::filter(MUT_TRUE == MUT_MIS)

summary_stats <- decision_wide %>%
  group_by(Sample, Gene, Mutation, Tool, HLA_gene) %>%
  summarise(
    n_true = sum(MUT_TRUE),
    n_mis  = sum(MUT_MIS),
    n_fn   = sum(MUT_TRUE & !MUT_MIS),
    n_fp   = sum(!MUT_TRUE & MUT_MIS),
    stability = sum(MUT_TRUE == MUT_MIS) / n(),
    .groups = "drop"
  )

#paired_for_check <- paired %>% mutate(HLA = gsub("\\*", "", HLA))
#scenarios_for_check <- scenarios %>% dplyr::filter(Sample == sample & HLA_Gene == hla_gene)
#check_counts(paired_for_check, scenarios_for_check)
#outfile <- paste(sample, hla_gene, mutation_gene, mutation, "results_joined.csv", sep = "_")
outfile <- paste(sample, mutation_gene, mutation, "results_joined.csv", sep = "_")
outfile_summary <- paste(sample, mutation_gene, mutation, "results_summary.csv", sep = "_")
outfile_all_stats <- paste(sample, mutation_gene, mutation, "results_stats_all.csv", sep = "_")

write.csv(paired_is_binder, file = outfile, quote = F, row.names= F)
write.csv(summary_stats, file = outfile_summary, quote = F, row.names= F)
write.csv(stats, file = outfile_all_stats, quote = F, row.names = F)