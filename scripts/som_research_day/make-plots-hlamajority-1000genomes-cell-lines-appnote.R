library(ggplot2)
library(ggthemes)
library(dplyr)
library(vroom)
library(forcats)
library(tidyr)
library(ggpubr)

setwd("/hlamajority-paper/external/mhc_genotyping/")
source("scripts/functions/evaluate_predictions_functions.R")
#all.in <- read.table("../../data/raw/1000-genomes/benchmark-1000genomes-nfhlamajority-local-update-db-exclude-trim-majority-all-samples/benchmark-1000genomes-nfhlamajority-all-20260309-majority-handle-error-kourami-hlala/combined_results/nf_hlamajority_all_calls_sorted.tsv", sep = "\t", header = T)
#results <- readRDS("../../data/processed/results/hlamajority/1000genomes-all-samples/1000-genomes-full-results-hlamajority-majority-vote.Rds")
all.in <- read.table("../../data/raw/1000-genomes/majority/all_samples/combined_results/nf_hlamajority_all_calls_sorted.tsv", sep = "\t", header = T)
results <- readRDS("../../data/processed/1000-genomes/majority/1000-genomes-full-results-hlamajority-majority-vote.Rds")
results$summary$Tool <- factor(
  results$summary$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
df <- read.csv("../../data/processed/1000-genomes/majority/1000-genomes-full-stats-hlamajority-majority-vote.csv")
df <- df %>% dplyr::mutate(Accuracy = 100*Accuracy)
df$Gene <- factor(df$Gene, levels = c("Overall", "A", "B", "C"))
df$Tool <- factor(
  df$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)

my_gene_labels <- c(
  "Overall" = "Overall Accuracy",
  "A"     = "HLA-A",
  "B"     = "HLA-B",
  "C"     = "HLA-C"
) 

data_in_orig <- read.csv("../../data/claeys-et-al/claeys-et-al-benchmarking-results.csv")
data_in <- data_in_orig[1:2,1:4]
data_in_long <- pivot_longer(data = data_in, cols = c("A", "B", "C"), names_to = "HLA_Allele")
data_in_long$Method <- "Original_benchmark"
colnames(data_in_long) <- c("Tool", "Gene", "Accuracy", "Method")
best_wes <- df %>% dplyr::filter(Tool != "nf-hlamajority" & Gene != "Overall") %>% group_by(Gene) %>% dplyr::filter(Accuracy == max(Accuracy)) %>% dplyr::select(Gene, Tool, Accuracy) %>% mutate(Tool = "best_wes") 
meta_wes <- df %>% dplyr::filter(Tool == "nf-hlamajority" & Gene != "Overall") %>%  dplyr::select(Gene, Tool, Accuracy)

best_wes$Tool <- "best_wes"
meta_wes$Tool <- "meta_wes"
full_results <- rbind.data.frame(best_wes, meta_wes)
full_results$Method <- "nf-hlamajority"

# combine results
all_results <- rbind.data.frame(data_in_long, full_results)
all_results$Accuracy <- as.numeric(all_results$Accuracy)
all_results$Method <- factor(
  all_results$Method,
  levels = c("Original_benchmark", "nf-hlamajority"),
  labels = c("Original benchmark", "nf-hlamajority")
)
all_results$Gene <- factor(
  all_results$Gene,
  levels = c("A", "B", "C"),
  labels = c("HLA-A", "HLA-B", "HLA-C")
)

all_results$Tool <- factor(
  all_results$Tool,
  levels = c("best_wes", "meta_wes"),
  labels = c("Best WES", "Meta WES")
)

claeys.results.full <- read.csv("../../data/claeys-et-al/benchmarking_results_claeys_cleaned.csv")
df.for.comparison <- df %>%
  dplyr::select(Gene, Tool, Accuracy) %>%
  dplyr::filter(Gene != "Overall")
df.for.comparison$Tool <- gsub("nf-hlamajority", "Metaclassifier", df.for.comparison$Tool)
df.for.comparison$Study <- "nf-hlamajority"
df.for.comparison$Accuracy <- round(df.for.comparison$Accuracy, 1)
claeys.results.full$tool <- gsub(pattern = "hlala", replacement = "HLA*LA", x = claeys.results.full$tool)
claeys.results.full$tool <- gsub(pattern = "kourami", replacement = "Kourami", x = claeys.results.full$tool)
claeys.results.full$tool <- gsub(pattern = "optitype", replacement = "Optitype", x = claeys.results.full$tool)
claeys.results.full$tool <- gsub(pattern = "polysolver", replacement = "Polysolver", x = claeys.results.full$tool)
claeys.results.full.long <- tidyr::pivot_longer(data = claeys.results.full, cols = c("A", "B", "C"), names_to = "Gene", values_to = "Accuracy") %>% 
  mutate(Accuracy = 100*Accuracy) %>% 
  rename(Tool = "tool")
# now add metaclassifier results
data_in_orig_meta_wes <- data_in_orig %>% dplyr::filter(Feature == "meta_wes") %>% 
  dplyr::select(Feature, A, B, C) %>% 
  pivot_longer(cols = c("A", "B", "C"), names_to = "Gene", values_to = "Accuracy") %>% 
  rename(`Tool` = "Feature") 
data_in_orig_meta_wes$Tool <- gsub("meta_wes", "Metaclassifier", data_in_orig_meta_wes$Tool)         
data_in_orig_meta_wes$Study <- "Original_benchmark"
claeys.results.full.long$Study <- "Original_benchmark"
claeys.results.full.long.combined <- rbind.data.frame(data_in_orig_meta_wes, claeys.results.full.long)
data_for_plotting <- rbind.data.frame(claeys.results.full.long.combined, df.for.comparison)
data_for_plotting$Accuracy <- as.numeric(data_for_plotting$Accuracy)
data_for_plotting$Study <- factor(
  data_for_plotting$Study,
  levels = c("Original_benchmark", "nf-hlamajority"),
  labels = c("Original benchmark", "nf-hlamajority")
)
data_for_plotting$Tool <- factor(
  data_for_plotting$Tool,
  levels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "Metaclassifier")#,
  #labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
data_for_plotting$Gene <- factor(
  data_for_plotting$Gene,
  levels = c("A", "B", "C"),
  labels = c("HLA-A", "HLA-B", "HLA-C")
)

compare_hlamajority_claeys_per_gene_per_tool <- ggplot(data_for_plotting, aes(x = Study, y = Accuracy, fill = Study)) +
  # Create bars
  geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
  facet_grid(
    Gene ~ Tool,
    scales = "fixed"
  ) +
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text( 
    aes(label = paste(round(as.numeric(Accuracy), 1), "%", sep = "")),
    position = position_dodge(width = 0.9), 
    #vjust = -0.5,
    vjust = -0.3,
    size = 5) +
  # Scales
  scale_y_continuous(breaks = seq(0, 100, 25), expand = c(0,0)) +
  
  # Labels
  labs(
    y = "Accuracy (%)",
    x = NULL,
    fill = "Study") +
  
  # Theme customization
  theme_bw() +
  coord_cartesian(ylim = c(0, 120)) +     # Set the visible limits here
  theme(
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines")
  )

# make plot cell lines
results <- readRDS("../../data/processed/cell-lines/majority/nci-full-results-hlamajority-majority-vote.Rds")
results$summary$Tool <- factor(
  results$summary$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
df <- read.csv("../../data/processed/cell-lines/majority/nci-full-stats-hlamajority-majority-vote.csv")
df <- df %>% dplyr::mutate(Accuracy = 100*Accuracy)
df$Gene <- factor(df$Gene, levels = c("Overall", "A", "B", "C"))
df$Tool <- factor(
  df$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)

my_gene_labels <- c(
  "Overall" = "Overall Accuracy",
  "A"     = "HLA-A",
  "B"     = "HLA-B",
  "C"     = "HLA-C"
) 

p <- ggplot(df, aes(x = Tool, y = Accuracy, fill = Tool)) +
  
  # Create bars
  geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
  
  # Facet by Gene
  facet_wrap(~Gene, scales = "fixed", 
             ncol = 4,
             labeller = as_labeller(my_gene_labels)
  ) +
  
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text(
    aes(label = paste(round(Accuracy, 1), "%", sep = "")),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, 
    size = 5) +
  
  # Colours: Highlight nf-hlamajority
  scale_fill_manual(values = c("#999999", "#999999", "#999999", "#999999", "#E69F00")) +
  
  # Scales
  scale_y_continuous(limits = c(0, 111), breaks = seq(0, 100, 25), expand = c(0,0)) +
  
  # Labels
  labs(
    y = "Accuracy (%)",
    x = "Tool",
    fill = "Tool") +
  
  # Theme customization
  theme_bw() +
  coord_cartesian(ylim = c(0, 111)) +     # Set the visible limits here
  theme(
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "none", # Hide legend since x-axis has labels
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 16),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines")
  )
p

ggsave(plot = compare_hlamajority_claeys_per_gene_per_tool, filename = "/hlamajority-paper/results/som_research_day/plots/hlamajority-1000genomes.svg", width = 14, height = 7)
ggsave(plot = p, filename = "/hlamajority-paper/results/som_research_day/plots/hlamajority-cell-lines.svg", width = 14, height = 5)
ggsave(plot = compare_hlamajority_claeys_per_gene_per_tool, filename = "/hlamajority-paper/results/som_research_day/plots/hlamajority-1000genomes.png", width = 14, height = 5)
ggsave(plot = p, filename = "/hlamajority-paper/results/som_research_day/plots/hlamajority-cell-lines.png", width = 14, height = 5)

#ggsave(plot = compare_hlamajority_claeys_per_gene_per_tool, filename = "/hlamajority-paper/results/som_research_day/plots/hlamajority-1000genomes.svg", width = 12, height = 7)

