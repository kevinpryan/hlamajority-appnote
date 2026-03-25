library(ggplot2)
library(ggthemes)
library(dplyr)
library(vroom)
library(forcats)
library(tidyr)
library(ggpubr)

setwd("/hlamajority-paper/external/mhc_genotyping/")
source("scripts/functions/evaluate_predictions_functions.R")
all.in <- read.table("../../data/raw/1000-genomes/benchmark-1000genomes-nfhlamajority-local-update-db-exclude-trim-majority-all-samples/benchmark-1000genomes-nfhlamajority-all-20260309-majority-handle-error-kourami-hlala/combined_results/nf_hlamajority_all_calls_sorted.tsv", sep = "\t", header = T)
results <- readRDS("../../data/processed/results/hlamajority/1000genomes-all-samples/1000-genomes-full-results-hlamajority-majority-vote.Rds")
results$summary$Tool <- factor(
  results$summary$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
df <- read.csv("../../data/processed/results/hlamajority/1000genomes-all-samples/1000-genomes-full-stats-hlamajority-majority-vote.csv")

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

# p_1row <- ggplot(df, aes(x = Tool, y = Accuracy, fill = Tool)) +
#   
#   # Create bars
#   geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
#   
#   # Facet by Gene
#   facet_wrap(~Gene, scales = "fixed", 
#              ncol = 4,
#              labeller = as_labeller(my_gene_labels)
#   ) +
#   
#   # Add text labels on top of bars (rounded to 1 decimal)
#   geom_text(#aes(label = sprintf("%.1f", accuracy)), 
#     aes(label = paste(round(Accuracy, 1), "%", sep = "")),
#     position = position_dodge(width = 0.9), 
#     vjust = -0.5, 
#     size = 6) +
#   
#   # Colors: Highlight Hlamajority (assuming it's the last factor level)
#   # You can customize these colors. 
#   # Here: Greys for others, Red/Blue for Hlamajority
#   scale_fill_manual(values = c("#999999", "#999999", "#999999", "#999999", "#E69F00")) +
#   
#   # Scales
#   scale_y_continuous(limits = c(0, 108), breaks = seq(0, 100, 25), expand = c(0,0)) +
#   
#   # Labels
#   labs(
#     #title = "Genotyping Accuracy by Tool and Gene",
#     #subtitle = "Comparison with NCI-60 WES Dataset",
#     y = "Accuracy (%)",
#     x = "",
#     fill = "Tool") +
#   
#   # Theme customization
#   theme_bw() +
#   coord_cartesian(ylim = c(0, 108)) +     # Set the visible limits here
#   theme(
#     strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
#     strip.text = element_text(face = "bold", size = 24),
#     panel.grid.major.x = element_blank(),
#     legend.position = "none", # Hide legend since x-axis has labels
#     axis.title = element_text(size = 20, face = "bold"), 
#     axis.text = element_text(size = 18), 
#     axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20),
#     axis.text.y = element_text(colour = "black"),
#   )
# 
# # 4. Display Plot
# print(p_1row)
# ggsave(plot = p_1row, filename = "../../results/app_note/plots/hlamajority-accuracy-per-gene-1000genomes-all-samples-1row.svg", width = 15, height = 7)
# 
# p <- ggplot(df, aes(x = Tool, y = Accuracy, fill = Tool)) +
#   
#   # Create bars
#   geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
#   
#   # Facet by Gene
#   facet_wrap(~Gene, scales = "fixed", 
#              ncol = 2,
#              labeller = as_labeller(my_gene_labels)
#   ) +
#   
#   # Add text labels on top of bars (rounded to 1 decimal)
#   geom_text(#aes(label = sprintf("%.1f", accuracy)), 
#     aes(label = paste(round(Accuracy, 1), "%", sep = "")),
#     position = position_dodge(width = 0.9), 
#     vjust = -0.5, 
#     size = 6) +
#   
#   # Colors: Highlight Hlamajority (assuming it's the last factor level)
#   # You can customize these colors. 
#   # Here: Greys for others, Red/Blue for Hlamajority
#   scale_fill_manual(values = c("#999999", "#999999", "#999999", "#999999", "#E69F00")) +
#   
#   # Scales
#   scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, 25), expand = c(0,0)) +
#   
#   # Labels
#   labs(
#     #title = "Genotyping Accuracy by Tool and Gene",
#     #subtitle = "Comparison with NCI-60 WES Dataset",
#     y = "Accuracy (%)",
#     x = "Tool",
#     fill = "Tool") +
#   
#   # Theme customization
#   theme_bw() +
#   coord_cartesian(ylim = c(0, 115)) +     # Set the visible limits here
#   theme(
#     #axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x labels
#     strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
#     strip.text = element_text(face = "bold", size = 18),
#     panel.grid.major.x = element_blank(),
#     legend.position = "none", # Hide legend since x-axis has labels
#     axis.title = element_text(size = 20, face = "bold"), 
#     axis.text = element_text(size = 16), 
#     axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
#     axis.text.y = element_text(colour = "black")
#   )
# 
# # 4. Display Plot
# print(p)
# ggsave(plot = p, filename = "../../results/app_note/plots/hlamajority-accuracy-per-gene-1000genomes-all-samples.svg", width = 7, height = 5)

data_in_orig <- read.csv("../../data/claeys-et-al/claeys-et-al-benchmarking-results.csv")
data_in <- data_in_orig[1:2,1:4]
data_in_long <- pivot_longer(data = data_in, cols = c("A", "B", "C"), names_to = "HLA_Allele")
mhci <- ggplot(data_in_long, aes(fill=Feature, y=as.numeric(value), x=HLA_Allele)) +
  geom_col(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) + # Keep this to remove bottom padding
  coord_cartesian(ylim=c(0,104)) +
  ylab("Accuracy (%)") +
  xlab("MHC Class I Gene") +
  scale_fill_tableau() +
  theme_bw() +
  scale_fill_discrete(c(""), labels = c("Best Individual Tool", "Metaclassifier")) +
  theme(axis.title=element_text(size=20), axis.text = element_text(size=16), legend.text = element_text(size = 16)) +
  geom_text( 
    position = position_dodge(width = 0.9), # Matches the bar dodge width
    aes(
      label = paste(round(as.numeric(value), 1), "%", sep = ""),
      group = Feature
    ),
    vjust = -0.2, # Position text just above the bar
    size = 4
  )
data_in_long$Method <- "Original_benchmark"
colnames(data_in_long) <- c("Tool", "Gene", "Accuracy", "Method")
#ylim(95.0,100.0) 
mhci

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
# new_name = old_name
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
  # facet_wrap(~Tool, scales = "free_y") + 
  facet_grid(
    Gene ~ Tool,
    scales = "free_y"#,
    #labeller = labeller(gene = my_gene_labels)
  ) +
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text(#aes(label = sprintf("%.1f", accuracy)), 
    aes(label = paste(round(as.numeric(Accuracy), 1), "%", sep = "")),
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 6) +
  # Scales
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, 25), expand = c(0,0)) +
  # Labels
  labs(
    y = "Accuracy (%)",
    x = "Tool",
    fill = "Study") +
  
  # Theme customization
  theme_bw() +
  coord_cartesian(ylim = c(0, 125)) +     # Set the visible limits here
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x labels
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", # Hide legend since x-axis has labels
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "black"),
  )
compare_hlamajority_claeys_per_gene_per_tool

# ggsave(filename = "../../results/app_note/plots/hlamajority-compare-claeys.svg", plot = compare_hlamajority_claeys_per_gene_per_tool, width = 10, height = 7)

# make plots cell lines

results <- readRDS("../../data/processed/results/hlamajority/cell-lines/nci-full-results-hlamajority-majority-vote.Rds")
results$summary$Tool <- factor(
  results$summary$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
df <- read.csv("../../data/processed/results/hlamajority/cell-lines/nci-full-stats-hlamajority-majority-vote.csv")
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

# p_1row <- ggplot(df, aes(x = Tool, y = Accuracy, fill = Tool)) +
#   
#   # Create bars
#   geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
#   
#   # Facet by Gene
#   facet_wrap(~Gene, scales = "fixed", 
#              ncol = 4,
#              labeller = as_labeller(my_gene_labels)
#   ) +
#   
#   # Add text labels on top of bars (rounded to 1 decimal)
#   geom_text(#aes(label = sprintf("%.1f", accuracy)), 
#     aes(label = paste(round(Accuracy, 1), "%", sep = "")),
#     position = position_dodge(width = 0.9), 
#     vjust = -0.5, 
#     size = 6) +
#   
#   # Colors: Highlight Hlamajority (assuming it's the last factor level)
#   # You can customize these colors. 
#   # Here: Greys for others, Red/Blue for Hlamajority
#   scale_fill_manual(values = c("#999999", "#999999", "#999999", "#999999", "#E69F00")) +
#   
#   # Scales
#   scale_y_continuous(limits = c(0, 108), breaks = seq(0, 100, 25), expand = c(0,0)) +
#   
#   # Labels
#   labs(
#     #title = "Genotyping Accuracy by Tool and Gene",
#     #subtitle = "Comparison with NCI-60 WES Dataset",
#     y = "Accuracy (%)",
#     x = "Tool",
#     fill = "Tool") +
#   
#   # Theme customization
#   theme_bw() +
#   coord_cartesian(ylim = c(0, 108)) +     # Set the visible limits here
#   theme(
#     #axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x labels
#     strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
#     strip.text = element_text(face = "bold", size = 24),
#     panel.grid.major.x = element_blank(),
#     legend.position = "none", # Hide legend since x-axis has labels
#     axis.title = element_text(size = 20, face = "bold"), 
#     axis.text = element_text(size = 18), 
#     axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20),
#     axis.text.y = element_text(colour = "black"),
#   )
# # 4. Display Plot
# print(p_1row)
# ggsave(plot = p_1row, filename = "/hlamajority-paper/results/app_note/plots/hlamajority-accuracy-per-gene-nci60-1row.svg", width = 10, height = 7)

p <- ggplot(df, aes(x = Tool, y = Accuracy, fill = Tool)) +
  
  # Create bars
  geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
  
  # Facet by Gene
  facet_wrap(~Gene, scales = "fixed", 
             ncol = 2,
             labeller = as_labeller(my_gene_labels)
  ) +
  
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text(#aes(label = sprintf("%.1f", accuracy)), 
    aes(label = paste(round(Accuracy, 1), "%", sep = "")),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, 
    size = 6) +
  
  # Colors: Highlight Hlamajority (assuming it's the last factor level)
  # You can customize these colors. 
  # Here: Greys for others, Red/Blue for Hlamajority
  scale_fill_manual(values = c("#999999", "#999999", "#999999", "#999999", "#E69F00")) +
  
  # Scales
  scale_y_continuous(limits = c(0, 111), breaks = seq(0, 100, 25), expand = c(0,0)) +
  
  # Labels
  labs(
    #title = "Genotyping Accuracy by Tool and Gene",
    #subtitle = "Comparison with NCI-60 WES Dataset",
    y = "Accuracy (%)",
    x = "Tool",
    fill = "Tool") +
  
  # Theme customization
  theme_bw() +
  coord_cartesian(ylim = c(0, 111)) +     # Set the visible limits here
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x labels
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 24),
    panel.grid.major.x = element_blank(),
    legend.position = "none", # Hide legend since x-axis has labels
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 20),
    axis.text.y = element_text(colour = "black"),
  )

nci_1000genomes_combined <- ggarrange(compare_hlamajority_claeys_per_gene_per_tool, p, ncol = 2, nrow = 1)
ggsave(plot = nci_1000genomes_combined, filename = "/hlamajority-paper/results/app_note/plots/hlamajority-1000genomes-nci-combined.svg", width = 20, height = 7)
