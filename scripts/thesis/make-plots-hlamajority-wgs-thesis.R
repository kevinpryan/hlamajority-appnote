library(ggplot2)
library(ggthemes)
library(dplyr)
library(vroom)
library(forcats)
library(tidyr)
library(ggpubr)

setwd("/hlamajority-paper/external/mhc_genotyping/")
source("scripts/functions/evaluate_predictions_functions.R")

all.in <- read.table("../../data/raw/1000-genomes/wgs-30x-149samples-majority/combined_results/nf_hlamajority_all_calls_sorted.tsv", sep = "\t", header = T)
results <- readRDS("../../data/processed/1000-genomes/wgs-30x-149samples-majority/1000-genomes-30x-wgs-full-results-hlamajority-majority-vote.Rds")
results$summary$Tool <- factor(
  results$summary$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
df <- read.csv("../../data/processed/1000-genomes/wgs-30x-149samples-majority/1000-genomes-30x-wgs-full-stats-hlamajority-majority-vote.csv")
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

df.for.figure <- df %>%
  dplyr::select(Gene, Tool, Accuracy)
df.for.figure$Study <- "nf-hlamajority"
df.for.figure$Accuracy <- round(df.for.figure$Accuracy, 1)

my_gene_labels <- c(
  "Overall" = "Overall Accuracy",
  "A"     = "HLA-A",
  "B"     = "HLA-B",
  "C"     = "HLA-C"
)

figure <- ggplot(df.for.figure, aes(x = Tool, y = Accuracy, fill = Tool)) +
  
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
    vjust = -0.5, 
    size = 6) +
  
  # Colours: Highlight Hlamajority 
  # Here: Greys for others, Orange for Hlamajority
  scale_fill_manual(values = c("#999999", "#999999", "#999999", "#999999", "#E69F00")) +
  
  # Scales
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, 25), expand = c(0,0)) +
  
  # Labels
  labs(
    y = "Accuracy (%)",
    x = "Tool",
    fill = "Tool") +
  
  # Theme customization
  theme_bw() +
  coord_cartesian(ylim = c(0, 120)) +    
  theme(
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 20),
    panel.grid.major.x = element_blank(),
    legend.position = "none", # Hide legend since x-axis has labels
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 18),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(color="black", size=24, face="bold.italic")
  ) +
  ggtitle("1000 Genomes WGS Benchmark")

figure

ggsave(plot = figure, filename = "/hlamajority-paper/results/thesis/plots/hlamajority-1000genomes-wgs-two-col.svg", width = 10, height = 7)
ggsave(plot = figure, filename = "/hlamajority-paper/results/thesis/plots/hlamajority-1000genomes-wgs-two-col.pdf", width = 10, height = 7)
