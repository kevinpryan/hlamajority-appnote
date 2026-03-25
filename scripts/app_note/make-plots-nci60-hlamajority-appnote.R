library(ggplot2)
library(ggthemes)
library(dplyr)
library(vroom)
library(forcats)
library(tidyr)
library(svglite)
setwd("/hlamajority-paper/external/mhc_genotyping/")
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

p_1row <- ggplot(df, aes(x = Tool, y = Accuracy, fill = Tool)) +
  
  # Create bars
  geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
  
  # Facet by Gene
  facet_wrap(~Gene, scales = "fixed", 
             ncol = 4,
             labeller = as_labeller(my_gene_labels)
  ) +
  
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text(#aes(label = sprintf("%.1f", accuracy)), 
    aes(label = paste(round(Accuracy, 1), "%", sep = "")),
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 6) +
  
  # Colors: Highlight Hlamajority (assuming it's the last factor level)
  # You can customize these colors. 
  # Here: Greys for others, Red/Blue for Hlamajority
  scale_fill_manual(values = c("#999999", "#999999", "#999999", "#999999", "#E69F00")) +
  
  # Scales
  scale_y_continuous(limits = c(0, 108), breaks = seq(0, 100, 25), expand = c(0,0)) +
  
  # Labels
  labs(
    #title = "Genotyping Accuracy by Tool and Gene",
    #subtitle = "Comparison with NCI-60 WES Dataset",
    y = "Accuracy (%)",
    x = "Tool",
    fill = "Tool") +
  
  # Theme customization
  theme_bw() +
  coord_cartesian(ylim = c(0, 108)) +     # Set the visible limits here
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
# 4. Display Plot
print(p_1row)
ggsave(plot = p_1row, filename = "/hlamajority-paper/results/app_note/plots/hlamajority-accuracy-per-gene-nci60-1row.svg", width = 10, height = 7)

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
# 4. Display Plot
print(p)
ggsave(plot = p, filename = "/hlamajority-paper/results/app_note/plots/hlamajority-accuracy-per-gene-nci60.svg", width = 9, height = 7)

