library(ggplot2)
library(ggthemes)
library(dplyr)
library(vroom)
library(forcats)
library(tidyr)
library(ggpubr)
library(rstatix)
setwd("/hlamajority-paper/external/mhc_genotyping/")
source("scripts/functions/evaluate_predictions_functions.R")
# Read in necessary data
# all calls nf-hlamajority 1000 Genomes
all.in <- read.table("../../data/raw/1000-genomes/majority/all_samples/combined_results/nf_hlamajority_all_calls_sorted.tsv", sep = "\t", header = T)
# results RDS object 1000 Genomes
results <- readRDS("../../data/processed/1000-genomes/majority/1000-genomes-full-results-hlamajority-majority-vote.Rds")
results$summary$Tool <- factor(
  results$summary$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
# nf-hlamajority stats
df <- read.csv("../../data/processed/1000-genomes/majority/1000-genomes-full-stats-hlamajority-majority-vote.csv")
# Claeys et al results
data_in_orig <- read.csv("../../data/claeys-et-al/claeys-et-al-benchmarking-results.csv")
# cleaned version of Claeys et al. results
claeys.results.full <- read.csv("../../data/claeys-et-al/benchmarking_results_claeys_cleaned.csv")
# cell line results nf-hlamajority
df.cell.lines.after.polysolver.change <- read.csv("../../data/processed/cell-lines-after-polysolver-change/majority/nci-full-stats-hlamajority-majority-vote.csv")
# # depth
# depth <- vroom("../../data/raw/1000-genomes/majority/all_samples/combined_results/nf_hlamajority_depth_sorted.tsv")
# scores <- read.csv("../../data/processed/1000-genomes/majority/1000genomes-score-per-sample.csv")
# gs.na <- read.csv("../../data/processed/1000-genomes/majority/1000-genomes-gs-na-samples.csv")
# # depth cell lines
# depth_cell_lines <- vroom("../../data/raw/cell-lines-after-polysolver-change/majority/combined_results/nf_hlamajority_depth_sorted.tsv")
# scores_cell_lines <- read.csv("../../data/processed/cell-lines-after-polysolver-change/majority/nci60-score-depth-per-sample-per-tool.csv")
cell.lines.all.results <- vroom("../../data/raw/cell-lines-after-polysolver-change/majority/combined_results/nf_hlamajority_all_calls_sorted.tsv")

results_cell_lines <- readRDS("../../data/processed/cell-lines-after-polysolver-change/majority/nci-full-results-hlamajority-majority-vote.Rds")
results_cell_lines$summary$Tool <- factor(
  results_cell_lines$summary$Tool, 
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)



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

# Make figure comparing Claeys et al and nf-hlamajority on 1000 Genomes data 
# Results Figure 3.3 B
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
    size = 6) +
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
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom", 
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(color="black", size=28, face="bold.italic")
  ) +
  ggtitle("1000 Genomes original benchmark vs nf-hlamajority")
compare_hlamajority_claeys_per_gene_per_tool
#  Results Figure 3.3 A 1000 Genomes accuracy per gene per tool
df.for.figure.1000genomes <- df %>%
  dplyr::select(Gene, Tool, Accuracy)
df.for.figure.1000genomes$Study <- "nf-hlamajority"
df.for.figure.1000genomes$Accuracy <- round(df.for.figure.1000genomes$Accuracy, 1)
data_for_plotting$Tool <- factor(
  data_for_plotting$Tool,
  levels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")#,
  #labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
data_for_plotting$Gene <- factor(
  data_for_plotting$Gene,
  levels = c("A", "B", "C"),
  labels = c("HLA-A", "HLA-B", "HLA-C")
)


figure.1000genomes <- ggplot(df.for.figure.1000genomes, aes(x = Tool, y = Accuracy, fill = Tool)) +
  
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
  coord_cartesian(ylim = c(0, 115)) +    
  theme(
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    legend.position = "none", # Hide legend since x-axis has labels
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(color="black", size=28, face="bold.italic")
  ) +
  labs(title = "1000 Genomes")


figure.1000genomes

# make plot cell lines AFTER polysolver alignment change
# results.cell.lines.after.polysolver.change <- readRDS("../../data/processed/cell-lines-after-polysolver-change/majority/nci-full-results-hlamajority-majority-vote.Rds")
# results.cell.lines.after.polysolver.change$summary$Tool <- factor(
#   results.cell.lines.after.polysolver.change$summary$Tool,
#   levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
#   labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
# )

df.cell.lines.after.polysolver.change <- df.cell.lines.after.polysolver.change %>% dplyr::mutate(Accuracy = 100*Accuracy)
df.cell.lines.after.polysolver.change$Gene <- factor(df.cell.lines.after.polysolver.change$Gene, levels = c("Overall", "A", "B", "C"))
df.cell.lines.after.polysolver.change$Tool <- factor(
  df.cell.lines.after.polysolver.change$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)

my_gene_labels <- c(
  "Overall" = "Overall Accuracy",
  "A"     = "HLA-A",
  "B"     = "HLA-B",
  "C"     = "HLA-C"
) 

cell.line.performance.after.polysolver.change <- ggplot(df.cell.lines.after.polysolver.change, aes(x = Tool, y = Accuracy, fill = Tool)) +
  
  # Create bars
  geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
  
  # Facet by Gene
  facet_wrap(~Gene, scales = "fixed", 
             ncol = 2,
             labeller = as_labeller(my_gene_labels)
  ) +
  
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text( 
    aes(label = paste(round(Accuracy, 1), "%", sep = "")),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, 
    size = 6) +
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
  coord_cartesian(ylim = c(0, 115)) + 
  
  theme(
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    legend.position = "none", # Hide legend since x-axis has labels
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(color="black", size=28, face="bold.italic"),
  ) +
  labs(title = "NCI-60 Cell Lines")
cell.line.performance.after.polysolver.change

combine.1000genomes.cell.lines <- ggarrange(
                             figure.1000genomes, 
                             compare_hlamajority_claeys_per_gene_per_tool,
                             cell.line.performance.after.polysolver.change, 
                             ncol = 1,
                             labels = c("A", "B", "C"),
                             font.label=list(color="black",size=28, face = "bold")
                             )
combine.1000genomes.cell.lines
ggsave(plot = combine.1000genomes.cell.lines, filename = "/hlamajority-paper/results/thesis/plots/hlamajority-1000genomes-nci-combined-20260525.svg", width = 15, height = 20)
ggsave(plot = combine.1000genomes.cell.lines, filename = "/hlamajority-paper/results/thesis/plots/hlamajority-1000genomes-nci-combined-20260525.pdf", width = 15, height = 20)
ggsave(plot = combine.1000genomes.cell.lines, filename = "/hlamajority-paper/results/thesis/figures/hlamajority-1000genomes-nci-combined-20260525.pdf", width = 15, height = 20)

# make wide figure excluding comparison with claeys

figure.1000genomes.1row <- ggplot(df.for.figure.1000genomes, aes(x = Tool, y = Accuracy, fill = Tool)) +
  
  # Create bars
  geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
  
  # Facet by Gene
  facet_wrap(~Gene, scales = "fixed", 
             nrow = 1,
             labeller = as_labeller(my_gene_labels)
  ) +
  
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text(#aes(label = sprintf("%.1f", accuracy)), 
    aes(label = paste(round(Accuracy, 1), "%", sep = "")),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, 
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
  coord_cartesian(ylim = c(0, 115)) +    
  theme(
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    legend.position = "none", # Hide legend since x-axis has labels
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(color="black", size=28, face="bold.italic")
  ) +
  labs(title = "1000 Genomes")


figure.1000genomes.1row

cell.line.performance.after.polysolver.change.1row <- ggplot(df.cell.lines.after.polysolver.change, aes(x = Tool, y = Accuracy, fill = Tool)) +
  
  # Create bars
  geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
  
  # Facet by Gene
  facet_wrap(~Gene, scales = "fixed", 
             nrow = 1,
             labeller = as_labeller(my_gene_labels)
  ) +
  
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text( 
    aes(label = paste(round(Accuracy, 1), "%", sep = "")),
    position = position_dodge(width = 0.9), 
    vjust = -0.3, 
    size = 6) +
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
  coord_cartesian(ylim = c(0, 115)) + 
  
  theme(
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    legend.position = "none", # Hide legend since x-axis has labels
    axis.title = element_text(size = 20, face = "bold"), 
    axis.text = element_text(size = 18), 
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_text(color="black", size=28, face="bold.italic"),
  ) +
  labs(title = "NCI-60 Cell Lines")
cell.line.performance.after.polysolver.change.1row

combine.1000genomes.cell.lines.no.comparison.claeys <- ggarrange(
  figure.1000genomes.1row, 
  # compare_hlamajority_claeys_per_gene_per_tool,
  cell.line.performance.after.polysolver.change.1row, 
  ncol = 1,
  labels = c("A", "B"),
  font.label=list(color="black",size=28, face = "bold")
)
ggsave(plot = combine.1000genomes.cell.lines.no.comparison.claeys, filename = "/hlamajority-paper/results/presentation/hlamajority-1000genomes-nci-combined-without-claeys-comparison.png", width = 20, height = 12, dpi = 600)

# depth
# depth_scores <- scores %>%
#   complete(gene, sample, tool) %>%   # create missing combinations
#   left_join(depth, by = c("gene", "sample"))
# # remove samples that are NA in the gold standard
# depth_scores_rm_na <- depth_scores %>% dplyr::filter(
#   !(sample == "NA12234" & gene == "HLA-C") &
#     !(sample == "NA12249" & gene == "HLA-B") &
#     !(sample == "NA18548" & gene == "HLA-C") &
#     !(is.na(Score))
# )
# #depth_scores_rm_na$correct_flag <- ifelse(depth_scores_rm_na$Score != 2 | is.na(depth_scores_rm_na$Score), "Incorrect", "Correct")
# depth_scores_rm_na$correct_flag <- ifelse(depth_scores_rm_na$Score != 2, "Incorrect", "Correct")
# 
# depth_scores_rm_na$correct_flag <- factor(depth_scores_rm_na$correct_flag, levels = c("Incorrect", "Correct"))
# 
# depth_scores_rm_na$tool <- factor(
#   depth_scores_rm_na$tool,
#   levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
#   labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
# )
# 
# depth_scores_rm_na %>% group_by(tool, correct_flag) %>% summarise(median_depth = median(mean_depth_hla_exons_2_3_gene))
# # summary_stats <- depth_scores_rm_na %>%
# #   group_by(tool, gene, correct_flag) %>%
# #   summarise(
# #     n = n(),
# #     mean_depth = mean(mean_depth_hla_exons_2_3_gene),
# #     median_depth = median(mean_depth_hla_exons_2_3_gene),
# #     sd_depth = sd(mean_depth_hla_exons_2_3_gene)
# #   ) %>%
# #   ungroup()
# 
# stat_test_per_tool <- depth_scores_rm_na %>%
#   group_by(tool) %>%
#   wilcox_test(mean_depth_hla_exons_2_3_gene ~ correct_flag) %>%
#   ungroup() %>%
#   # 2. Adjust the p-values across all tests
#   adjust_pvalue(method = "BH") %>%
#   # 3. Add significance stars (optional, but nice for plots)
#   add_significance("p.adj") %>%
#   # 4. Get y-position for plotting the labels on the graph
#   add_xy_position(x = "correct_flag", fun = "max", data = depth_scores_rm_na) %>% 
#   mutate(
#     p.adj.label = ifelse(
#       p.adj < 0.001,
#       formatC(p.adj, format = "e", digits = 2),
#       formatC(p.adj, format = "f", digits = 3)
#     )
#   )
# print(stat_test_per_tool)
# 
# stat_test_gene_tool <- depth_scores_rm_na %>%
#   group_by(gene, tool) %>%
#   wilcox_test(mean_depth_hla_exons_2_3_gene ~ correct_flag) %>%
#   ungroup() %>%
#   adjust_pvalue(method = "BH") %>%      # adjust across ALL tests
#   add_significance("p.adj") %>%
#   add_xy_position(
#     x = "correct_flag",
#     fun = "max",
#     data = depth_scores_rm_na
#   ) %>% 
#   mutate(
#     p.adj.label = ifelse(
#       p.adj < 0.001,
#       formatC(p.adj, format = "e", digits = 2),
#       formatC(p.adj, format = "f", digits = 3)
#     )
#   )
# stat_test_gene_tool
# 
# p_1000_genomes_per_tool <- ggplot(depth_scores_rm_na, aes(x = correct_flag, y = mean_depth_hla_exons_2_3_gene, fill = correct_flag)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(width = 0.2, alpha = 0.6) +
#   facet_wrap(~tool, scales = "free_y", nrow = 1) +
#   # facet_grid(
#   #   gene ~ tool,
#   #   scales = "fixed"#,
#   #   #labeller = labeller(gene = my_gene_labels)
#   # ) +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
#   labs(title = "Gene-Specific Coverage by Tool and Call Accuracy",
#        subtitle = "Benjamini-Hochberg test adjusted p-values",
#        x = "Call Type",
#        y = "Mean Depth of HLA Exons 2 & 3") +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_text(size = 18),
#         axis.text.x = element_text(size = 15, colour = "black"),
#         axis.text.y = element_text(size = 13, colour = "black"),
#         
#         strip.text = element_text(size = 18, colour = "black")
#   ) +
#   stat_pvalue_manual(
#     stat_test_per_tool,
#     label = "p.adj = {p.adj.label}, {p.adj.signif}",
#     tip.length = 0.01,
#     bracket.nudge.y = 0.05,
#     inherit.aes = FALSE
#   )
# p_1000_genomes_per_tool
# 
# 
# p_gene_adjusted_gene_tool <- ggplot(depth_scores_rm_na, aes(x = correct_flag, y = mean_depth_hla_exons_2_3_gene, fill = correct_flag)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(width = 0.2, alpha = 0.6) +
#   #facet_wrap(~tool, scales = "free_y") +
#   facet_grid(
#     gene ~ tool,
#     scales = "fixed"#,
#     #labeller = labeller(gene = my_gene_labels)
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
#   labs(title = "Gene-Specific Coverage by Tool and Call Accuracy",
#        subtitle = "Benjamini-Hochberg test adjusted p-values",
#        x = "Call Type",
#        y = "Mean Depth of HLA Exons 2 & 3") +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_text(size = 18),
#         axis.text.x = element_text(size = 15, colour = "black"),
#         axis.text.y = element_text(size = 13, colour = "black"),
#         
#         strip.text = element_text(size = 18, colour = "black")
#   ) +
#   stat_pvalue_manual(
#     stat_test_gene_tool,
#     label = "p.adj = {p.adj.label}, {p.adj.signif}",
#     tip.length = 0.01,
#     bracket.nudge.y = 0.05,
#     inherit.aes = FALSE
#   )
# p_gene_adjusted_gene_tool
# 
# # cell lines
# cell.lines.all.results.samples <- cell.lines.all.results %>% dplyr::select(sample, tool)
# samples <- cell.lines.all.results.samples %>% distinct(sample)
# samples$tool <- "hlamajority"
# samples.tools.complete <- rbind.data.frame(samples, cell.lines.all.results.samples)
# names(samples.tools.complete) <- c("sample_id.srx", "tool")
# master_df_mapped_full <- readRDS("../../data/processed/cell-lines-after-polysolver-change/majority/nci-map.Rds") %>% dplyr::select(sample, sample_id.srx) %>% distinct(sample, sample_id.srx) # %>% dplyr::select(sample, sample_id) %>% distinct(sample, sample_id)
# samples.tools.complete <- full_join(samples.tools.complete, master_df_mapped_full)
# Gene <- c("A", "B", "C")
# samples.tools.complete.gene <- samples.tools.complete
# samples.tools.complete.gene <- samples.tools.complete %>%
#   tidyr::crossing(Gene = Gene)
# 
# # now only keep those not missing in gold standard
# samples.missing.gs <- results_cell_lines$gold_standard_missing
# # samples.tools.complete.rm.na.gs <- samples.tools.complete %>% dplyr::filter(
# #   !(sample %in% "NA12234" & gene == "HLA-C") &
# #     !(sample == "NA12249" & gene == "HLA-B") &
# #     !(sample == "NA18548" & gene == "HLA-C")
# # )
# # remove these cell lines: "MALME-3M" "MDA-N"
# samples.tools.complete.gene <- samples.tools.complete.gene %>% dplyr::filter(sample != "MALME-3M" & sample != "MDA-N")
# 
# samples.tools.complete.gene.filtered <- samples.tools.complete.gene %>%
#   dplyr::anti_join(
#     samples.missing.gs,
#     by = c("sample" = "Sample", "Gene" = "Gene")
#   )
# samples.tools.complete.gene.filtered$Gene <- paste("HLA-", samples.tools.complete.gene.filtered$Gene, sep = "")
# hlamajority.depth <- depth_cell_lines %>% dplyr::select(sample, gene, mean_depth_hla_exons_2_3_gene)
# detailed.hlamajority.depth <- hlamajority.depth %>% rename(`sample_id.srx` = "sample") %>% left_join(master_df_mapped_full) #%>% dplyr::select(-all_of(c(A1, A2, B1, B2, C1, C2))) #rename(sample_id = sample)
#depth_scores <- scores %>%
#  complete(gene, sample, tool) %>%   # create missing combinations
#  left_join(depth, by = c("gene", "sample"))
# depth_scores_complete <- depth.scores %>%
#   complete(gene, sample, tool) %>%   # create missing combinations
#   left_join(detailed.hlamajority.depth, by = c("gene", "sample"))
# scores_cell_lines <- scores_cell_lines %>% left_join(master_df_mapped_full)
# depth_cell_lines <- depth_cell_lines %>% mutate(sample_id.srx = sample)
# depth_scores_cell_lines <- scores_cell_lines %>%
#   complete(gene, sample_id.srx, tool) %>%   # create missing combinations
#   left_join(depth_cell_lines, by = c("gene", "sample_id.srx"))
# 
# depth_scores <- left_join(depth.scores, detailed.hlamajority.depth)
# 
# depth_scores$tool <- factor(
#   depth_scores$tool,
#   levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
#   labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
# )
# depth_scores$correct_flag <- ifelse(depth_scores$Score == 2, "Correct", "Incorrect")
# depth_scores$correct_flag <- factor(depth_scores$correct_flag, levels = c("Incorrect", "Correct"))
# 
# stat_test_gene <- depth_scores %>%
#   group_by(tool) %>%
#   wilcox_test(mean_depth_hla_exons_2_3_gene ~ correct_flag) %>%
#   ungroup() %>%
#   # 2. Adjust the p-values across all tests
#   adjust_pvalue(method = "holm") %>%
#   # 3. Add significance stars (optional, but nice for plots)
#   add_significance("p.adj") %>%
#   # 4. Get y-position for plotting the labels on the graph
#   add_xy_position(x = "correct_flag", fun = "max", data = depth_scores)
# print(stat_test_gene)
# 
# depth_scores %>% group_by(tool) %>% summarise(median_depth = median)
# p_gene_adjusted <- ggplot(depth_scores, aes(x = correct_flag, y = mean_depth_hla_exons_2_3_gene, fill = correct_flag)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(width = 0.2, alpha = 0.6) +
#   facet_wrap(~tool, scales = "free_y") +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
#   labs(title = "Gene-Specific Coverage by Tool and Call Accuracy",
#        subtitle = "Benjamini-Hochberg corrected p-values",
#        x = "Call Type",
#        y = "Mean Depth of HLA Exons 2 & 3 (Gene)") +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_text(size = 18),
#         axis.text.x = element_text(size = 15, colour = "black"),
#         axis.text.y = element_text(size = 13, colour = "black"),
#         
#         strip.text = element_text(size = 18, colour = "black")
#   ) +
#   # Add the adjusted p-values and significance bars from our table
#   stat_pvalue_manual(
#     stat_test_gene,
#     label = "p.adj = {p.adj}, {p.adj.signif}", # Custom label
#     tip.length = 0.01,
#     bracket.nudge.y = 0.05,
#     inherit.aes = FALSE
#   )
# 
# print(p_gene_adjusted)
# 
# 
# stat_test_gene_tool <- depth_scores %>%
#   group_by(gene, tool) %>%
#   wilcox_test(mean_depth_hla_exons_2_3_gene ~ correct_flag) %>%
#   ungroup() %>%
#   adjust_pvalue(method = "holm") %>%      # adjust across ALL tests
#   add_significance("p.adj") %>%
#   add_xy_position(
#     x = "correct_flag",
#     fun = "max",
#     data = depth_scores
#   )
# 
# print(stat_test_gene_tool)
# 
# p_gene_adjusted_gene_tool <- ggplot(depth_scores, aes(x = correct_flag, y = mean_depth_hla_exons_2_3_gene, fill = correct_flag)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(width = 0.2, alpha = 0.6) +
#   #facet_wrap(~tool, scales = "free_y") +
#   facet_grid(
#     gene ~ tool,
#     scales = "fixed"#,
#     #labeller = labeller(gene = my_gene_labels)
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
#   labs(title = "Gene-Specific Coverage by Tool and Call Accuracy",
#        subtitle = "Holm test corrected p-values",
#        x = "Call Type",
#        y = "Mean Depth of HLA Exons 2 & 3") +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_text(size = 18),
#         axis.text.x = element_text(size = 15, colour = "black"),
#         axis.text.y = element_text(size = 13, colour = "black"),
#         
#         strip.text = element_text(size = 18, colour = "black")
#   ) +
#   # Add the adjusted p-values and significance bars from our table
#   stat_pvalue_manual(
#     stat_test_gene_tool,
#     label = "p.adj = {p.adj}, {p.adj.signif}", # Custom label
#     tip.length = 0.01,
#     bracket.nudge.y = 0.05,
#     inherit.aes = FALSE
#   )
# p_gene_adjusted_gene_tool

