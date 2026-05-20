library(ggplot2)
library(ggthemes)
library(dplyr)
library(vroom)
library(forcats)
library(tidyr)
library(ggpubr)
#install.packages("tidytext")
library(tidytext)
library(tibble)
setwd("/hlamajority-paper/external/mhc_genotyping/")
source("scripts/functions/evaluate_predictions_functions.R")
#all.in <- read.table("../../data/raw/1000-genomes/majority/all_samples/combined_results/nf_hlamajority_all_calls_sorted.tsv", sep = "\t", header = T)
results <- readRDS("../../data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/nci-full-results-lens-compare-hlamajority.Rds")
results$summary$Tool <- factor(
  results$summary$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority", "arcasHLA", "HLAprofiler", "seq2HLA", "Optitype_ar", "Optitype_ad", "LENS-v1.8-consensus"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype-DNA-hlamajority", "nf-hlamajority", "arcasHLA", "HLAprofiler", "seq2HLA", "Optitype-RNA", "Optitype-DNA-LENS", "LENS-v1.8-consensus")
)
df <- read.csv("../../data/processed/cell-lines-after-polysolver-change/lens-majority-benchmark/nci-full-stats-lens-compare-hlamajority.csv")
df <- df %>% dplyr::filter(Tool != "Optitype_ad")
df <- df %>% dplyr::mutate(Accuracy = 100*Accuracy)
df$Gene <- factor(df$Gene, levels = c("Overall", "A", "B", "C"))
df$Tool <- factor(
  df$Tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority", "arcasHLA", "HLAprofiler", "seq2HLA", "Optitype_ar", "LENS-v1.8-consensus"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype-DNA", "nf-hlamajority", "arcasHLA", "HLAprofiler", "seq2HLA", "Optitype-RNA", "LENS-v1.8-consensus")
)

my_gene_labels <- c(
  "Overall" = "Overall Accuracy",
  "A"     = "HLA-A",
  "B"     = "HLA-B",
  "C"     = "HLA-C"
) 

# Figure: Accuracy per gene per tool

df.for.figure <- df %>%
  dplyr::select(Gene, Tool, Accuracy)

df.for.figure$Accuracy <- round(df.for.figure$Accuracy, 1)



df.for.figure <- df.for.figure %>%
  dplyr::mutate(
    Tool_ordered = tidytext::reorder_within(Tool, Accuracy, Gene)
  )



df.for.figure <- df.for.figure %>%
  dplyr::mutate(
    Tool_ordered = reorder_within(Tool, Accuracy, Gene)
  )

figure <- ggplot(df.for.figure, aes(x = Tool_ordered, y = Accuracy)) +
  
  geom_col(
    fill = "#4D4D4D",
    width = 0.7,
    color = "black",
    size = 0.2
  ) +
  
  facet_wrap(~Gene, scales = "free_x", ncol = 1,
             labeller = as_labeller(my_gene_labels)) +
  
  geom_text(
    aes(label = paste0(round(Accuracy, 1), "%")),
    vjust = -0.5,
    size = 5
  ) +
  
  scale_x_reordered() +
  
  scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 100, 25),
    expand = c(0,0)
  ) +
  
  labs(
    y = "Accuracy (%)",
    x = "Tool"
  ) +
  
  theme_bw() +
  coord_cartesian(ylim = c(0, 115)) +
  
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines")
  )
figure
ggsave(figure, filename = "../../results/lens-hlamajority-accuracy.svg", width = 14, height = 7)
ggsave(figure, filename = "../../results/lens-hlamajority-accuracy.png", width = 14, height = 7)
ggsave(figure, filename = "../../results/lens-hlamajority-accuracy.pdf", width = 12, height = 16)
# figure: concordance

make_concordance_matrix <- function(df_gene) {
  
  df <- df_gene %>%
    mutate(
      Tool1 = as.character(Tool1),
      Tool2 = as.character(Tool2),
      Tool_min = pmin(Tool1, Tool2),
      Tool_max = pmax(Tool1, Tool2)
    ) %>%
    select(Gene, Tool1 = Tool_min, Tool2 = Tool_max, Concordance) %>%
    distinct(Tool1, Tool2, .keep_all = TRUE)
  
  tools <- unique(c(df$Tool1, df$Tool2))
  
  diag_df <- data.frame(
    Gene = unique(df$Gene),
    Tool1 = tools,
    Tool2 = tools,
    Concordance = 1
  )
  
  bind_rows(df, diag_df)
}

make_concordance_matrix_for_weighted_mean <- function(df_gene) {
  df <- df_gene %>%
    mutate(
      Tool1 = as.character(Tool1),
      Tool2 = as.character(Tool2),
      Tool_min = pmin(Tool1, Tool2),
      Tool_max = pmax(Tool1, Tool2)
    ) %>%
    select(Gene, Tool1 = Tool_min, Tool2 = Tool_max, Concordance, N) %>%
    distinct(Tool1, Tool2, .keep_all = TRUE)
  return(df)
}

concordance_df <- results$concordance %>%
  mutate(pair = paste(pmin(Tool1, Tool2), pmax(Tool1, Tool2), sep = "_"))

concordance_clean <- concordance_df %>%
  filter(Tool1 != "Optitype_ad", Tool2 != "Optitype_ad") %>%
  mutate(
    Tool1 = gsub("optitype", "Optitype-DNA", Tool1),
    Tool2 = gsub("optitype", "Optitype-DNA", Tool2)
  ) %>%
  split(.$Gene) %>%
  lapply(make_concordance_matrix) %>%
  bind_rows()

tool_levels <- c(
  "kourami", "hlala", "polysolver", "Optitype-DNA",
  "hlamajority", "arcasHLA", "HLAprofiler", "seq2HLA",
  "Optitype_ar", "LENS-v1.8-consensus"
)

tool_labels <- c(
  "Kourami", "HLA*LA", "Polysolver", "Optitype-DNA",
  "nf-hlamajority", "arcasHLA", "HLAprofiler", "seq2HLA",
  "Optitype-RNA", "LENS-v1.8-consensus"
)

concordance_clean <- concordance_clean %>%
  mutate(
    Tool1 = factor(Tool1, levels = tool_levels, labels = tool_labels),
    Tool2 = factor(Tool2, levels = tool_levels, labels = tool_labels)
  ) %>%
  mutate(
    Tool1_chr = as.character(Tool1),
    Tool2_chr = as.character(Tool2)
  ) %>%
  mutate(
    Tool_min = ifelse(Tool1_chr < Tool2_chr, Tool1_chr, Tool2_chr),
    Tool_max = ifelse(Tool1_chr < Tool2_chr, Tool2_chr, Tool1_chr)
  ) %>%
  select(Gene, Tool1 = Tool_min, Tool2 = Tool_max, Concordance) %>%
  distinct(Tool1, Tool2, Gene, .keep_all = TRUE)

my_gene_labels_remove_total <- c(
  "A"     = "HLA-A",
  "B"     = "HLA-B",
  "C"     = "HLA-C"
) 
heatmap <- ggplot(concordance_clean, aes(Tool2, Tool1, fill = Concordance)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Concordance, 2)), size = 3) +
  facet_wrap(~Gene,
             labeller = as_labeller(my_gene_labels_remove_total)
             ) +
  scale_fill_gradient2(
    low = "blue", high = "red",
    limit = c(0,1),
    name = "Concordance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    legend.position = "right",
    # strip.background =element_rect(fill="grey"),
    # panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_rect(color = "black", fill = "grey90", linewidth = 1),
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines")
  )
heatmap
ggsave(heatmap, filename = "../../results/heatmap.svg", width = 14, height = 7)
ggsave(heatmap, filename = "../../results/heatmap.png", width = 14, height = 7)
ggsave(heatmap, filename = "../../results/heatmap.pdf", width = 14, height = 7)

concordance_df_clean_for_weighted <- concordance_df %>%
  filter(Tool1 != "Optitype_ad", Tool2 != "Optitype_ad") %>%
  mutate(
    Tool1 = gsub("optitype", "Optitype-DNA", Tool1),
    Tool2 = gsub("optitype", "Optitype-DNA", Tool2)
  ) %>%
  split(.$Gene) %>%
  lapply(make_concordance_matrix_for_weighted_mean) %>%
  bind_rows()

concordance_df_clean_for_weighted <- concordance_df_clean_for_weighted %>%
  mutate(
    Tool1 = factor(Tool1, levels = tool_levels, labels = tool_labels),
    Tool2 = factor(Tool2, levels = tool_levels, labels = tool_labels)
  ) %>%
  mutate(
    Tool1_chr = as.character(Tool1),
    Tool2_chr = as.character(Tool2)
  ) %>%
  mutate(
    Tool_min = ifelse(Tool1_chr < Tool2_chr, Tool1_chr, Tool2_chr),
    Tool_max = ifelse(Tool1_chr < Tool2_chr, Tool2_chr, Tool1_chr)
  ) %>%
  select(Gene, Tool1 = Tool_min, Tool2 = Tool_max, Concordance, N) %>%
  distinct(Tool1, Tool2, Gene, .keep_all = TRUE)

per_tool_concordance_weighted <- concordance_df_clean_for_weighted %>%
  tidyr::pivot_longer(
    cols = c(Tool1, Tool2),
    names_to = "role",
    values_to = "Tool"
  ) %>%
  group_by(Gene, Tool) %>%
  summarise(
    Mean_Concordance = weighted.mean(Concordance, w = N, na.rm = TRUE),
    Total_Comparisons = sum(N),
    N_pairs = n(),
    .groups = "drop"
  )
tool_labels_levels <- data.frame(Tool = tool_labels, levels = tool_levels)
per_tool_concordance_weighted_levels <- left_join(per_tool_concordance_weighted, tool_labels_levels)
 
# add accuracy
accuracy_df <- results$summary %>%
  dplyr::select(Gene, Tool, Accuracy) %>%
  dplyr::mutate(
    Accuracy = Accuracy * 100,
    levels = Tool
  ) %>%
  dplyr::select(-Tool) %>% 
  filter(levels != "Optitype_ad") %>% 
  mutate(levels = gsub(pattern = "optitype", replacement = "Optitype-DNA", x = levels))
  #left_join(per_tool_concordance_weighted_levels, by = c("Gene", "levels"))
  

per_tool_concordance_weighted_levels <- per_tool_concordance_weighted_levels %>%
                                        dplyr::left_join(accuracy_df, by = c("Gene", "levels")) %>% 
                                        mutate(Mean_Concordance = 100*Mean_Concordance)
# default is pearson correlation
per_tool_concordance_weighted_levels_cor <- per_tool_concordance_weighted_levels %>% 
                                            group_by(Gene) %>% 
                                            summarise(cor = cor(Accuracy, Mean_Concordance, use = "complete.obs"))

cor_labels <- per_tool_concordance_weighted_levels_cor %>%
  mutate(
    label = paste0("cor = ", round(cor, 3))
  )
gene_labels <- per_tool_concordance_weighted_levels_cor %>%
  mutate(label = paste0("HLA-", Gene)) %>% #, "\ncor = ", round(cor, 3))) %>%
  select(Gene, label) %>%
  deframe()  

weighted_concordance_per_tool_plot <- ggplot(per_tool_concordance_weighted,
       aes(x = reorder_within(Tool, Mean_Concordance, Gene), 
           y = 100*Mean_Concordance)) +
       # aes(x = reorder(Tool, Mean_Concordance),
       #     y = Mean_Concordance)) +
         scale_x_reordered() +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~Gene, 
             scales = "free_y",
             labeller = as_labeller(gene_labels)
  ) +
  labs(y = "Mean Concordance (%)", x = "Tool") +
  theme_minimal() +
  # theme(
  #   #axis.title = element_blank(),
  #   #panel.grid = element_blank(),
  #   axis.text = element_text(color = "black"),
  #   panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
  #   strip.background = element_rect(color = "black", fill = "grey90", linewidth = 1),
  #   strip.text = element_text(face = "bold", size = 12),
  #   panel.spacing = unit(1, "lines")
  # ) 
  theme(
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold", size = 16),
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold", size = 16),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )
weighted_concordance_per_tool_plot
ggsave(weighted_concordance_per_tool_plot, filename = "../../results/weighted-concordance-per-tool.svg", width = 14, height = 7)
ggsave(weighted_concordance_per_tool_plot, filename = "../../results/weighted-concordance-per-tool.png", width = 14, height = 7)
ggsave(weighted_concordance_per_tool_plot, filename = "../../results/weighted-concordance-per-tool.pdf", width = 14, height = 7)

summarise_gene_tool <- function(results, gene, tool) {
  
  x <- results$details[[gene]][[tool]]$error_types$Type
  
  tab <- table(x)
  
  data.frame(
    Gene  = gene,
    Tool  = tool,
    Type  = names(tab),
    Count = as.integer(tab),
    row.names = NULL
  )
}
tools <-  c("hlamajority", "LENS-v1.8-consensus")
genes <- c("A", "B", "C")
final_df <- do.call(
  rbind,
  lapply(genes, function(g) {
    do.call(
      rbind,
      lapply(tools, function(t) {
        summarise_gene_tool(results, g, t)
      })
    )
  })
)   

final_df_mismatches <- final_df |>
  dplyr::filter(Type != "Correct") |>
  group_by(Gene, Tool) |>
  mutate(
    Percent = Count / sum(Count) * 100
  ) |>
  ungroup()

final_df_mismatches$Type <- factor(final_df_mismatches$Type,
                                   levels = c("Dropout (Hetero -> Homo)", "Hallucination (Homo -> Hetero)", "Partial Mismatch", "Complete Mismatch"),
                                   labels = c("Dropout", "Allele Gain", "Partial Mismatch", "Complete Mismatch")
)
# 
# final_df_mismatches$Tool <- factor(final_df_mismatches$Tool,
#                                    levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
#                                    labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
# )
final_df_mismatches$Tool <- factor(final_df_mismatches$Tool,
                                   levels = c("LENS-v1.8-consensus", "hlamajority"),
                                   labels = c("LENS-v1.8-consensus", "nf-hlamajority")
)

palette_mistake_types <- c(
  Correct = "#016FB9",
  Dropout = "#22AED1",
  "Allele Gain" = "#6D8EA0",
  "Partial Mismatch" = "#AFA98D",
  "Complete Mismatch" = "#182825"
)

p_types_percent <- ggplot(
  final_df_mismatches,
  aes(x = Type, y = Percent, fill = Type)
) +
  geom_col( 
    width = 0.7,
    color = "black",
    linewidth = 0.2
  ) +
  facet_grid(
    Gene ~ Tool,
    scales = "fixed",
    labeller = labeller(Gene = my_gene_labels)
  ) +
  scale_y_continuous(
    expand = c(0, 0)
  ) +
  labs( 
    x = "Outcome type",
    y = "Percent of errors",
    fill = "Type"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    # axis.text.x = element_text(
    #   angle = 45,
    #   hjust = 1,
    #   colour = "black"
    # ),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.title = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_text(size =18),
    legend.text = element_text(size = 16)
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(values = palette_mistake_types)

p_types_percent

p_types_counts <- ggplot(
  final_df_mismatches,
  aes(x = Type, y = Count, fill = Type)
) +
  geom_col( 
    width = 0.7,
    color = "black",
    linewidth = 0.2
  ) +
  facet_grid(
    Gene ~ Tool,
    scales = "fixed",
    labeller = labeller(Gene = my_gene_labels)
  ) +
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    expand = c(0, 0)  
    ) +
  labs( 
    x = "Error type",
    y = "Number of errors",
    fill = "Type"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    # axis.text.x = element_text(
    #   angle = 45,
    #   hjust = 1,
    #   colour = "black"
    # ),
    axis.text.x = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.title = element_text(size = 18),
    legend.position = "bottom",
    legend.title = element_text(size =18),
    legend.text = element_text(size = 16),
    panel.spacing.y = unit(1, "lines")
  ) +
 # coord_cartesian(ylim = c(0, 10)) +
  scale_fill_manual(values = palette_mistake_types)

p_types_counts
ggsave(p_types_counts, filename = "../../results/error_types.svg", width = 10, height = 7)

top_tools <- per_tool_concordance_weighted_levels %>%
  group_by(Gene) %>%
  filter(Mean_Concordance == max(Mean_Concordance, na.rm = TRUE)) %>%
  select(Gene, Tool, Mean_Concordance)

top_tools_label <- top_tools %>%
  group_by(Gene) %>%
  summarise(
    top_tool = paste(Tool, collapse = ", "),
    top_acc = unique(Mean_Concordance),
    .groups = "drop"
  )

annotation_df <- per_tool_concordance_weighted_levels_cor %>%
  left_join(top_tools_label, by = "Gene") %>%
  mutate(
    label = paste0(
      "cor = ", round(cor, 2),
      " | Top concordance: ", top_tool,
      " (", round(top_acc, 2), "%)"
    )
  )


accuracy_vs_concordance <- ggplot(per_tool_concordance_weighted_levels, aes(x = Accuracy, y = Mean_Concordance)) +
  geom_point(size = 3, alpha = 0.8) +
  
  # facet_wrap(~Gene,
  #            labeller = as_labeller(my_gene_labels_remove_total)
  #            ) +
  facet_wrap(~Gene, labeller = as_labeller(gene_labels)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.5) +

  labs(
    y = "Mean Concordance (%)",
    x = "Accuracy (%)"
  ) +
  geom_label(
    data = annotation_df,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.03,
    vjust = 1.3,
    fill = "white",
    label.size = 0.2,
    size = 3
  ) +
  theme_minimal() +
  theme(
    #panel.grid = element_blank(),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 16),
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold", size = 16),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )
accuracy_vs_concordance
ggsave(accuracy_vs_concordance, filename = "../../results/accuracy_vs_concordance.svg", width = 14, height = 7)
ggsave(accuracy_vs_concordance, filename = "../../results/accuracy_vs_concordance.png", width = 14, height = 7)
ggsave(accuracy_vs_concordance, filename = "../../results/accuracy_vs_concordance.pdf", width = 14, height = 7)

df_results <- results$summary %>% dplyr::mutate(Call_Rate = 100*Call_Rate)
df_results$Gene <- factor(df_results$Gene, levels = c("Overall", "A", "B", "C"))
# df_results$Tool <- factor(
#   df_results$Tool,
#   levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
#   labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
# )

df_results <- df_results %>%
  dplyr::mutate(
    Tool_ordered = reorder_within(Tool, Call_Rate, Gene)
  )

p_call <- ggplot(df_results, aes(x = Tool_ordered, y = Call_Rate)) +
  
  geom_col(
    fill = "#4D4D4D",
    width = 0.7,
    color = "black",
    size = 0.2
  ) +
  
  facet_wrap(~Gene, scales = "free_x", ncol = 3,
             labeller = as_labeller(my_gene_labels)) +
  
  geom_text(
    aes(label = paste0(round(Call_Rate, 1), "%")),
    vjust = -0.5,
    size = 3.5
  ) +
  
  scale_x_reordered() +
  
  scale_y_continuous(
    limits = c(0, 120),
    breaks = seq(0, 100, 25),
    expand = c(0,0)
  ) +
  
  labs(
    y = "Call rate (%)",
    x = "Tool"
  ) +
  
  theme_bw() +
  coord_cartesian(ylim = c(0, 115)) +
  
  theme(
    strip.background = element_rect(fill = "#f0f0f0"),
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 12),
    axis.text.y = element_text(colour = "black"),
    panel.spacing = unit(0.8, "lines")
  )
# 4. Display Plot
print(p_call)
ggsave(p_call, filename = "../../results/call_rate.svg", width = 14, height = 7)
ggsave(p_call, filename = "../../results/call_rate.png", width = 14, height = 7)
ggsave(p_call, filename = "../../results/call_rate.pdf", width = 14, height = 7)

# plot false positives false negatives


