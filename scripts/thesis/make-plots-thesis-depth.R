#install.packages("ggrastr")
library(ggrastr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(vroom)
library(forcats)
library(tidyr)
library(ggpubr)
library(rstatix)
setwd("/hlamajority-paper/external/mhc_genotyping/")
# depth
depth <- vroom("../../data/raw/1000-genomes/majority/all_samples/combined_results/nf_hlamajority_depth_sorted.tsv")
scores <- read.csv("../../data/processed/1000-genomes/majority/1000genomes-score-per-sample.csv")
gs.na <- read.csv("../../data/processed/1000-genomes/majority/1000-genomes-gs-na-samples.csv")
# scores/depth cell lines
scores_cell_lines <- read.csv("../../data/processed/cell-lines-after-polysolver-change/majority/nci60-score-depth-per-sample-per-tool.csv")
palette_correct_incorrect <- c(Correct = "#05A8AA", Incorrect = "#DA2C38")

# depth
depth_scores <- scores %>%
  complete(gene, sample, tool) %>%   # create missing combinations
  left_join(depth, by = c("gene", "sample"))
# remove samples that are NA in the gold standard
depth_scores_rm_na <- depth_scores %>% dplyr::filter(
  !(sample == "NA12234" & gene == "HLA-C") &
    !(sample == "NA12249" & gene == "HLA-B") &
    !(sample == "NA18548" & gene == "HLA-C") &
    !(is.na(Score))
)
depth_scores_rm_na$correct_flag <- ifelse(depth_scores_rm_na$Score != 2, "Incorrect", "Correct")

depth_scores_rm_na$correct_flag <- factor(depth_scores_rm_na$correct_flag, levels = c("Incorrect", "Correct"))

depth_scores_rm_na$tool <- factor(
  depth_scores_rm_na$tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)

depth_scores_rm_na %>% group_by(tool, correct_flag) %>% summarise(median_depth = median(mean_depth_hla_exons_2_3_gene))

stat_test_per_tool_1000_genomes <- depth_scores_rm_na %>%
  group_by(tool) %>%
  wilcox_test(mean_depth_hla_exons_2_3_gene ~ correct_flag) %>%
  ungroup() %>%
  # 2. Adjust the p-values across all tests
  adjust_pvalue(method = "BH") %>%
  # 3. Add significance stars (optional, but nice for plots)
  add_significance("p.adj") %>%
  # 4. Get y-position for plotting the labels on the graph
  add_xy_position(x = "correct_flag", fun = "max", data = depth_scores_rm_na) %>% 
  mutate(
    p.adj.label = ifelse(
      p.adj < 0.001,
      formatC(p.adj, format = "e", digits = 2),
      formatC(p.adj, format = "f", digits = 3)
    )
  )
print(stat_test_per_tool_1000_genomes)

stat_test_gene_tool_1000_genomes <- depth_scores_rm_na %>%
  group_by(gene, tool) %>%
  wilcox_test(mean_depth_hla_exons_2_3_gene ~ correct_flag) %>%
  ungroup() %>%
  adjust_pvalue(method = "BH") %>%      # adjust across ALL tests
  add_significance("p.adj") %>%
  add_xy_position(
    x = "correct_flag",
    fun = "max",
    data = depth_scores_rm_na
  ) %>% 
  mutate(
    p.adj.label = ifelse(
      p.adj < 0.001,
      formatC(p.adj, format = "e", digits = 2),
      formatC(p.adj, format = "f", digits = 3)
    )
  )
stat_test_gene_tool_1000_genomes

p_1000_genomes_per_tool <- ggplot(depth_scores_rm_na, aes(x = correct_flag, y = mean_depth_hla_exons_2_3_gene, fill = correct_flag)) +
  geom_boxplot(outlier.shape = NA) +
  # geom_jitter(width = 0.2, alpha = 0.6) +
  ggrastr::geom_point_rast(
    position = position_jitter(width = 0.15),
    alpha = 0.3,
    size = 0.4,
    raster.dpi = 300
  ) +
  facet_wrap(~tool, scales = "free_y", nrow = 1) +
  # facet_grid(
  #   gene ~ tool,
  #   scales = "fixed"#,
  #   #labeller = labeller(gene = my_gene_labels)
  # ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "HLA Coverage by Tool and Call Accuracy (Per Tool)",
       subtitle = "Benjamini-Hochberg test adjusted p-values",
       x = "Call Type",
       y = "Mean Depth of HLA Exons 2 & 3") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 15, colour = "black"),
        axis.text.y = element_text(size = 13, colour = "black"),
        
        strip.text = element_text(size = 18, colour = "black")
  ) +
  stat_pvalue_manual(
    stat_test_per_tool_1000_genomes,
    label = "p.adj = {p.adj.label}, {p.adj.signif}",
    tip.length = 0.01,
    bracket.nudge.y = 0.05,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = palette_correct_incorrect) 
p_1000_genomes_per_tool


p_1000_genomes_gene_adjusted_gene_tool <- ggplot(depth_scores_rm_na, aes(x = correct_flag, y = mean_depth_hla_exons_2_3_gene, fill = correct_flag)) +
  geom_boxplot(outlier.shape = NA) +
  ggrastr::geom_point_rast(
    position = position_jitter(width = 0.15),
    alpha = 0.3,
    size = 0.4,
    raster.dpi = 300
  ) +
  # geom_jitter(width = 0.2, alpha = 0.6) +
  #facet_wrap(~tool, scales = "free_y") +
  facet_grid(
    gene ~ tool,
    scales = "fixed"#,
    #labeller = labeller(gene = my_gene_labels)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "HLA Coverage by Tool and Call Accuracy (Per Tool, Per Gene)",
       subtitle = "Benjamini-Hochberg test adjusted p-values",
       x = "Call Type",
       y = "Mean Depth of HLA Exons 2 & 3") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 15, colour = "black"),
        axis.text.y = element_text(size = 13, colour = "black"),
        
        strip.text = element_text(size = 18, colour = "black")
  ) +
  stat_pvalue_manual(
    stat_test_gene_tool_1000_genomes,
    label = "p.adj = {p.adj.label}, {p.adj.signif}",
    tip.length = 0.01,
    bracket.nudge.y = 0.05,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = palette_correct_incorrect) 

p_1000_genomes_gene_adjusted_gene_tool

# cell lines
scores_cell_lines <- scores_cell_lines %>% dplyr::filter(!is.na(Score))
scores_cell_lines$tool <- factor(
  scores_cell_lines$tool,
  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
  labels = c("Kourami", "HLA*LA", "Polysolver", "Optitype", "nf-hlamajority")
)
scores_cell_lines <- scores_cell_lines %>% dplyr::filter(!is.na(Score))
scores_cell_lines$correct_flag <- ifelse(scores_cell_lines$Score == 2, "Correct", "Incorrect")
scores_cell_lines$correct_flag <- factor(scores_cell_lines$correct_flag, levels = c("Incorrect", "Correct"))

stat_test_per_tool_cell_lines <- scores_cell_lines %>%
  group_by(tool) %>%
  wilcox_test(mean_depth_hla_exons_2_3_gene ~ correct_flag) %>%
  ungroup() %>%
  # 2. Adjust the p-values across all tests
  adjust_pvalue(method = "BH") %>%
  # 3. Add significance stars (optional, but nice for plots)
  add_significance("p.adj") %>%
  # 4. Get y-position for plotting the labels on the graph
  add_xy_position(x = "correct_flag", fun = "max", data = scores_cell_lines) %>% 
  mutate(
    p.adj.label = ifelse(
      p.adj < 0.001,
      formatC(p.adj, format = "e", digits = 2),
      formatC(p.adj, format = "f", digits = 3)
    )
  )
print(stat_test_per_tool_cell_lines)

stat_test_per_tool_per_gene_cell_lines <- scores_cell_lines %>%
  group_by(tool, gene) %>%
  wilcox_test(mean_depth_hla_exons_2_3_gene ~ correct_flag) %>%
  ungroup() %>%
  # 2. Adjust the p-values across all tests
  adjust_pvalue(method = "BH") %>%
  # 3. Add significance stars (optional, but nice for plots)
  add_significance("p.adj") %>%
  # 4. Get y-position for plotting the labels on the graph
  add_xy_position(x = "correct_flag", fun = "max", data = scores_cell_lines) %>% 
  mutate(
    p.adj.label = ifelse(
      p.adj < 0.001,
      formatC(p.adj, format = "e", digits = 2),
      formatC(p.adj, format = "f", digits = 3)
    )
  )
p_per_tool_cell_lines <- ggplot(scores_cell_lines, aes(x = correct_flag, y = mean_depth_hla_exons_2_3_gene, fill = correct_flag)) +
  geom_boxplot(outlier.shape = NA) +
  # geom_jitter(width = 0.2, alpha = 0.6) +
  ggrastr::geom_point_rast(
    position = position_jitter(width = 0.15),
    alpha = 0.3,
    size = 0.4,
    raster.dpi = 300
  ) +
  facet_wrap(~tool, scales = "free_y", nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "HLA Coverage by Tool and Call Accuracy (Per Tool)",
       subtitle = "Benjamini-Hochberg corrected p-values",
       x = "Call Type",
       y = "Mean Depth of HLA Exons 2 & 3 (Gene)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 15, colour = "black"),
        axis.text.y = element_text(size = 13, colour = "black"),
        
        strip.text = element_text(size = 18, colour = "black")
  ) +
  # Add the adjusted p-values and significance bars from our table
  stat_pvalue_manual(
    stat_test_per_tool_cell_lines,
    label = "p.adj = {p.adj.label}, {p.adj.signif}", # Custom label
    tip.length = 0.01,
    bracket.nudge.y = 0.05,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = palette_correct_incorrect) 

p_per_tool_cell_lines


p_per_tool_per_gene_cell_lines <- ggplot(scores_cell_lines, aes(x = correct_flag, y = mean_depth_hla_exons_2_3_gene, fill = correct_flag)) +
  geom_boxplot(outlier.shape = NA) +
  # geom_jitter(width = 0.2, alpha = 0.6) +
  ggrastr::geom_point_rast(
    position = position_jitter(width = 0.15),
    alpha = 0.3,
    size = 0.4,
    raster.dpi = 300
  ) +
  facet_wrap(~tool, scales = "free_y") +
  facet_grid(
    gene ~ tool,
    scales = "fixed"#,
    #labeller = labeller(gene = my_gene_labels)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "HLA Coverage by Tool and Call Accuracy (Per Tool, Per Gene)",
       subtitle = "Benjamini-Hochberg corrected p-values",
       x = "Call Type",
       y = "Mean Depth of HLA Exons 2 & 3 (Gene)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 15, colour = "black"),
        axis.text.y = element_text(size = 13, colour = "black"),
        
        strip.text = element_text(size = 18, colour = "black")
  ) +
  # Add the adjusted p-values and significance bars from our table
  stat_pvalue_manual(
    stat_test_per_tool_per_gene_cell_lines,
    label = "p.adj = {p.adj.label}, {p.adj.signif}", # Custom label
    tip.length = 0.01,
    bracket.nudge.y = 0.05,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = palette_correct_incorrect) 


print(p_per_tool_per_gene_cell_lines)

combine.1000genomes.depth <- ggarrange(
  p_1000_genomes_per_tool, 
  p_1000_genomes_gene_adjusted_gene_tool,
  ncol = 1,
  labels = c("A", "B"),
  font.label=list(color="black",size=28, face = "bold"),
  heights = c(1, 2)
)
combine.1000genomes.depth
ggsave(plot = combine.1000genomes.depth, filename = "/hlamajority-paper/results/thesis/plots/hlamajority-1000genomes-depth-20260527.svg", width = 12, height = 17, device = cairo_pdf)
ggsave(plot = combine.1000genomes.depth, filename = "/hlamajority-paper/results/thesis/plots/hlamajority-1000genomes-depth-20260527.pdf", width = 12, height = 17, device = cairo_pdf)
ggsave(plot = combine.1000genomes.depth, filename = "/hlamajority-paper/results/thesis/figures/hlamajority-1000genomes-depth-20260527.pdf", width = 12, height = 17, device = cairo_pdf)

combine.cell.lines.depth <- ggarrange(
  p_per_tool_cell_lines,
  p_per_tool_per_gene_cell_lines,
  ncol = 1,
  labels = c("A", "B"),
  font.label=list(color="black",size=28, face = "bold"),
  heights = c(1, 2)
)

ggsave(plot = combine.cell.lines.depth, filename = "/hlamajority-paper/results/thesis/plots/hlamajority-cell-lines-depth-20260527.svg", width = 12, height = 17, device = cairo_pdf)
ggsave(plot = combine.cell.lines.depth, filename = "/hlamajority-paper/results/thesis/plots/hlamajority-cell-lines-depth-20260527.pdf", width = 12, height = 17, device = cairo_pdf)
ggsave(plot = combine.cell.lines.depth, filename = "/hlamajority-paper/results/thesis/figures/hlamajority-cell-lines-depth-20260527.pdf", width = 12, height = 17, device = cairo_pdf)
