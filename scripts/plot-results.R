library(ggplot2)
library(ggthemes)
df <- read.csv("../data/benchmark-12-nci60-all-results-per-gene-combined.csv")
df$Gene <- factor(df$Gene, levels = c("All_Genes", "HLA-A", "HLA-B", "HLA-C"))
df$Tool <- factor(df$Tool, 
                  levels = c("kourami", "hlala", "polysolver", "optitype", "hlamajority"),
                  labels = c("Kourami", "HLA-LA", "Polysolver", "Optitype", "nf-hlamajority")
)

my_gene_labels <- c(
  "All_Genes" = "Overall Accuracy",
  "HLA-A"     = "HLA-A",
  "HLA-B"     = "HLA-B",
  "HLA-C"     = "HLA-C"
)

p <- ggplot(df, aes(x = Tool, y = accuracy, fill = Tool)) +
  
  # Create bars
  geom_col(position = position_dodge(), width = 0.7, color = "black", size = 0.2) +
  
  # Facet by Gene
  facet_wrap(~Gene, scales = "fixed", 
             ncol = 2,
             labeller = as_labeller(my_gene_labels)) +
  
  # Add text labels on top of bars (rounded to 1 decimal)
  geom_text(#aes(label = sprintf("%.1f", accuracy)), 
            aes(label = paste(round(accuracy, 1), "%", sep = "")),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 6) +
  
  # Colors: Highlight Hlamajority (assuming it's the last factor level)
  # You can customize these colors. 
  # Here: Greys for others, Red/Blue for Hlamajority
  scale_fill_manual(values = c("#999999", "#999999", "#999999", "#999999", "#E69F00")) +
  
  # Scales
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 100, 25), expand = c(0,0)) +
  
  # Labels
  labs(
       #title = "Genotyping Accuracy by Tool and Gene",
       #subtitle = "Comparison with NCI-60 WES Dataset",
       y = "Accuracy (%)",
       x = "Tool",
       fill = "Tool") +
  
  # Theme customization
  theme_bw() +
  coord_cartesian(ylim = c(0, 112)) +     # Set the visible limits here
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x labels
    strip.background = element_rect(fill = "#f0f0f0"), # Facet header background
    strip.text = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    legend.position = "none", # Hide legend since x-axis has labels
    axis.title = element_text(size = 18), 
    axis.text = element_text(size = 16), 
    axis.text.x = element_text(angle = 45, hjust = 1, colour = "black"),
    axis.text.y = element_text(colour = "black")
  )

# 4. Display Plot
print(p)

# ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene.png")
# ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene.svg")
# ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene.pdf")

ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene-mod.png")
ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene-mod.svg")
ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene-mod.pdf")

ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene-mod-20251222.png", dpi = 350)
ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene-mod-20251222.svg", dpi = 350, width = 9.28, height = 8.79)
ggsave(p, file = "../figures/redo-figure-20251222/benchmark-per-tool-per-gene-mod-20251222-v2.svg", dpi = 350, width = 12.28, height = 10.79)

ggsave(p, file = "../plots/benchmark-results/benchmark-per-tool-per-gene-mod-20251222.pdf", dpi = 350)

# colnames(results) <- c("Optitype", "Polysolver", "Kourami", "HLA-LA", "nf-hlamajority")
# results <- as.data.frame(t(results))
# results$Tool <- rownames(results)
# palette_tool <- c(Optitype = "#CBE896", Polysolver = "#2D3142", "Kourami" = "#BEB7A4", "HLA-LA" = "#FF7F11", "nf-hlamajority" = "#4E6E58")
# 
# 
# outfile 
#   #             position = position_dodge(width = 0.9),
#   #             vjust = -0.5,
#   #             size = 4)
#   
# outfile <- ggplot(results, aes(x = reorder(Tool, V1), y = V1, fill = Tool)) + 
#   geom_col() +
#   ylab("Accuracy (%)") +
#   xlab("Method") +
#   # Use the fill aesthetic you just defined
#   scale_fill_manual(values = palette_tool) +
#   theme_bw() +
#   # Set y-axis limits and remove padding at the bottom
#   scale_y_continuous(expand = c(0, 0)) + # Keep this to remove bottom padding
#   coord_cartesian(ylim = c(0, 101)) +     # Set the visible limits here
#   theme(
#     axis.title = element_text(size = 18), 
#     axis.text = element_text(size = 14), 
#     # Hide the legend as the x-axis labels are sufficient
#     legend.position = "none" ,
#     axis.text.x = element_text(angle = 45, hjust = 1)
#     
#   ) +
#   # Optional: Add text labels on top of the bars
#   geom_text(
#     aes(label = paste(round(V1, 1), "%", sep = "")),
#     vjust = -0.2, # Position text just above the bar
#     size = 4
#   )
# 
# ggsave(outfile, file = "../plots/benchmark-results/benchmark-per-tool.png")
# ggsave(outfile, file = "../plots/benchmark-results/benchmark-per-tool.svg")
# ggsave(outfile, file = "../plots/benchmark-results/benchmark-per-tool.pdf")
# 
