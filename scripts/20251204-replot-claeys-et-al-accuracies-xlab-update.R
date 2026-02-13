library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggpubr)
data_in_orig <- read.csv("../claeys-et-al/claeys-et-al-benchmarking-results.csv")
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

#ylim(95.0,100.0) 
mhci
mhci_legend_move <- ggplot(data_in_long, aes(fill=Feature, y=as.numeric(value), x=HLA_Allele)) + 
  geom_col(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) + # Keep this to remove bottom padding
  coord_cartesian(ylim=c(0,102)) +
  ylab("Accuracy (%)") +
  xlab("MHC Class I Gene") +
  scale_fill_tableau() +
  theme_bw() +
  scale_fill_discrete(c(""), labels = c("Best Individual Tool", "Metaclassifier")) +
  ggtitle("Claeys et al. 2023 metaclassifier performance") +
  theme(axis.title=element_text(size=20), 
        axis.text = element_text(size=16), 
        legend.text = element_text(size = 16), 
        legend.position = "left", 
        plot.title = element_text(hjust=0.5, size = 20)
        ) +
  geom_text(
    position = position_dodge(width = 0.9), # Matches the bar dodge width
    aes(
      label = paste(round(as.numeric(value), 1), "%", sep = ""),
      group = Feature
    ),
    vjust = -0.2, # Position text just above the bar
    size = 4
  )

#ylim(95.0,100.0) 
mhci_legend_move

mhci_legend_below <- ggplot(data_in_long, aes(fill=Feature, y=as.numeric(value), x=HLA_Allele)) + 
  geom_col(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) + # Keep this to remove bottom padding
  coord_cartesian(ylim=c(0,102)) +
  ylab("Accuracy (%)") +
  xlab("MHC Class I Gene") +
  scale_fill_tableau() +
  theme_bw() +
  scale_fill_discrete(c(""), labels = c("Best Individual Tool", "Metaclassifier")) +
  ggtitle("Claeys et al. 2023 metaclassifier performance") +
  theme(axis.title=element_text(size=20, face = "bold"), 
        axis.text = element_text(size=18), 
        legend.text = element_text(size = 20), 
        legend.position = "bottom", 
        plot.title = element_text(hjust=0.5, size = 25, face = "bold"), 
        legend.title = element_text(size = 20)
        ) +
  geom_text(
    position = position_dodge(width = 0.9), # Matches the bar dodge width
    aes(
      label = paste(round(as.numeric(value), 1), "%", sep = ""),
      group = Feature
    ),
    vjust = -0.2, # Position text just above the bar
    size = 4
  )

#ylim(95.0,100.0) 
mhci_legend_below

data_in_class2 <- data_in_orig[1:2,c(1,5:7)]
data_in_class2_long <- pivot_longer(data = data_in_class2, cols = c(2:4), names_to = "HLA_Allele")

mhcii <- ggplot(data_in_class2_long, aes(fill=Feature, y=as.numeric(value), x=HLA_Allele)) + 
  geom_col(position="dodge") +
  coord_cartesian(ylim=c(0,104)) +
  scale_y_continuous(expand = c(0, 0)) + # Keep this to remove bottom padding
  ylab("Accuracy (%)") +
  xlab("MHC Class II Gene") +
  scale_fill_tableau() +
  theme_bw() +
  scale_fill_discrete(c(""), labels = c("Best Individual Tool", "Metaclassifier")) +
  theme(
        axis.title=element_text(size=20), 
        axis.text = element_text(size=16), 
        legend.text = element_text(size = 16)
        ) +
  geom_text(
    position = position_dodge(width = 0.9), # Matches the bar dodge width
    aes(
      label = paste(round(as.numeric(value), 1), "%", sep = ""),
      group = Feature
    ),
    vjust = -0.2, # Position text just above the bar
    size = 4
  )
mhcii

plots_arranged <- ggarrange(mhci, mhcii, ncol = 2, common.legend = T)

plots_arranged_title <- annotate_figure(plots_arranged, top = text_grob("Claeys et al. 2023 metaclassifier performance", 
                                      color = "black", face = "bold", size = 20))

ggsave(plot = plots_arranged_title, filename = "../claeys-et-al/update-xlab/claeys-plots-adjust-yaxis.png", device = "png", 
                                    width = 30, 
                                    height = 15,
                                    units = "cm",
                                    dpi = 800)
ggsave(plot = plots_arranged_title, filename = "../claeys-et-al/update-xlab/claeys-plots-adjust-yaxis.pdf", width = 20, height = 10)
ggsave(plot = plots_arranged_title, filename = "../claeys-et-al/update-xlab/claeys-plots-adjust-yaxis.svg", width = 20, height = 10)
