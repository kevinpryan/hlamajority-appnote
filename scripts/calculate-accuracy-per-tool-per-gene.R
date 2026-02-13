library(dplyr)
dat <- read.csv("../data/HLA typing-appnote-bmark-pergene-pertool-combo.csv") %>% dplyr::filter(!is.na(matching_calls_pcr))

hlala <- dat %>% dplyr::filter(tool == "hlala")
accuracy_hlala <- 100*(sum(hlala$matching_calls_pcr/sum(hlala$total_calls_for_accuracy)))
accuracy_hlala <- hlala %>% group_by(Gene) %>% summarise(accuracy = 100*(sum(matching_calls_pcr)/sum(total_calls_for_accuracy)))

calculate_accuracy_per_tool <- function(df, toolname){
  accuracy <- df %>% 
              dplyr::filter(tool == toolname) %>% 
              group_by(Gene) %>% 
              summarise(accuracy = 100*(sum(matching_calls_pcr)/sum(total_calls_for_accuracy)),
                        matching_calls_pcr_total = sum(matching_calls_pcr),
                        total_calls_for_accuracy_all_samples = sum(total_calls_for_accuracy)) %>% 
              mutate(Tool = toolname)
  return(accuracy)   
}
accuracy_majority <- calculate_accuracy_per_tool(df = dat, toolname = "hlamajority")
accuracy_optitype <- calculate_accuracy_per_tool(df = dat, toolname = "optitype")
accuracy_polysolver <- calculate_accuracy_per_tool(df = dat, toolname = "polysolver")
accuracy_hlala <- calculate_accuracy_per_tool(df = dat, toolname = "hlala")
accuracy_kourami <- calculate_accuracy_per_tool(df = dat, toolname = "kourami")
accuracy_combined <- rbind.data.frame(accuracy_majority, accuracy_optitype, accuracy_polysolver, accuracy_hlala, accuracy_kourami)

accuracy_total <- accuracy_combined %>% 
                  group_by(Tool) %>% 
                  summarise(matching_calls_pcr_total_all_genes = sum(matching_calls_pcr_total),
                            total_calls_for_accuracy_all_samples_all_genes = sum(total_calls_for_accuracy_all_samples)
                            ) %>% 
                  mutate(accuracy = 100*(matching_calls_pcr_total_all_genes/total_calls_for_accuracy_all_samples_all_genes)) %>% 
                  rename(matching_calls_pcr_total = matching_calls_pcr_total_all_genes,
                         total_calls_for_accuracy_all_samples = total_calls_for_accuracy_all_samples_all_genes) %>% 
                  ungroup()
accuracy_total$Gene <- "All_Genes"

accuracy_total <- accuracy_total[,colnames(accuracy_combined)]

accuracy_for_plotting <- rbind.data.frame(accuracy_total, accuracy_combined)

write.csv(accuracy_for_plotting, file = "../data/benchmark-12-nci60-all-results-per-gene-combined.csv", quote = F, row.names = F)
