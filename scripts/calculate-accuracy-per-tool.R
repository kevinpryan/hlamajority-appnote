library(dplyr)
dat <- read.csv("../data/sanantonio-benchmark-sampledata-nci60-per-tool.csv") %>% dplyr::filter(!is.na(matching_calls_pcr))
hlala <- dat %>% dplyr::filter(tool == "hlala")
accuracy_hlala <- 100*(sum(hlala$matching_calls_pcr/sum(hlala$total_calls_for_accuracy)))
kourami <- dat %>% dplyr::filter(tool == "kourami") 
accuracy_kourami <- 100*(sum(kourami$matching_calls_pcr/sum(kourami$total_calls_for_accuracy)))
polysolver <- dat %>% dplyr::filter(tool == "polysolver") 
accuracy_polysolver <- 100*(sum(polysolver$matching_calls_pcr/sum(polysolver$total_calls_for_accuracy)))
optitype <- dat %>% dplyr::filter(tool == "optitype") 
accuracy_optitype <- 100*(sum(optitype$matching_calls_pcr/sum(optitype$total_calls_for_accuracy)))

majority_results <- read.csv("../data/nci60-12sample-hlamajority-benchmark-results.csv")
accuracy_majority <- 100*sum(majority_results$matching_calls)/sum(majority_results$calls_pcr)

results <- data.frame(accuracy_optitype = accuracy_optitype, accuracy_polysolver = accuracy_polysolver, accuracy_kourami = accuracy_kourami, accuracy_hlala = accuracy_hlala, accuracy_majority = accuracy_majority)

write.csv(results, file = "../data/benchmark-12-nci60-results.csv", quote = F, row.names = F)