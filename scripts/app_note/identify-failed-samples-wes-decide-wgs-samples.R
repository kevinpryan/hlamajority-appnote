library(dplyr)
setwd("/hlamajority-paper/external/mhc_genotyping/")
results <- readRDS("../../data/processed/1000-genomes/majority/1000-genomes-full-results-hlamajority-majority-vote.Rds")
hla_a_error_samples <- results$details$A$hlamajority$error_types %>% dplyr::filter(Type != "Correct") %>% dplyr::select(Sample) %>% pull()
hla_b_error_samples <- results$details$B$hlamajority$error_types %>% dplyr::filter(Type != "Correct") %>% dplyr::select(Sample) %>% pull()
hla_c_error_samples <- results$details$C$hlamajority$error_types %>% dplyr::filter(Type != "Correct") %>% dplyr::select(Sample) %>% pull()
hla_a_error_samples
hla_b_error_samples
hla_c_error_samples
all_error_samples <- unique(c(hla_a_error_samples, hla_b_error_samples, hla_c_error_samples))
length(unique(all_error_samples))
# 52
urls <- read.table("../../data/1000genomes-wgs-30x-crams.txt")
urls$sample <- sub("\\.final\\.cram$", "", basename(urls$V1))
urls$sample
# how many of the failed samples actually have a matched 30X WGS sample?
length(intersect(all_error_samples, urls$sample))
# get these samples
all_error_samples_with_wgs <- all_error_samples[which(all_error_samples %in% urls$sample)]
# 49
# look at crams downloaded so far
crams_20260716 <- read.table("../../data/crams-downloaded-20260716.txt")
crams_20260716$sample <- sub("\\.final\\.cram$", "", basename(crams_20260716$V1))
length(intersect(crams_20260716$sample, all_error_samples_with_wgs))
files_to_download <- all_error_samples_with_wgs[which(!(all_error_samples_with_wgs %in% crams_20260716$sample))]
urls_to_redownload <- urls %>% dplyr::filter(sample %in% files_to_download) %>% dplyr::select(V1)
write.table(urls_to_redownload, file = "../../data/urls-20260716.txt", quote = F, col.names = F, row.names = F)
urls_to_redownload
# # look at crams downloaded so far 20260717
crams_20260717 <- read.table("../../data/crams-downloaded-20260717.txt")
crams_20260717$sample <- sub("\\.final\\.cram$", "", basename(crams_20260717$V1))
length(intersect(crams_20260717$sample, all_error_samples_with_wgs))
files_to_download <- all_error_samples_with_wgs[which(!(all_error_samples_with_wgs %in% crams_20260717$sample))]
urls_to_redownload <- urls %>% dplyr::filter(sample %in% files_to_download) %>% dplyr::select(V1)
write.table(urls_to_redownload, file = "../../data/urls-20260717.txt", quote = F, col.names = F, row.names = F)

# given the non-failing samples we have already downloaded, choose 100 to test WGS
crams_20260717_no_fail <- crams_20260717 %>% dplyr::filter(!sample %in% all_error_samples_with_wgs)
# 604 samples
#read in table with ancestry information
hla_diversity <- read.table("../../data/igsr_samples (1).tsv", header = T, sep = "\t")
table(hla_diversity$Population.code)
table(hla_diversity$Sample.name)

hla_diversity <- hla_diversity %>% dplyr::select(Sample.name, Population.code, Superpopulation.code)
names(hla_diversity)[1] <- "sample"
crams_20260717_no_fail_diversity <- left_join(crams_20260717_no_fail, hla_diversity)
superpop_table <- table(crams_20260717_no_fail_diversity$Superpopulation.code)
table(crams_20260717_no_fail_diversity$Population.code)
proportional_sample_numbers <- (superpop_table/sum(superpop_table))*100
# > proportional_sample_numbers

#AFR      AMR      EAS      EUR 
#25.49669 19.37086 25.16556 29.96689

proportional_sample_numbers <- round((superpop_table/sum(superpop_table))*100, digits = 0)

# > proportional_sample_numbers

#AFR AMR EAS EUR 
#25  19  25  30 
sum(proportional_sample_numbers)
# [1] 99

proportional_sample_numbers <- c(26,19,25,30)
names(proportional_sample_numbers) <- c("AFR", "AMR", "EAS", "EUR")
sum(proportional_sample_numbers)
#[1] 100

set.seed(42)

sample_sizes <- tibble(
  Superpopulation.code = c("AFR", "AMR", "EAS", "EUR"),
  n_select = c(26, 19, 25, 30)
)

selected_100 <- crams_20260717_no_fail_diversity %>%
  left_join(sample_sizes, by = "Superpopulation.code") %>%
  group_by(Superpopulation.code) %>%
  group_modify(~ slice_sample(.x, n = unique(.x$n_select))) %>%
  ungroup() %>%
  select(-n_select)

write.csv(
  selected_100,
  "../../data/selected_100_1000G_WGS_benchmark_samples.csv",
  row.names = FALSE
)

selected_100_samplesheet <- selected_100 %>% dplyr::select(sample, V1)
colnames(selected_100_samplesheet) <- c("sample","aln")
selected_100_samplesheet_annot <- selected_100_samplesheet
selected_100_samplesheet_annot$pass_fail_wes <- "PASS"
paths_failed_samples <- paste("/data3/kryan/nextflow-cache/1000-genomes-crams-wgs-30x/", all_error_samples_with_wgs, ".final.cram", sep = "")
samplesheet_failed_samples <- data.frame(
  sample = all_error_samples_with_wgs,
  aln = paths_failed_samples
)
samplesheet_failed_samples_annot <- samplesheet_failed_samples
samplesheet_failed_samples_annot$pass_fail_wes <- "FAIL"
samplesheet_failed_samples
samplesheet_combined <- rbind.data.frame(selected_100_samplesheet, samplesheet_failed_samples)
samplesheet_combined_annot <- rbind.data.frame(selected_100_samplesheet_annot, samplesheet_failed_samples_annot)
write.csv(samplesheet_combined, "../../data/samplesheet-149-wgs-samples.csv", quote = F, row.names = F)
write.csv(samplesheet_combined_annot, "../../data/samplesheet-149-wgs-samples-pass-fail.csv", quote = F, row.names = F)
