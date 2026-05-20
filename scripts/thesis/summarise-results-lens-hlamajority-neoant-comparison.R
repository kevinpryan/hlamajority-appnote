# read in data
setwd("/hlamajority-paper/external/mhc_genotyping/")
dat <- read.csv("../../scripts/thesis/nf-neoantigen/results/combined/summaries_combined_out.csv")
dat_all <- read.csv("../../scripts/thesis/nf-neoantigen/results/combined/combined_stats_out.csv")
dat %>% group_by(Tool) %>% 
        summarise(total_n_fn = sum(n_fn)
                  #, 
                  #total_n_fp = sum(n_fp)
                  )
dat %>% group_by(Tool) %>% 
  summarise(
            total_n_fp = sum(n_fp)
  )
tes <- dat %>% dplyr::filter(Tool == "LENS-v1.8-consensus")
sum(tes$n_fp)

dat %>% group_by(Tool, HLA_gene) %>% 
  summarise(total_n_fn = sum(n_fn),
            total_n_fp = sum(n_fp),
            total_n_mis = sum(n_mis)
  )

dat_all_fp_fn <- dat_all %>% dplyr::filter(FP == TRUE | FN == TRUE)
peptide_length_per_tool <- dat_all_fp_fn %>% mutate(peptide_length = nchar(Peptide_mut)) %>%  group_by(peptide_length, Tool) %>% summarise(n_pep = n()) %>% arrange(-n_pep)

ggplot(peptide_length_per_tool, aes(fill=Tool, y=n_pep, x=peptide_length)) + 
  geom_bar(position="dodge", stat="identity")

mutations_summary <- dat_all_fp_fn %>%  group_by(Mutation, Tool) %>% summarise(n_pep = n()) %>% arrange(-n_pep)
ggplot(mutations_summary, aes(fill=Tool, y=n_pep, x=Mutation)) + 
  geom_bar(position="dodge", stat="identity")
