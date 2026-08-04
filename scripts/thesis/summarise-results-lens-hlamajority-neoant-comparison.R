library(dplyr)
# read in data
setwd("/hlamajority-paper/external/mhc_genotyping/")
stats <- read.csv("../../scripts/thesis/nf-neoantigen/results/combined/combined_stats_out.csv")
summaries <- read.csv("../../scripts/thesis/nf-neoantigen/results/combined/summaries_combined_out.csv")
sum(stats$n_binder_gain)
stats %>% group_by(Tool) %>% summarise(
                             total_binder_gain = sum(n_binder_gain),
                             total_binder_loss = sum(n_binder_loss),
                             total_strong_binder_gain = sum(n_strong_binder_gain),
                             total_strong_binder_loss = sum(n_strong_binder_loss)
                             )

stats %>% group_by(Tool, HLA_gene) %>% summarise(
  total_binder_gain = sum(n_binder_gain),
  total_binder_loss = sum(n_binder_loss),
  total_strong_binder_gain = sum(n_strong_binder_gain),
  total_strong_binder_loss = sum(n_strong_binder_loss)
)

plot_df <- stats %>%
  group_by(Tool) %>%
  summarise(
    binder_gain = sum(n_binder_gain),
    binder_loss = sum(n_binder_loss),
    strong_binder_gain = sum(n_strong_binder_gain),
    strong_binder_loss = sum(n_strong_binder_loss)
  ) %>%
  tidyr::pivot_longer(
    cols = -Tool,
    names_to = "event",
    values_to = "count"
  )
library(ggplot2)
ggplot(plot_df, aes(x = Tool, y = count, fill = event)) +
  geom_col(position = "dodge")

# plot_df_per_gene <- stats %>%
#   group_by(Tool, HLA_gene) %>%
#   summarise(
#     binder_gain = sum(n_binder_gain),
#     binder_loss = sum(n_binder_loss),
#     strong_binder_gain = sum(n_strong_binder_gain),
#     strong_binder_loss = sum(n_strong_binder_loss)
#   ) %>%
#   tidyr::pivot_longer(
#     cols = -Tool,
#     names_to = "event",
#     values_to = "count"
#   )
library(ggplot2)
ggplot(plot_df, aes(x = Tool, y = count, fill = event)) +
  geom_col(position = "dodge")

binder_gains <- summaries %>% dplyr::filter(binder_gain == TRUE)
binder_losses <- summaries %>% dplyr::filter(binder_loss == TRUE)
strong_gain <- summaries %>% dplyr::filter(strong_binder_gain == TRUE)
# stats %>% group_by(Tool) %>% 
#         summarise(total_n_fn = sum(n_fn)
#                   #, 
#                   #total_n_fp = sum(n_fp)
#                   )
# dat %>% group_by(Tool) %>% 
#   summarise(
#             total_n_fp = sum(n_fp)
#   )
# tes <- dat %>% dplyr::filter(Tool == "LENS-v1.8-consensus")
# sum(tes$n_fp)

# dat %>% group_by(Tool, HLA_gene) %>% 
#   summarise(total_n_fn = sum(n_fn),
#             total_n_fp = sum(n_fp),
#             total_n_mis = sum(n_mis)
#   )

dat_all_fp_fn <- dat_all %>% dplyr::filter(FP == TRUE | FN == TRUE)
peptide_length_per_tool <- dat_all_fp_fn %>% mutate(peptide_length = nchar(Peptide_mut)) %>%  group_by(peptide_length, Tool) %>% summarise(n_pep = n()) %>% arrange(-n_pep)

ggplot(peptide_length_per_tool, aes(fill=Tool, y=n_pep, x=peptide_length)) + 
  geom_bar(position="dodge", stat="identity")

mutations_summary <- dat_all_fp_fn %>%  group_by(Mutation, Tool) %>% summarise(n_pep = n()) %>% arrange(-n_pep)
ggplot(mutations_summary, aes(fill=Tool, y=n_pep, x=Mutation)) + 
  geom_bar(position="dodge", stat="identity")
