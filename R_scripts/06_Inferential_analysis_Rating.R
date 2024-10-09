library(tidyverse)
library(tidyr)
library(haven) 
library(dplyr)
library(gdata) 
library(renv)
library(ggplot2)
library(distributional)
library(ggdist)
library(cowplot)
library(patchwork)
library(reshape2)
library(magrittr)
library(car)  
library(effectsize)
set.seed(10)

# Load data
source(here::here("R_scripts", "00_FP data loaded.R")) 
source(here::here("R_scripts", "00_HCP data loaded.R")) 
source(here::here("R_scripts", "01_FP clean data.R"))  
source(here::here("R_scripts", "02_HCP clean data.R")) 

# Organize data format to long data set. 
pivot_longer(fp_tb_clean, -id, names_to="term", values_to = "number") -> fp_tb_long
fp_tb_long %>%
  mutate(term = unlist(fp_tb_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_b_') %>%
           stringr::str_replace_all(
             pattern = '\\.', replacement = ' ')) %>%
  mutate(subject = "fp") -> fp_tb_long

pivot_longer(hcp_tb_clean, -id, names_to="term", values_to = "number") -> hcp_tb_long
hcp_tb_long %>%
  mutate(term = unlist(hcp_tb_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_b_') %>%
           stringr::str_replace_all(
             pattern = '\\.', replacement = ' ')) %>%
  mutate(subject = "hcp") -> hcp_tb_long

# Combine FP and HCP to analyze the differences between the rating responses from two samples.
bind_rows(fp_tb_long, hcp_tb_long) -> all_tb_long

# KS analysis for data between HCP & FP.
all_tb_long %>%
  split(.$term) %>% 
  purrr::map_df (function(df) {
    ks.test(number~subject, data = df) -> x0
    data.frame(term = df$term[1], 
               D = x0$statistic, 
               p.value = x0$p.value) 
  }) -> fphcp_tb_ks_real

#' Permutation 
all_tb_long %>% 
  mutate( 
    id = paste0(subject,"_",id) 
  ) -> all_tb_long_per

# fphcp_tb_ks_100ksim <- replicate (
#   100000, simplify = FALSE,
# 
#   { all_tb_long_per %>%
#       group_by(id) %>%
#       summarise(subject = first(subject))  %>%
#       mutate(subject = sample(subject)) -> permuteb
# 
#     all_tb_long_per %>%
#       mutate(subject = permuteb$subject[match(x = id, permuteb$id)]) -> permute_allb_long
# 
#     permute_allb_long %>%
#       split(.$term) %>%
#       purrr::map_df(function(df){
#         ks.test(number~subject, data = df) -> x0
# 
#         data.frame(term = df$term[1], D = x0$statistic, pvalue = x0$p.value) })
# 
#   }
# 
# )
# 
# Save the permutation results.
# save(fphcp_tb_ks_100ksim, file = "fphcp_tb_ks_100ksim.rda")

base::load(here::here("derived_data", "fphcp_tb_ks_100ksim.rda"))

purrr::map_df(1:length(fphcp_tb_ks_100ksim),\(i){
  select(fphcp_tb_ks_100ksim[[i]], term, pvalue) |>
    mutate(i = i)
}) -> fphcp_tb_ks_100ksim_terms

split(fphcp_tb_ks_100ksim_terms, fphcp_tb_ks_100ksim_terms$term) |>
  purrr::map_df(\(df){
    term0 = df$term[1]
    real = filter(fphcp_tb_ks_real, term == term0) |> pull(p.value)
    c(term = term0,
      adj_p = mean(df$pvalue<real)
    )
  }) -> fphcp_tb_ks_sim_terms

fphcp_tb_ks_100ksim_terms |>
  ggplot(aes(x=pvalue))+
  geom_histogram()+
  geom_vline(data = fphcp_tb_ks_real,
             aes(xintercept=p.value), color = 'red') +
  facet_wrap(~term) +
  ggtitle("Figure 3") +
  xlab("KS test statistic") +
  ylab("Permutation count") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 0.5, 1)) 


fphcp_tb_ks_real %>%
  left_join(fphcp_tb_ks_sim_terms, by = "term") %>%
  rename(p.value_adjusted = adj_p) -> fphcp_tb_ks_real

# write.csv(fphcp_tb_ks_real, "derived_data/fphcp_tb_ks_real.csv", row.names=FALSE)

fphcp_tb_ks_100ksim %>%
  purrr::map_dbl(function(df){
    sum(-2*log(df$pvalue))
  }) -> fphcp_tb_ks_sim

#Calculate p-value.
sum(-2*log(fphcp_tb_ks_real$p.value)) # Fisher's method of sum of D = 190.7055
mean(sum(-2*log(fphcp_tb_ks_real$p.value)) < fphcp_tb_ks_sim) # 0

hist(fphcp_tb_ks_sim, xlim=c(-10,200),
     xlab = "KS test statistic",      
     ylab = "Permutation count",
     main = "") 
abline(v=sum(-2*log(fphcp_tb_ks_real$p.value)), col="red", lwd=2)
ci_95_ks <- quantile(fphcp_tb_ks_sim, probs = 0.95)
abline(v=ci_95_ks, col="red", lwd=2, lty=2)  
title("Figure 4", adj=0, font.main=1)

#Rating descriptive figure.
hcp_tb_clean %>%
  select(-id) %>%
  colMeans(na.rm = TRUE) %>%
  sort(decreasing = TRUE) %>%
  names() %>%
  substring(8,100) %>%
  stringr::str_replace_all(
    pattern = '\\.', replacement = ' ') -> hcp_tb_term_ordered

all_tb_long |>
  mutate(term = factor(term, levels=hcp_tb_term_ordered, ordered = TRUE)) |>
ggplot(mapping = aes(x= term,
        y = number, fill=subject)) +
  geom_hline(yintercept = seq(0, 100, by = 5), color = "lightgrey") +
  geom_boxplot(outlier.shape = NA) +
  theme_ggdist() +
  theme(legend.position="top")+
  guides(fill=guide_legend("Group")) +
  scale_fill_manual(values = c("fp" = "darkgray", "hcp" = rgb(.9,.9,.9)), labels = c("FP", "HCP")) + 
  xlab("Probability term")+
  ylab("Rating")+
  ggtitle("Descriptive Rating comparison between HCP and FP") +
  coord_flip() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) -> rating_f2

ggsave(rating_f2, filename = here::here("Figures", "rating_f2.pdf"),
 height = 10, width = 10, dpi = 300)

#' Between/ within variances comparison

split_term_tb <- split(all_tb_long, all_tb_long$term) 

anova_results_tb <- 
  map_df(split_term_tb, \(df){
    aov(number ~ subject, data = df)|>
      eta_squared(partial = FALSE) |>
      purrr::pluck('Eta2') -> eta2
    data.frame(term = df$term[1], eta2_tb = round(eta2, 3))
  })

anova_results_tb |> summarise(
  min=min(eta2_tb),
  Q25 = quantile(eta2_tb,p=.25),
  median=median(eta2_tb),
  Q75 = quantile(eta2_tb,p=.75),
  max=max(eta2_tb)
)

anova_results_tb |> 
  mutate(
    rank = rank(eta2_tb),
    term = factor(term, levels = hcp_tb_term_ordered, ordered = TRUE)
  ) |>
  arrange(term)

anova_results_tb |> 
  mutate(
    rank = rank(eta2_tb),
    term = factor(term, levels = hcp_tb_term_ordered, ordered = TRUE)
  ) |>
  ggplot(aes(x=term, y=eta2)) + geom_point() +
  coord_flip() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15))