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
pivot_longer(
  fp_ta_clean, cols = c(1:17), names_to ="term", values_to = "ordering") -> fp_ta_long 
fp_ta_long <- fp_ta_long %>%
  mutate(term = unlist(fp_ta_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_a_') %>%
           stringr::str_replace_all(
             pattern = '\\.', replacement = ' ')) %>%
  mutate(subject = "fp")

pivot_longer(
  hcp_ta_clean, cols = c(1:17), names_to ="term", values_to = "ordering") -> hcp_ta_long 
hcp_ta_long <- hcp_ta_long %>% 
  mutate(term = unlist(hcp_ta_long$term, recursive =TRUE) %>%  
           stringr::str_remove(pattern = 'task_a_') %>%
           stringr::str_replace_all(
             pattern = '\\.', replacement = ' ')) %>%
  mutate(subject = "hcp")

# Combine FP and HCP to analyze the differences between the ordering responses from two samples.
bind_rows(fp_ta_long, hcp_ta_long)  -> all_ta_long

# Add a column of the number of orderings for each term. 
all_ta_long %>%
  mutate(ordering = factor(ordering, levels = 1:16, ordered = TRUE)) %>% 
  group_by(subject, term, ordering, .drop = FALSE) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(!is.na(ordering)) -> all_order_n

# Log linear analysis for contingency table .
all_order_n %>%
  split(.$term) %>% 
  purrr::map_df(function(df){ 
    df %>%
      select(-term) %>%
      tidyr::pivot_wider(names_from = subject, values_from = n) %>%
      select(-ordering) %>%
      as.matrix() %>%
      MASS::loglm( ~ 1 + 2, data = .) -> x0
    data.frame(term = df$term[1], 
               X2_real = x0$lrt, 
               df = x0$df) 
  }) %>%
  mutate(p.value = pchisq(X2_real, df, lower.tail = FALSE))  -> fphcp_ta_x2_real

# Permutation
all_ta_long %>% 
  mutate( 
    id = paste0(subject,"_",id) 
  ) -> all_ta_long_per

# fphcp_ta_x2_100ksim <- replicate (
#   100000, simplify = FALSE,
# 
#     { all_ta_long_per %>%
#       group_by(id) %>%
#       summarise(subject = first(subject))  %>%
#       mutate(subject = sample(subject)) -> permute0
# 
#     all_ta_long_per %>%
#       mutate(subject = permute0$subject[match(x = id, permute0$id)]) -> permute_all_long
# 
#     permute_all_long %>%
#       mutate(ordering = factor(ordering, levels = 1:16, ordered = TRUE)) %>%
#       group_by(subject, term, ordering, .drop = FALSE) %>%
#       summarise(n = n(), .groups = 'drop') %>%
#       ungroup() %>%
#       filter(!is.na(ordering)) ->permute_order_n
# 
#     permute_order_n %>%
#       split(.$term) %>%
#       purrr::map_df(function(df){
# 
#           df %>%
#           select(-term) %>%
#           tidyr::pivot_wider(names_from = subject, values_from = n) %>%
#           select(-ordering) %>%
#           as.matrix() %>%
#           MASS::loglm( ~ 1 + 2, data = .) -> x0
#         data.frame(term = df$term[1], X2 = x0$lrt, df = x0$df)
# 
#       }) %>%
#       mutate(p.value = pchisq(X2, df, lower.tail = FALSE))
# 
#   }
# )

# Save the permutation results.
# save(fphcp_ta_x2_100ksim, file = "fphcp_ta_x2_100ksim.rda")
 
base::load(here::here("derived_data", "fphcp_ta_x2_100ksim.rda"))

purrr::map_df(1:length(fphcp_ta_x2_100ksim),\(i){
  select(fphcp_ta_x2_100ksim[[i]], term, X2) |>
    mutate(i = i)
}) -> fphcp_ta_x2_100ksim_terms

split(fphcp_ta_x2_100ksim_terms, fphcp_ta_x2_100ksim_terms$term) |>
  purrr::map_df(\(df){
    term0 = df$term[1]
    real = filter(fphcp_ta_x2_real, term == term0) |> pull(X2_real)
    c(term = term0,
      adj_p = mean(df$X2>real)
      )
  }) -> fphcp_ta_x2_sim_terms


fphcp_ta_x2_100ksim_terms |>
  ggplot(aes(x=X2))+
  geom_histogram()+
  geom_vline(data = fphcp_ta_x2_real,
              aes(xintercept=X2_real), color = 'red') +
  facet_wrap(~term) +
  ggtitle("Figure 1") +
  xlab("LRT statistic") +
  ylab("Permutation count") +
  theme_minimal()

fphcp_ta_x2_real %>%
  left_join(fphcp_ta_x2_sim_terms, by = "term") %>%
  rename(p.value_adjusted = adj_p) -> fphcp_ta_x2_real

# write.csv(fphcp_ta_x2_real, "derived_data/fphcp_ta_x2_real.csv", row.names=FALSE)

fphcp_ta_x2_100ksim %>%
  purrr::map_dbl(function(df){
    sum(df$X2)
  }) -> fphcp_ta_x2_sim

#Calculate total p-value.
sum(fphcp_ta_x2_real$X2_real) #sum of G^2 = 252.687
mean(sum(fphcp_ta_x2_real$X2_real) < fphcp_ta_x2_sim) #total p-value = 0.00032

hist(fphcp_ta_x2_sim, xlim=c(100, 300),
     xlab="LRT statistics",      
     ylab="Permutation count",      
     main="") 
abline(v=sum(fphcp_ta_x2_real$X2_real), col="red", lwd=2)
ci_95_lrt <- quantile(fphcp_ta_x2_sim, probs = 0.95)
abline(v=ci_95_lrt, col="red", lwd=2, lty=2)  
title("Figure 2", adj=0, font.main=1)

#Ordering descriptive figure.
hcp_tb_clean %>%
  colMeans(na.rm = TRUE) %>%
  sort(decreasing = TRUE) %>%
  names() %>%
  substring(8,100) %>%
  stringr::str_replace_all(
    pattern = '\\.', replacement = ' ') -> hcp_tb_term_ordered

all_ta_long %>%
  ggplot(aes(x = factor(term, levels=hcp_tb_term_ordered, ordered = TRUE),
             y = ordering, fill= subject)) +
  geom_hline(yintercept = seq(0, 16, by = 1), color = "lightgrey") +
  stat_histinterval(
    aes(side = ifelse (subject == "fp", "left", "right")),
    density = "histogram", point_size = 0.0001, show_interval = FALSE) +
  theme_ggdist() +
  theme(legend.position="top") +
  guides(fill=guide_legend("Group")) +
  scale_fill_manual(values = c("fp" = "darkgrey", "hcp" = "black"), labels = c("FP", "HCP")) + 
  xlab("Probability term")+
  ylab("Rank")+
  ggtitle("Descriptive Ordering comparison between HCP and FP") +
  coord_flip() +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  scale_y_continuous(breaks = 1:16, limits = c(1, 16)) -> ordering_f1

ggsave(ordering_f1, filename = here::here("Figures","ordering_f1.pdf"),
       height = 10, width = 10, dpi = 300)

# Between/ within variances comparison

split_term_ta <- split(all_ta_long, all_ta_long$term) 

anova_results_ta <- 
  map_df(split_term_ta, \(df){
      aov(ordering ~ subject, data = df)|>
      eta_squared(partial = FALSE) |>
      purrr::pluck('Eta2') -> eta2
    data.frame(term = df$term[1], eta2_ta = round(eta2,3))
    })

anova_results_ta |> summarise(
  min=min(eta2_ta),
  Q25 = quantile(eta2_ta,p=.25),
  median=median(eta2_ta),
  Q75 = quantile(eta2_ta,p=.75),
  max=max(eta2_ta)
  )

anova_results_ta |> 
  mutate(
    #rank = rank(eta2_ta),
    term = factor(term, levels = hcp_tb_term_ordered, ordered = TRUE)
  ) |>
  arrange(term) |>
  left_join(anova_results_tb, by = "term")
  



