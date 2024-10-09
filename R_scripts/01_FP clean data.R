#+ echo = FALSE, message =FALSE
library(tidyverse)
library(tidyr)
library(haven) 
library(dplyr)
library(data.table) 
library(here)
library(data.table) 
library(ggplot2)
library(ggExtra)
library(magrittr)


#' Excluded participants who did not provide data or did not finish the task within data collection duration.
#+ echo=TRUE, message=FALSE, warning = FALSE
source(here::here("R_scripts", "00_FP data loaded.R")) 


#' Next, we spotted participants who did not follow the instruction in the Ordering task.
#+ echo=TRUE, message=FALSE, warning = FALSE
source(here::here("R_scripts", "03a_FP Ordering categorization.R")) 


#' Given the categorization, we excluded participants given the MLE results.
#+ echo=TRUE, message = FALSE, warning = FALSE
source(here::here("R_scripts", "03b_FP Ordering exclusion criterion.R"))

#' And we excluded participants based on the Rating exclusion criterion.
#+ echo=TRUE, message = FALSE, warning = FALSE
source(here::here("R_scripts", "03c_FP Rating exclusion criterion.R"))

#' Ordering responses exclusion. 

#+ echo=TRUE, message = FALSE, warning = FALSE

# Reverse reversers' answers. 
subset(fp_ta, (id %in% c(fp_ta_reversers$rev_id))) -> fp_ta_rev

fp_ta_rev %>%
  select(-id, -itemString)-> fp_ta_rev_cal
as.data.frame(fp_ta_rev_cal * (-1) + 17) %>%
  add_column(id = fp_ta_rev$id)-> fp_ta_rev_correct

# Subset the Compliers in Ordering task. 
subset(
  fp_ta, 
  !(id %in% 
      c(fp_ta_randomizers$ran_id, 
        fp_ta_reversers$rev_id))) %>%
  select(-itemString) -> fp_ta_com

fp_ta_com %>%
  rbind(fp_ta_rev_correct)  -> fp_ta_com_rev

subset(
  fp_ta_com_rev, 
  !(id %in% 
      c(# Excluded respondents by Rating exclusion criterion in Ordering responses. 
        fp_tb_id$below_mid_id))) -> fp_ta_clean

#' Rating responses exclusion. 

#+ echo=TRUE, message = FALSE, warning = FALSE

subset(
  fp_tb, 
  !(id %in% 
      c(fp_ta_randomizers$ran_id, 
        fp_tb_id$below_mid_id))) -> fp_tb_clean

#' Save the clean data. 
# write.csv(fp_ta_clean, "derived_data/fp_ta_clean.csv", row.names=FALSE)
# write.csv(fp_tb_clean, "derived_data/fp_tb_clean.csv", row.names=FALSE)

# Descriptive 
# FPs who did only Ordering task and excluded by being randomizers. 
# length(intersect(fp_onlydid_ordering, fp_ta_randomizers$ran_id)) # N = 17 
# 
# # FPs who did only Rating task and excluded by rating exclusion criterion. 
# length(intersect(fp_onlydid_rating, fp_tb_id$below_mid_id)) # N = 0
# 
# # FPs who did both judgement tasks and excluded by being randomizers. 
# setdiff(fp_ta_randomizers$ran_id,fp_tb_id$below_mid_id) -> excluded_by_ord
# length(intersect(fp_did_both, excluded_by_ord)) # N = 11
# 
# # FPs who did both judgement tasks and excluded by rating criterion. 
# setdiff(fp_tb_id$below_mid_id,fp_ta_randomizers$ran_id) -> excluded_by_rat
# length(intersect(fp_did_both, excluded_by_rat)) # N = 4
# 
# # FPs who did both judgement tasks and excluded by being randomizers and rating criterion. 
# intersect(fp_ta_randomizers$ran_id,fp_tb_id$below_mid_id) -> excluded_by_both
# length(intersect(fp_did_both, excluded_by_both)) # N = 8

#' The number of participants in each cluster as well as how they look like in a figure. 
#+ echo=TRUE, message = FALSE, warning = FALSE
# 
# fp_ta_wcentroid %>%
#   left_join(fp_tb_wcentroid, by="id1") -> fp_tab_wcentroid
# 
# fp_tab_wcentroid %>% 
#   mutate(clustering = case_when(
#     id1 %in% c(fp_tb_id$below_mid_id) ~ "4Excluded ID in Rating",
#     id1 %in% c(fp_ta_id$ran_id) ~ "1Randomizers ID in Ordering",
#     id1 %in% c(fp_ta_id$rev_id) ~ "3Reversers ID in Ordering",
#     id1 %in% c(fp_ta_id$con_id) ~ "2Constant ID in Ordering")) -> fp_tab_graph
# 
# o_r <- intersect(fp_ta$id, fp_tb$id)  finished both ordering and rating tasks in the survey. 
# no_r <- setdiff(fp_tb$id, fp_ta$id)  finished Rating not Ordering
# o_nr <- setdiff(fp_ta$id, fp_tb$id) finished Ordering not Rating
# 
# match(fp_tb_id$below_mid_id, fp_ta_id$ran_id) excluded by both
# match(fp_tb_id$below_mid_id, randomizers) excluded by both
# 
# randomizers <- fp_ta_id$ran_id %>% unlist() 
# 36-sum(is.na(match(randomizers, o_r))) finished both tasks, excluded by ordering
# 36-sum(is.na(match(randomizers, o_nr))) only finished ordering, excluded by ordering
# match(fp_tb_id$below_mid_id, o_r)  finished both tasks, excluded by rating
# match(hcp_tb_id$below_mid_id, no_r) only finished rating, excluded by rating
# 
# fp_tab_graph %>%
#   ggplot(aes(x = fp_ta_pairwise_cor, y = fp_tb_pairwise_cor, col= clustering)) +
#   geom_point(shape = 16, alpha=0.7)  +
#   xlim(c(-1,1)) +
#   ylim(c(-1,1)) +
#   labs(
#     x = "ID corr. w/ centroid in Ordering task",
#     y = "ID corr. w/ centroid in Rating task",
#     caption = "") +
#   ggtitle("Scatter plot between task A&B in FP sample")+
#   theme_minimal() +
#   theme(legend.position="bottom") +
#   theme(panel.grid.minor = element_blank()) -> p
# 
# ggMarginal(p, type="histogram") 
# 


