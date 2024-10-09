#+ echo = FALSE, message =FALSE, warning = FALSE
library(tidyverse)
library(tidyr)
library(haven) 
library(dplyr)
library(data.table)  
library(here)
library(data.table) 
library(ggplot2)
library(ggExtra)

#' Excluded participants who did not provide data or did not finish the task within data collection duration.
#+ echo=TRUE, message=FALSE, warning = FALSE
source(here::here("R_scripts", "00_HCP data loaded.R")) 

#' Next, we spotted participants who did not follow the instruction in the Ordering task.
#+ echo=TRUE, message=FALSE, warning = FALSE
source(here::here("R_scripts", "04a_HCP Ordering categorization.R")) 

#' Given the categorization, we excluded participants given the MLE results.
#+ echo=TRUE, message = FALSE, warning = FALSE
source(here::here("R_scripts", "04b_HCP Ordering exclusion criterion.R"))

#' And we excluded participants based on the Rating exclusion criterion.
#+ echo=TRUE, message = FALSE, warning = FALSE
source(here::here("R_scripts", "04c_HCP Rating exclusion criterion.R"))

#' Ordering responses exclusion. 

#+ echo=TRUE, message = FALSE, warning = FALSE

# Reverse reversers' answers. 
subset(hcp_ta, (id %in% c(hcp_ta_reversers$rev_id))) -> hcp_ta_rev

hcp_ta_rev %>%
  select(-id, -itemString)-> hcp_ta_rev_cal
as.data.frame(hcp_ta_rev_cal * (-1) + 17) %>%
  add_column(id = hcp_ta_rev$id)-> hcp_ta_rev_correct

# Subset the Constant Value Givers in Ordering task. 
subset(
  hcp_ta, 
  !(id %in% c(hcp_ta_randomizers$ran_id,
              hcp_ta_reversers$rev_id))
) %>%
  select(-itemString) -> hcp_ta_com

hcp_ta_com %>%
  rbind(hcp_ta_rev_correct)  -> hcp_ta_com_rev

subset(
  hcp_ta_com_rev, 
  !(id %in% 
      c(# Excluded respondents by Rating exclusion criterion in Ordering responses. 
        hcp_tb_id$below_mid_id))) -> hcp_ta_clean 

#' Rating responses exclusion. 

#+ echo=TRUE, message = FALSE, warning = FALSE

subset(
  hcp_tb, 
  !(id %in% 
      c(hcp_ta_randomizers$ran_id, 
        hcp_tb_id$below_mid_id))) -> hcp_tb_clean

#' Save the clean data. 
# write.csv(hcp_ta_clean, "derived_data/hcp_ta_clean_new.csv", row.names=FALSE)
# write.csv(hcp_tb_clean, "derived_data/hcp_tb_clean_new.csv", row.names=FALSE)

# Descriptive 
# # HCPss who did only Ordering task and excluded by being randomizers. 
# length(intersect(hcp_onlydid_ordering, hcp_ta_randomizers$ran_id)) # N = 10
# 
# # HCPs who did only Rating task and excluded by rating exclusion criterion. 
# length(intersect(hcp_onlydid_rating, hcp_tb_id$below_mid_id)) # N = 0
# 
# # HCPs who did both judgement tasks and excluded by being randomizers. 
# setdiff(hcp_ta_randomizers$ran_id,hcp_tb_id$below_mid_id) -> hcp_excluded_by_ord
# length(intersect(hcp_did_both, hcp_excluded_by_ord)) # N = 10
# 
# # HCPs who did both judgement tasks and excluded by rating criterion. 
# setdiff(hcp_tb_id$below_mid_id,hcp_ta_randomizers$ran_id) -> hcp_excluded_by_rat
# length(intersect(hcp_did_both, hcp_excluded_by_rat)) # N = 3
# 
# # HCPs who did both judgement tasks and excluded by being randomizers and rating criterion. 
# intersect(hcp_ta_randomizers$ran_id,hcp_tb_id$below_mid_id) -> hcp_excluded_by_both
# length(intersect(hcp_did_both, hcp_excluded_by_both)) # N = 7  

#' The number of participants in each cluster as well as how they look like in a figure. 
#' #' We can also visually see how out-liners were represented in both ordering and rating tasks. 
#' #+ echo=FALSE, message = FALSE, warning = FALSE
#' 
#' hcp_ta_wcentroid %>%
#'   left_join(hcp_tb_wcentroid, by="id1") -> hcp_tab_wcentroid
#' 
#' hcp_tab_wcentroid %>% 
#'   mutate(clustering = case_when(
#'     id1 %in% c(hcp_tb_id$below_mid_id) ~ "4Excluded ID in Rating; 10/118",
#'     id1 %in% c(hcp_ta_id$ran_id) ~ "1Randomizers ID in Ordering; 27/153",
#'     id1 %in% c(hcp_ta_id$rev_id) ~ "3Reversers ID in Ordering; 4/153",
#'     id1 %in% c(hcp_ta_id$con_id) ~ "2Constant ID in Ordering; 121/153")) -> hcp_tab_graph
#' 
#' o_r <- intersect(hcp_ta$id, hcp_tb$id) # N = 117  finished both ordering and rating tasks in the survey. 
#' no_r <- setdiff(hcp_tb$id, hcp_ta$id) # ID = 106 finished Rating not Ordering
#' o_nr <- setdiff(hcp_ta$id, hcp_tb$id) # N = 36 finished Ordering not Rating
#' 
#' randomizers <- hcp_ta_id$ran_id %>% unlist() 
#' 27-sum(is.na(match(randomizers, o_r))) # N= 18 finished both tasks, excluded by ordering
#' 27-sum(is.na(match(randomizers, o_nr))) # N= 9 only finished ordering, excluded by ordering
#' match(hcp_tb_id$below_mid_id, o_r) # N = 10 finished both tasks, excluded by rating
#' match(hcp_tb_id$below_mid_id, no_r) # N= 0 only finished rating, excluded by rating
#' 
#' match(hcp_tb_id$below_mid_id, hcp_ta_id$ran_id) # N = 7 excluded by both
#' match(hcp_tb_id$below_mid_id, randomizers) # N= 7 excluded by both
#' 
#' match(hcp_tb_id$below_mid_id, hcp_ta_id$ran_id)
#' 
#' hcp_tab_graph %>%
#'   ggplot(aes(x = hcp_ta_pairwise_cor, y = hcp_tb_pairwise_cor, col= clustering)) +
#'   geom_point(shape = 16, alpha=0.7)  +
#'   xlim(c(-1,1)) +
#'   ylim(c(-1,1)) +
#'   labs(
#'     x = "ID corr. w/ centroid in Ordering task",
#'     y = "ID corr. w/ centroid in Rating task",
#'     caption = "") +
#'   ggtitle("Scatter plot between task A&B in HCP sample")+
#'   theme_minimal() +
#'   theme(panel.grid.minor = element_blank()) -> p
#' 
#' ggMarginal(p, type="histogram") 
#' 

