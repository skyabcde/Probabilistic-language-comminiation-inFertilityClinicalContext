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
library(qualtRics)
library(magrittr)

#+ echo=TRUE, message=FALSE, warning = FALSE

source(here::here('R_scripts/utility_functions.R'))

HCP_qualtrics_id = "SV_5A1WoGMMbAM5l2u"

HCP_data <- fetch_survey(surveyID = HCP_qualtrics_id, verbose = TRUE
                         , label = TRUE
                         , convert = FALSE
                         #, force_request = TRUE
)


HCP_data %<>%  
  # filter participants who did not consent (Q3_4) to the study
  filter(!is.na(Q3_4)) %>%
  filter(!is.na(nItems)) %>%
  split(.$ResponseId) %>%
  purrr::map_dfr(parseItemStrings, task_a = 59, task_b = 58, .id = "ResponseId") 

# write.csv(HCP_data, "data/HCP_data_n216.csv", row.names=FALSE)

#' Load data
fertility_doctors = here::here("data/HCP_data_n216.csv.gitignore")
hcp_all = read_csv(fertility_doctors)

#' Excluded participants who did not complete the survey within data collection duration. 
hcp_all %>% 
  filter(!ResponseId %in% c("R_1etbfKgOTtwk3Lj", "R_shDYmf48AOwqwoN", "R_2zGACadx6UTUmXD",
                            "R_tZ3ntExl3VLb0Ep", "R_2taJ3bmpdUUd9id")) %>%
  mutate(id = row_number()) -> hcp_all

#' Excluded participants who did not provide data. 

#' Create column label vector of Ordering task. 
hcp_all_ta_col <- colnames(hcp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() 

#' Remove participants who did not finish all orderings. 
hcp_ta <- hcp_all %>%   
  select(all_of(hcp_all_ta_col), itemString)  %>%
  mutate(id = row_number()) %>%
  rowwise(id) %>% 
  mutate(total = sum(is.na(c_across(1:17)))) %>%
  ungroup() %>%
  filter(total == 1) %>%
  select(-total) 


#' Create column label vector of Rating task. 
hcp_tb_all <- hcp_all %>% 
  select(matches("task_b_")) %>%
  select(-matches("task_b_display_")) %>%
  mutate(
    across(everything(), as.numeric),
    id = row_number()
  )  %>%
  round(digits=0) 

#' Remove participants who did not finish all ratings.  
hcp_tb <- hcp_tb_all %>% 
  rowwise(id) %>% 
  mutate(total = sum(is.na(c_across(1:17)))) %>%
  ungroup() %>%
  filter(total == 1) %>%
  select(-total)

#' HCPs who finished both Ordering and Rating tasks. N = 121
intersect(hcp_ta$id, hcp_tb$id) -> hcp_did_both

#' HCPs who finished only Ordering tasks. N = 38
setdiff(hcp_ta$id, hcp_tb$id) -> hcp_onlydid_ordering

#' HCPs who finished only Rating tasks. N = 1
setdiff(hcp_tb$id, hcp_ta$id) -> hcp_onlydid_rating
