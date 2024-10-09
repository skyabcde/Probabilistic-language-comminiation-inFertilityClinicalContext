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
library(qualtRics)
library(magrittr)

#+ echo=TRUE, message=FALSE, warning = FALSE

source(here::here('R_scripts/utility_functions.R'))

FP_qualtrics_id = "SV_b4q6y4EvCoFg2sl"

FP_data <- fetch_survey(surveyID = FP_qualtrics_id, verbose = TRUE
                        , label = TRUE
                        , convert = FALSE
                        #, force_request = TRUE
)


FP_data %<>%
  # filter participants who did not consent (Q3_6) to the study
  filter(!is.na(Q3_6)) %>%
  filter(!is.na(nItems)) %>%
  split(.$ResponseId) %>%
  purrr::map_dfr(parseItemStrings, task_a = 22, task_b = 31, .id = "ResponseId") 

# write.csv(FP_data, "data/FP_data_n257.csv", row.names=FALSE)

#' Load data
fertility_patient = here::here("data/FP_data_n257.csv.gitignore")
fp_all = read_csv(fertility_patient)

#' Excluded participants who did not complete the survey within data collection duration. 
fp_all %>% 
  filter(!ResponseId %in% c("R_3JLAjfum2sSanAf","R_2Y69sd7PB2JXcr1")) %>%
  mutate(id = row_number()) -> fp_all

#' Excluded participants who did not provide data. 

#' Create column label vector of Ordering task. 
fp_all_ta_col <- colnames(fp_all) %>%
  grepl(pattern = '^task_a_') %>%
  which() 

#' Remove participants who did not finish all orderings. 
fp_ta <- fp_all %>%   
  select(all_of(fp_all_ta_col), itemString)  %>%
  mutate(id = row_number()) %>%
  rowwise(id) %>% 
  mutate(total = sum(is.na(c_across(1:17)))) %>%
  ungroup() %>%
  filter(total == 1) %>%
  select(-total)

#' Create column label vector of Rating task. 
fp_tb_all <- fp_all %>% 
  select(matches("task_b_")) %>%
  select(-matches("task_b_display_")) %>%
  mutate(
    across(everything(), as.numeric),
    id = row_number()
  )  %>%
  round(digits=0) 

#' Remove participants who did not finish all ratings. 
fp_tb <- 
  fp_tb_all %>% 
  rowwise(id) %>% 
  mutate(total = sum(is.na(c_across(1:17)))) %>%
  ungroup() %>%
  filter(total == 1) %>%
  select(-total)

#' FPs who finished both Ordering and Rating tasks.
intersect(fp_ta$id, fp_tb$id) -> fp_did_both

#' FPs who finished only Ordering tasks.
setdiff(fp_ta$id, fp_tb$id) -> fp_onlydid_ordering

#' FPs who finished only Rating tasks.
setdiff(fp_tb$id, fp_ta$id) -> fp_onlydid_rating
