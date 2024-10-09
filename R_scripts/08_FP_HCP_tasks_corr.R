library(rlist)
library(tidyverse)
library(tidyr)
library(haven) 
library(gdata) 
library(data.table) 
library(here)
library(renv)
library(data.table) 
library(dplyr)
library(ggplot2)
library(dataRetrieval)
library(cowplot)

# Load data

source(here::here("R_scripts", "01_FP clean data.R")) 
source(here::here("R_scripts", "02_HCP clean data.R")) 

fp_ta_clean %>%
  left_join(fp_tb_clean, by = "id") -> fp_taskab

fp_taskab %>%
  filter(rowSums(is.na(select(., -id))) < 3) -> fp_taskab

colnames(fp_taskab) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> fp_col_a

colnames(fp_taskab) %>%
  grepl(pattern = '^task_b_') %>%
  which() -> fp_col_b

# Individual correlation between ranking and rating responses. 
data.frame(id1 = fp_taskab$id) %>% 
  mutate(
    taskab_cor = purrr::map(id1, function(id1){

      ranking = unlist(fp_taskab[fp_taskab$id == id1, fp_col_a]) 
      rankings = ranking[!is.na(ranking)] 
      
      names(rankings) = names(rankings) %>% 
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all("\\.", " ")
      
      rating = unlist(fp_taskab[fp_taskab$id == id1, fp_col_b]) 
      ratings = rating[!is.na(rating)] %>%
        sort() 
      
      names(ratings) = names(ratings) %>% 
        stringr::str_remove(pattern = 'task_b_') %>%
        stringr::str_replace_all("\\.", " ")
      
      rankings = rankings[names(ratings)]
      
        # Perform the Kendall correlation test
        cor_test_result = 
          cor.test(rankings, ratings, method = 'kendall')
        
        # Return the correlation coefficient and p-value
        list(correlation = -cor_test_result$estimate, 
                 p_value = cor_test_result$p.value) 
        
    })
  )  %>%
  unnest_wider(taskab_cor) -> cor_taskab_individual_fp 

mean(cor_taskab_individual_fp$correlation) # 0.7748288
sd(cor_taskab_individual_fp$correlation) # 0.09785056

fisher_p_value <- (-2) * sum(log(cor_taskab_individual_fp$p_value))

df <- 2 * length(cor_taskab_individual_fp$p_value)

combined_p_value <- pchisq(fisher_p_value, df, lower.tail = FALSE)

print(combined_p_value) #0

hcp_ta_clean %>%
  left_join(hcp_tb_clean, by = "id") -> hcp_taskab

hcp_taskab %>%
  filter(rowSums(is.na(select(., -id))) < 3) -> hcp_taskab

colnames(hcp_taskab) %>%
  grepl(pattern = '^task_a_') %>%
  which() -> hcp_col_a

colnames(hcp_taskab) %>%
  grepl(pattern = '^task_b_') %>%
  which() -> hcp_col_b

data.frame(id1 = hcp_taskab$id) %>% 
  mutate(
    taskab_cor = purrr::map(id1, function(id1){
      
      ranking = unlist(hcp_taskab[hcp_taskab$id == id1, hcp_col_a]) 
      rankings = ranking[!is.na(ranking)] 
      
      names(rankings) = names(rankings) %>% 
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all("\\.", " ")
      
      rating = unlist(hcp_taskab[hcp_taskab$id == id1, hcp_col_b]) 
      ratings = rating[!is.na(rating)] %>%
        sort() 
      
      names(ratings) = names(ratings) %>% 
        stringr::str_remove(pattern = 'task_b_') %>%
        stringr::str_replace_all("\\.", " ")
      
      rankings = rankings[names(ratings)]
      
      # Perform the Kendall correlation test
      cor_test_result = 
        cor.test(rankings, ratings, method = 'kendall')
      
      # Return the correlation coefficient and p-value
      list(correlation = -cor_test_result$estimate, 
           p_value = cor_test_result$p.value) 
      
    })
  )  %>%
  unnest_wider(taskab_cor) -> cor_taskab_individual_hcp 

mean(cor_taskab_individual_hcp$correlation) # 0.767402
sd(cor_taskab_individual_hcp$correlation) # 0.1046039

fisher_p_value_hcp <- (-2) * sum(log(cor_taskab_individual_hcp$p_value))

df <- 2 * length(cor_taskab_individual_hcp$p_value)

combined_p_value_hcp <- pchisq(fisher_p_value_hcp, df, lower.tail = FALSE)

print(combined_p_value_hcp) #1.914929e-305
