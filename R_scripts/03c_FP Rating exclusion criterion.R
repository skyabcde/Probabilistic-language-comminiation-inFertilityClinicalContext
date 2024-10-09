# Calculate Kendall tau correlation between pairwise ratings within participants. 
colnames(fp_tb) %>%
  grepl(pattern = '^task_b_') %>%
  which() -> fp_tb_col

expand.grid(id1 = fp_tb$id, id2 = fp_tb$id) %>% 
  mutate(
    fp_tb_pairwise_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_tb[fp_tb$id == id1, fp_tb_col])
      id2_responses = unlist(fp_tb[fp_tb$id == id2, fp_tb_col])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(fp_tb_pairwise_cor) 
  ) -> fp_tb_pairwise

# Pairwise correlation with centroid. 
fp_tb_pairwise %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  mutate(fp_tb_mean_cor = mean(fp_tb_pairwise_cor)) %>%
  ungroup() %>%
  mutate(fp_tb_centro_id = id1[which.max(fp_tb_mean_cor)]) %>%
  filter(id2 == fp_tb_centro_id) %>%
  select(-id2, -fp_tb_centro_id)-> fp_tb_wcentroid

# Data exclusion. 
fp_tb_wcentroid %>%
  summarise(below_mid_id = id1[fp_tb_pairwise_cor < 0.5]) -> fp_tb_id
