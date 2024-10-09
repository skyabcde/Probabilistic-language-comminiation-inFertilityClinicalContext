# Calculate Kendall tau correlation between pairwise ratings within participants. 
colnames(hcp_tb) %>%
  grepl(pattern = '^task_b_') %>%
  which() -> hcp_ta_col

expand.grid(id1 = hcp_tb$id, id2 = hcp_tb$id) %>% 
  mutate(
    hcp_tb_pairwise_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp_tb[hcp_tb$id == id1, hcp_ta_col])
      id2_responses = unlist(hcp_tb[hcp_tb$id == id2, hcp_ta_col])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(hcp_tb_pairwise_cor) 
  ) -> hcp_tb_pairwise

# Pairwise correlation with centroid. 
hcp_tb_pairwise %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  mutate(hcp_tb_mean_cor = mean(hcp_tb_pairwise_cor)) %>%
  ungroup() %>%
  mutate(hcp_tb_centro_id = id1[which.max(hcp_tb_mean_cor)]) %>%
  filter(id2 == hcp_tb_centro_id) %>%
  select(-id2, -hcp_tb_centro_id)-> hcp_tb_wcentroid

# Data exclusion. 
hcp_tb_wcentroid %>%
  summarise(below_mid_id = id1[hcp_tb_pairwise_cor < 0.5]) -> hcp_tb_id
