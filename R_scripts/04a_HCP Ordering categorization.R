# Create column label vector.
hcp_ta_col <- colnames(hcp_ta) %>%
  grepl(pattern = '^task_a_') %>%
  which()

# Calculate Kendall tau correlation between pairwise orderings within participants. 
expand.grid(id1 = hcp_ta$id, id2 = hcp_ta$id) %>% 
  mutate(
    hcp_ta_pairwise_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(hcp_ta[hcp_ta$id == id1, hcp_ta_col])
      id2_responses = unlist(hcp_ta[hcp_ta$id == id2, hcp_ta_col])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(hcp_ta_pairwise_cor) 
  ) -> hcp_ta_pairwise

# Calculate the ranked average pairwise ordering for graph purpose. 
# hcp_ta_pairwise %>%
#   filter(id1 != id2) %>% 
#   group_by(id1) %>% 
#   summarise(hcp_ta_mean_cor = mean(hcp_ta_pairwise_cor)) %>%
#   arrange(hcp_ta_mean_cor) %>% 
#   mutate(hcp_ta_rank_cor = row_number()) -> hcp_ta_ranked_ave
# 
# # Heat graph.  
# hcp_ta_pairwise %>%
#   as_tibble() %>%
#   mutate(
#     id1 = factor(id1, levels = hcp_ta_ranked_ave$id1, ordered = TRUE),
#     id2 = factor(id2, levels = hcp_ta_ranked_ave$id1, ordered = TRUE)
#   ) -> hcp_ta_pairwise_graph
# 
# hcp_ta_pairwise_graph %>%
#   mutate(
#     id1 = as.integer(id1),
#     id2 = as.integer(id2)
#   ) %>%
#   ggplot(aes(x = id1, y = id2, z = hcp_ta_pairwise_cor)) + 
#   geom_contour_filled() +
#   scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
#   scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
#   scale_fill_viridis_d(name = "Kendall correlation") + 
#   ggtitle(ggtitle(label = "Pairwise ordinal correlations between doctors")) -> hcp_kendall_ordering

#ggsave(hcp_kendall_ordering, 
#       filename = "/cloud/project/Paper_Figures/hcp_kendall_ordering.pdf",
#       height = 6, width = 8)

# Correlation with "most typical"
hcp_ta_pairwise %>%
  filter(id1 != id2) %>% 
  group_by(id1) %>% 
  mutate(hcp_ta_mean_cor = mean(hcp_ta_pairwise_cor)) %>%
  ungroup() %>%
  mutate(hcp_ta_centro_id = id1[which.max(hcp_ta_mean_cor)]) %>%
  filter(id2 == hcp_ta_centro_id) %>%
  select(-id2,-hcp_ta_centro_id)-> hcp_ta_wcentroid

# Keep centroid 
hcp_ta_pairwise %>%
  # filter(id1 != id2) %>%
  group_by(id1) %>%
  mutate(hcp_ta_mean_cor = mean(hcp_ta_pairwise_cor)) %>%
  ungroup() %>%
  mutate(hcp_ta_centro_id = id1[which.max(hcp_ta_mean_cor)]) %>%
  filter(id2 == hcp_ta_centro_id) %>%
  select(-id2,-hcp_ta_centro_id)-> hcp_ta_wcentroid_pluscentroid

# #Correlation with initial random ordering.
# data.frame(id1 = hcp_ta$id) %>%
#   mutate(
#     hcp_ta_item_cor = purrr::map_dbl(id1, function (id1){
#       a = unlist(hcp_ta[hcp_ta$id == id1, hcp_ta_col])
#       if(all(is.na(a))) return(NA)
#       id1_responses = a %>%
#         sort() %>%
#         names() %>%
#         stringr::str_remove_all(pattern = "[0-9]") %>%
#         stringr::str_remove(pattern = 'task_a_') %>%
#         stringr::str_replace_all(
#           pattern = '\\.', replacement = ' '
#         )
#       item_string_order = strsplit(hcp_ta[hcp_ta$id == id1,]$itemString,split = ';') [[1]]
#       cor(1:16, match(table = id1_responses, item_string_order), method = 'k')
#     })) -> hcp_ta_itemstring
# 
# hcp_ta_wcentroid_pluscentroid %>%
#   left_join(hcp_ta_itemstring, by="id1") -> hcp_ta_centro_item
# 
# hcp_ta_centro_item %>%
#   mutate(mle_cluster = ifelse(id1 %in% hcp_ta_compilers$com_id, "com",
#                               ifelse(id1 %in% hcp_ta_reversers$rev_id, "rev",
#                                      ifelse(id1 %in% hcp_ta_randomizers$ran_id, "ran", " ")))) %>%
#   add_column(cluster_exemplars_3_hcp) -> hcp_ta_centro_item
# 
# #Scatter plot b/w best and item ranking cor.
# hcp_ta_centro_item %>%
#   ggplot(aes(hcp_ta_pairwise_cor, hcp_ta_item_cor)) +
#   geom_point() +
#   theme_minimal()+
#   labs(title = 'HCP',
#        x = 'Corr. w/ most typical respondent', y = 'Corr. w/ initial random ordering') -> hcp_corr_centroid_initial
# 
# ggsave(hcp_corr_centroid_initial,
#        filename = here::here("Paper_Figures", "hcp_corr_w_centroid_initial.pdf"),
#        height = 6, width = 7.5)
