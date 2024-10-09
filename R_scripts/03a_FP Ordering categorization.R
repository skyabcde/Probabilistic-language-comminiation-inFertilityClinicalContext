# Create column label vector.
fp_ta_col <- colnames(fp_ta) %>%
  grepl(pattern = '^task_a_') %>%
  which()

# Calculate Kendall tau correlation between pairwise orderings within participants.
expand.grid(id1 = fp_ta$id, id2 = fp_ta$id) %>%
  mutate(
    fp_ta_pairwise_cor = purrr::map2_dbl(id1, id2, function(id1, id2){
      id1_responses = unlist(fp_ta[fp_ta$id == id1, fp_ta_col])
      id2_responses = unlist(fp_ta[fp_ta$id == id2, fp_ta_col])
      cor(id1_responses, id2_responses, method = 'k', use = 'pairwise')
    })
  ) %>%
  filter(
    !is.na(fp_ta_pairwise_cor)
  ) -> fp_ta_pairwise

#Calculate the ranked average pairwise ordering for graph purpose.
fp_ta_pairwise %>%
  filter(id1 != id2) %>%
  group_by(id1) %>%
  summarise(fp_ta_mean_cor = mean(fp_ta_pairwise_cor)) %>%
  arrange(fp_ta_mean_cor) %>%
  mutate(fp_ta_rank_cor = row_number()) -> fp_ta_ranked_ave

fp_ta_pairwise %>%
  as_tibble() %>%
  mutate(
    id1 = factor(id1, levels = fp_ta_ranked_ave$id1, ordered = TRUE),
    id2 = factor(id2, levels = fp_ta_ranked_ave$id1, ordered = TRUE)
  ) -> fp_ta_pairwise_graph

#Pairwise ordinal correlations between patients
fp_ta_pairwise_graph %>%
  mutate(
    id1 = as.integer(id1),
    id2 = as.integer(id2)
  ) %>%
  ggplot(aes(x = id1, y = id2, z = fp_ta_pairwise_cor)) +
  geom_contour_filled() +
  scale_x_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_y_continuous(name = "Proportion of participants less 'typical'", expand = c(0,0)) +
  scale_fill_viridis_d(name = "Kendall correlation") +
  ggtitle(ggtitle(label = "Figure 1")) -> fp_kendall_ordering

ggsave(fp_kendall_ordering,
      filename = "Figures/fp_kendall_ordering.pdf",
      height = 6, width = 8)

# Correlation with "most typical" respondent.
fp_ta_pairwise %>%
  filter(id1 != id2) %>%
  group_by(id1) %>%
  mutate(fp_ta_mean_cor = mean(fp_ta_pairwise_cor)) %>%
  ungroup() %>%
  mutate(fp_ta_centro_id = id1[which.max(fp_ta_mean_cor)]) %>%
  filter(id2 == fp_ta_centro_id) %>%
  select(-id2,-fp_ta_centro_id) -> fp_ta_wcentroid

# Keep centroid
fp_ta_pairwise %>%
#  filter(id1 != id2) %>%
  group_by(id1) %>%
  mutate(fp_ta_mean_cor = mean(fp_ta_pairwise_cor)) %>%
  ungroup() %>%
  mutate(fp_ta_centro_id = id1[which.max(fp_ta_mean_cor)]) %>%
  filter(id2 == fp_ta_centro_id) %>%
  select(-id2,-fp_ta_centro_id) -> fp_ta_wcentroid_pluscentroid

# Correlation with initial random ordering.
data.frame(id1 = fp_ta$id) %>%
  mutate(
    fp_ta_item_cor = purrr::map_dbl(id1, function (id1){
      a = unlist(fp_ta[fp_ta$id == id1, fp_ta_col])
      if(all(is.na(a))) return(NA)
      id1_responses = a %>%
        sort() %>%
        names() %>%
        stringr::str_remove_all(pattern = "[0-9]") %>%
        stringr::str_remove(pattern = 'task_a_') %>%
        stringr::str_replace_all(
          pattern = '\\.', replacement = ' '
        )
      item_string_order = strsplit(fp_ta[fp_ta$id == id1,]$itemString,split = ';') [[1]]
      cor(1:16, match(table = id1_responses, item_string_order), method = 'k')
    })) -> fp_ta_itemstring

fp_ta_wcentroid_pluscentroid %>%
  left_join(fp_ta_itemstring, by="id1") -> fp_ta_centro_item

# Scatter plot b/w best and item ranking cor.
fp_ta_centro_item %>%
  ggplot(aes(fp_ta_pairwise_cor, fp_ta_item_cor)) +
  geom_point() +
  theme_minimal()+
  labs(title = 'Figure 2',
       x = 'Corr. w/ most typical respondent', y = 'Corr. w/ initial random ordering') -> fp_corr_centroid_initial

ggsave(fp_corr_centroid_initial,
       filename = "Figures/fp_corr_w_centroid_initial.pdf",
       height = 6, width = 7.5)
