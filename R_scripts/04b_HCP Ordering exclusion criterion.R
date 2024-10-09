# MLE
hcp_ta_fun <- function(par, y) {
  n=15
  sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
  probs= exp(c(par[3:4],0))
  density = (probs[1]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[1]), shape2 = exp(par[2])) +
    (probs[2]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[2]), shape2 = exp(par[1])) +
    (probs[3]/sum(probs))* dnorm(y,0, sd)
  -sum(log(density))
} 

hcp_ta_wcentroid %>% 
  pull(hcp_ta_pairwise_cor)  -> hcp_ta_wcentroid_cor

# Parameter estimation for MLE
hcp_ta_par <- optim(par=c(-1,5,-1,-2), fn=hcp_ta_fun, y=hcp_ta_wcentroid_cor)
beta_par_all <- exp(hcp_ta_par$par[1:2]) 
n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))

hcp_ta_wcentroid %>% 
  mutate(curve_compiler = 0.5*dbeta((hcp_ta_wcentroid$hcp_ta_pairwise_cor+1)/2, shape1 = beta_par_all[2], shape2 = beta_par_all[1]),
         curve_reverser = 0.5*dbeta((hcp_ta_wcentroid$hcp_ta_pairwise_cor+1)/2, shape1 = beta_par_all[1], shape2 = beta_par_all[2]),
         curve_middle = (dnorm(hcp_ta_wcentroid$hcp_ta_pairwise_cor, 0, sd))) %>%
  select(-hcp_ta_mean_cor, -hcp_ta_pairwise_cor) %>%
  arrange(desc(curve_compiler))-> hcp_ta_density

# Likelihood ratio
hcp_ta_density  %>%
  rowwise() %>%
  mutate(ratio_compiler = curve_compiler/curve_middle,
         ratio_randomizer1 = curve_middle/curve_compiler, 
         ratio_randomizer2 = curve_middle/curve_reverser, 
         ratio_reverser = curve_reverser/curve_middle) -> hcp_ta_ratio

hcp_ta_ratio %>%
  summarise(ran_id = id1[curve_middle>curve_compiler & curve_middle>curve_reverser|
                       curve_middle<curve_compiler & ratio_compiler <10 |
                       curve_middle<curve_reverser & ratio_reverser <10]) -> hcp_ta_randomizers

hcp_ta_ratio %>%
  summarise(rev_id = id1[ratio_reverser>10]) -> hcp_ta_reversers

hcp_ta_ratio %>%
  summarise(com_id = id1[ratio_compiler>10]) -> hcp_ta_compilers

hcp_ta_compilers <- add_row(hcp_ta_compilers, 
                           com_id = hcp_ta_wcentroid_pluscentroid$id1[which.max(hcp_ta_wcentroid_pluscentroid$hcp_ta_mean_cor)])

