# MLE
fp_ta_fun <- function(par, y) {
  n=15
  sd=sqrt(2*(2*n+5)/(9*n*(n-1)))
  probs= exp(c(par[3:4],0))
  density = (probs[1]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[1]), shape2 = exp(par[2])) +
    (probs[2]/sum(probs))*0.5*dbeta((y+1)/2, shape1 = exp(par[2]), shape2 = exp(par[1])) +
    (probs[3]/sum(probs))* dnorm(y,0, sd)
  -sum(log(density))
} 

fp_ta_wcentroid %>% 
  pull(fp_ta_pairwise_cor)  -> fp_ta_wcentroid_cor

# Parameter estimation for MLE
fp_ta_par <- optim(par=c(-1,5,-1,-2), fn=fp_ta_fun, y=fp_ta_wcentroid_cor)
beta_par_all <- exp(fp_ta_par$par[1:2]) 
n=15
sd=sqrt(2*(2*n+5)/(9*n*(n-1)))

fp_ta_wcentroid %>% 
  mutate(curve_compiler = 0.5*dbeta((fp_ta_wcentroid$fp_ta_pairwise_cor+1)/2, shape1 = beta_par_all[2], shape2 = beta_par_all[1]),
         curve_reverser = 0.5*dbeta((fp_ta_wcentroid$fp_ta_pairwise_cor+1)/2, shape1 = beta_par_all[1], shape2 = beta_par_all[2]),
         curve_middle = (dnorm(fp_ta_wcentroid$fp_ta_pairwise_cor, 0, sd))) %>%
  select(-fp_ta_mean_cor, -fp_ta_pairwise_cor) %>%
  arrange(desc(curve_compiler))-> fp_ta_density

# Likelihood ratio
fp_ta_density  %>%
  rowwise() %>%
  mutate(ratio_compiler = curve_compiler/curve_middle,
         ratio_randomizer1 = curve_middle/curve_compiler, 
         ratio_randomizer2 = curve_middle/curve_reverser, 
         ratio_reverser = curve_reverser/curve_middle) -> fp_ta_ratio

fp_ta_ratio %>%
  summarise(ran_id = id1[curve_middle>curve_compiler & curve_middle>curve_reverser|
                              curve_middle<curve_compiler & ratio_compiler <10 |
                              curve_middle<curve_reverser & ratio_reverser <10]) -> fp_ta_randomizers

fp_ta_ratio %>%
  summarise(rev_id = id1[ratio_reverser>10]) -> fp_ta_reversers

fp_ta_ratio %>%
  summarise(com_id = id1[ratio_compiler>10]) -> fp_ta_compilers

fp_ta_compilers <- add_row(fp_ta_compilers, 
                           com_id = fp_ta_wcentroid_pluscentroid$id1[which.max(fp_ta_wcentroid_pluscentroid$fp_ta_mean_cor)])
