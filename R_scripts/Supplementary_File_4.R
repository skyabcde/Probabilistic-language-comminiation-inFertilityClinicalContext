#' ---
#' title: "Supplementary File 4"
#' subtitle: "Inferential analysis"
#' author: "Jiawen Sky Liu"
#' date: "`r format(Sys.time(), '%H:%M:%S %Z on %d %b %Y')`"
#' output: 
#'   # html_document: default
#'   pdf_document: default
#' ---
#'

#' <style type="text/css">
#'  .twoC {width: 100%}
#'  .clearer {clear: both}
#'  .twoC .tcright {max-width: 50%; float: right}
#'  .twoC .tcleft {max-width: 50%; float: left}
#' </style>

#' # Ordering task
#' 
#' To determine congruency between HCPs’ and FPs’ ordering responses, we compared 
#' the distributions of ranks for each term using log-linear analysis and subsequent 
#' permutation testing. 
#' 
#' ### Step 1: 
#' Data preparation and transformation: Participants’ ordering responses 
#' were transformed into a count structure that measures the frequency of each rank 
#' for a term. These counts were then grouped and tallied to determine the number 
#' of orderings per term for HCP and FP samples. 
#' 
#' ### Step 2: 
#' Log-linear analysis: A log-linear analysis was conducted on the 
#' contingency table for HCP and FP. The observed likelihood ratio test (LRT) 
#' statistics were calculated for each term, and the overall LRT statistic was 
#' obtained by summing these values. 
#' 
#' ### Step 3: 
#' Permutation testing (Good, 1994): To assess the statistical 
#' significance of the observed log-linear analysis results, permutation testing 
#' was conducted to generate a random distribution for each task between HCP and FP. 
#' This involved randomly shuffling the labels between responses among HCP and FP 
#' within the ordering responses and recalculating the log-linear statistics for 
#' each permutation. The permutation test also accounted for biases in the test 
#' statistics due to the dependency among ordering responses from the same respondents.
#' 
#' ### Step 4: 
#' Visualization and results: Figure 1 shows the permutation distribution 
#' of test statistics for each term, with histograms and vertical lines indicating 
#' the observed LRT values. Finally, the sum of observed values was compared to the
#' distribution of the summed statistics from the permutations, providing an overall 
#' p-value for the comparison between HCP and FP in the ordering task. Figure 2
#' shows the permutation results of the summed statistics, with solid vertical line
#' indicating summed observed LRT value and dotted vertical line indicating the
#' 95th percentile of the distribution. 
#' 
#' 
#' # Rating task 
#' 
#' To determine congruency between HCPs’ and FPs’ rating responses, we compared 
#' the distributions of ratings for each term through a Kolmogorov-Smirnov (KS) 
#' test and subsequent permutation testing. 
#' 
#' ### Step 1: 
#' Kolmogorov-Smirnov (KS) Test: A non-parametric KS test was conducted to compare 
#' the distributions of the ratings between HCP and FP sub-groups for each term. 
#' The observed KS test statistics for each term were combined using Fisher’s method 
#' (Rosenthal, 1978) to form a general statistic that reflects the overall 
#' difference between sub-groups. Fisher’s method involved first converting each 
#' p-value into a Chi-squared statistic using the formula $X_{i}=-2ln(p_{i})$ where 
#' the $ln$ denotes the natural logarithms. These Chi-squared values were then summed 
#' using by 
#' $X = -2 \displaystyle\sum_{i=1}^{16} ln(p_{i})$
#' to produce the combined statistic. 
#' 
#' ### Step 2: 
#' Permutation testing (Good, 1994): To assess the statistical significance of 
#' the observed KS statistic results, permutation testing was conducted to generate 
#' a random distribution for each task between HCP and FP. The same procedure was 
#' applied as in the ordering task, with labels between HCP and FP responses randomly 
#' shuffled and the KS statistics recalculated for each permutation.
#' 
#' ### Step 3: 
#' Visualization and results: Figure 3 shows the permutation distribution of 
#' test statistics for each term, with histograms and vertical lines indicating 
#' the observed p-values from the KS tests. Finally, the observed KS test statistics, 
#' calculated from Step 1, were compared to the distribution of summed statistics from
#' the permutations, providing an overall p-value for the comparison between HCP
#' and FP in the rating task. Figure 4 shows the permutation results of the summed
#' statistics, with solid vertical line indicating summed observed KS value and dotted
#' vertical line indicating the 95th percentile of the distribution.
#'
#'
#'
#' ![Figure 1](/Users/sky/Library/CloudStorage/OneDrive-CardiffUniversity/23-24 Fertility_Probability Term_Paper/Fertility_prob_paper_code/Figures/supplementary5_hist1.pdf)
#' ![Figure 2](/Users/sky/Library/CloudStorage/OneDrive-CardiffUniversity/23-24 Fertility_Probability Term_Paper/Fertility_prob_paper_code/Figures/supplementary5_hist2.pdf)
#' ![Figure 3](/Users/sky/Library/CloudStorage/OneDrive-CardiffUniversity/23-24 Fertility_Probability Term_Paper/Fertility_prob_paper_code/Figures/supplementary5_hist3.pdf)
#' ![Figure 4](/Users/sky/Library/CloudStorage/OneDrive-CardiffUniversity/23-24 Fertility_Probability Term_Paper/Fertility_prob_paper_code/Figures/supplementary5_hist4.pdf)
#' 
#' 
#' 
#' 
#' Code details see "R_scripts" -- 
#' "05_Inferential analysis Ordering.R" and 
#' "06_Inferential analysis Rating.R"
#' 
#' 
#' # Reference 
#' Rosenthal, R. (1978). Combining results of independent studies. 
#' Psychological Bulletin, 85(1), 185–193. https://doi.org/10.1037/0033-2909.85.1.185  
#' Good, P. (1994). Permutation Tests. Springer New York. 
#' https://doi.org/10.1007/978-1-4757-2346-5
