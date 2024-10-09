#' ---
#' title: "Supplementary File 3"
#' subtitle: "Screening and categorization analysis"
#' author: "Jiawen Sky Liu"
#' date: "`r format(Sys.time(), '%H:%M:%S %Z on %d %b %Y')`"
#' output:
#'  # html_document: default
#'   pdf_document: 
#'       latex_engine: xelatex
#' #bibliography: /cloud/project/Fertility.bib
#' #link-citations: TRUE
#' ---

#' <style type="text/css">
#'  .twoC {width: 100%}
#'  .clearer {clear: both}
#'  .twoC .tcright {max-width: 50%; float: right}
#'  .twoC .tcleft {max-width: 50%; float: left}
#' </style>

#' # Part 1
#' 
#' To quantify the strength of the ordinal relationship between responses in ordering 
#' and rating tasks, Kendall's tau (Kendall, 1938) correlation coefficient testing
#' was used. The Kendall's rank correlation coefficient $\tau$ measures the rank
#' distance between two sets of responses to assess their similarity. 
#' Specifically, $\tau$ is defined as: 
#' 
#' $\tau = \frac{2}{n(n-1)} \displaystyle\sum_{i<j} sgn(x_{i}-x_{j}) sgn(y_{i}-y_{j})$ (1.1)
#' 
#' where n represents the number of participants,
#' $x_{i}$ and $x_{j}$ are the ranks of the i-th and j-th participants in the first term, and 
#' $y_{i}$ and $y_{j}$ are the ranks in the second term.
#' 
#' A $\tau = 0$ indicates no correlation, while higher values indicate stronger 
#' correlation between responses.
#' 
#' Thus, the rank correlation coefficient is used to test whether participants followed 
#' the task instructions. 
#' 
#' We first calculated the pairwise Kendall's tau correlations between each 
#' respondent's ordering response and those of all other respondents within both the
#' HCP and FP samples.
#' The mean Kendall's tau was then derived from these individual pairwise 
#' correlations.
#' The respondent with the highest average Kendall's tau was called the "most typical" 
#' respondent, and was used for further categorization analysis. 
#' 
#' Respondents were subsequently grouped into three categories based on their correlation 
#' with the "most typical" respondent. As illustrated in Figure 1*, the
#' upper-right cluster, termed "compliers", is highly positively correlated with 
#' the most typical respondent and with each other.
#' Further checking of their ordering responses indicates them performing the task as instructed.
#' The lower-left cluster, termed "reversers", shows a strong negative correlation with
#' the most typical respondent and is also highly correlated within itself.
#' Their responses were found to be anti-correlated to the compliers,
#' suggesting a systenmatic misunderstanding of the instructions.
#' Respondents clustered in the middle, with low or no correlation with either groups,
#' were labeled "randomizers", as their responses were poorly correlated with others. 
#' 
#' Figure 2* also supports the categorization of the three clusters in FP samples.
#' It shows the correlation between each respondent's ordering response
#' and the most typical respondent's, on the x-axis, as well as the correlation
#' between each respondent's ordering response and their initial random ordering in the task, on the y-axis.
#' The figure illustrates that randomizers who have low correlation with other
#' respondents also highly correlate with the initial random ordering, whereas
#' both compliers and reversers have none or negative correlation with the initial random ordering.
#' 
#' *Both Figure 1&2 were obtained from FP sample, illustrated as an example. 
#' 
#' ![Figure 1](/Users/sky/Library/CloudStorage/OneDrive-CardiffUniversity/23-24 Fertility_Probability Term_Paper/Fertility_prob_paper_code/Figures/fp_kendall_ordering.pdf)
#' ![Figure 2](/Users/sky/Library/CloudStorage/OneDrive-CardiffUniversity/23-24 Fertility_Probability Term_Paper/Fertility_prob_paper_code/Figures/fp_corr_w_centroid_initial.pdf)
#' 
#' Code details refer to "R_scripts" -- 
#' "03a_FP Ordering categorization.R" and
#' "04a_HCP Ordering categorization.R" 
#' 
#' # Part 2 -- Ordering exclusion criterion 
#' 
#' To model the Kendall's correlation coefficients between the compliers' and
#' reversers' responses and the most typical response, we used two beta distributions.
#' The beta distribution is defined by the two-parameter probability density function:
#' 
#' $f(x | \alpha, \beta) = \frac{\Gamma(\alpha + \beta)}{\Gamma(\alpha))\Gamma(\beta)} x^{\alpha-1} (1-x)^{\beta-1}$, 
#' $0\leq x\leq 1, \alpha >0,  \beta>0$  (1.2)
#' 
#' The parameters $\alpha$ and $\beta$ are symmetrically related as: 
#' 
#' $f(x|\alpha, \beta) = f(1-x|\beta, \alpha)$   (1.3)
#' 
#' which $X$ represents beta distribution for compliers with parameters $\alpha$ and $\beta$,
#' and $1-X$ represents beta distribution for reversers with parameters $\beta$ and $\alpha$,
#' reflecting the anti-correlation between these two groups.
#' 
#' 
#' For the randomizers, whose responses are independent of the most typical response and
#' highly correlated with the initial random orderings, we approximated Kendall's
#' tau using a normal distribution with a mean of zero and variance of $2(2n+5)/9n(n-1)$.
#' 
#' The proportion parameters $P_{c}$ and $P_{r}$ represent the proportions of compliers
#' and reversers in the sample, respectively, while the proportion of randomizers
#' is represented by [1-($P_{c}$+$P_{r}$)].
#' The full model incorporating the 3 categories of participants is expressed as:
#' 
#' $f(x|\alpha, \beta)*P_{c} + f(1-x|\beta, \alpha)*P_{r} + Normal~(0, 2(2n+5)/9n(n-1))*[1-(P_{c}+P_{r})]$   (1.4)
#' 
#' These parameters were estimated by numerical optimization in RStudio (RStudio Team, 2023).
#' The most typical respondent, who had Kendall's tau correlation coefficient $\tau = 1$,
#' was eliminated from the optimization process.
#' 
#' Finally, after fitting the parameter values to the model, a respondent was
#' included in the ordering task analysis only if their likelihood ratio of being
#' in either the compliers or reversers category was $≥ 10$.
#' 
#' Code details refer to "R_scripts" -- 
#' "03b_FP Ordering exclusion criterion.R" and
#' "04b_HCP Ordering exclusion criterion.R" 
#' 
#' # Part 3 -- Rating task exclusion criterion 
#' The same Kendall's correlation coefficient statistic of the rating responses
#' was used to exclude respondents in rating task. 
#' Specifically, a respondent's rating response was included in further analysis only
#' if its Kendall's correlation coefficient $\tau$ between the respondent's rating response
#' and the most typical respondent's, obtained from rating task, greater than 0.5.
#' 
#' Code details refer to "R_scripts" -- 
#' "03c_FP Rating exclusion criterion.R" and 
#' "04c_HCP Rating exclusion criterion.R" 
#' 
#' ### Reference 
#' 
#' Kendall, M. G. (1938). A new measure of rank correlation. Biometrika, 
#' 30(1–2), 81–93. https://doi.org/10.1093/biomet/30.1-2.81  
#' RStudio Team (2023). RStudio: Integrated Development for R. RStudio, PBC, 
#' Boston, MA URL http://www.rstudio.com/   





