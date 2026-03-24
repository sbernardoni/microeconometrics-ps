*============================================================================
/* Group number: 1 */
/* Group composition: Sara Bernardoni, Gabriele Molè, Florens Schlosser */
*============================================================================

*=============================================================================
/* 								Setup 										*/
*=============================================================================

clear

set more off

/* For commands */

/* First time running this code? Please remove the comment marks from the code below and install of the necessary packages */

/*
ssc install outreg2, replace
ssc install ivreg2, replace
ssc install estout, replace
ssc install randomizr, replace
ssc install ritest, replace
ssc install lassopack, replace
ssc install pdslasso, replace
ssc install ranktest, replace
ssc install balancetable, replace
ssc install randtreat, replace
 */

/* For graphs & stuff */
/*
ssc install grstyle, replace
ssc install coefplot, replace
graph set window fontface "Lato"
grstyle init
grstyle set plain, horizontal
*/
local user = c(username)

if ("`user'" == "erick") {
    global filepath "/home/erick/TEMP/"
}

if ("`user'" == "Sara") {
    global filepath "C:\Users\Sara\Documents\ESS\20295 - Microeconometrics\microeconometrics-ps\ps1"
	global output "C:\Users\Sara\Documents\ESS\20295 - Microeconometrics\microeconometrics-ps\ps1\ps1_output"
}

if ("`user'" == "gabrielemole") {
    global filepath ""C:\Users\Stealth\Desktop\microeconometrics-ps\ps1""
	global output "C:\Users\Stealth\Desktop\microeconometrics-ps\ps1\ps1_output"
}

*=============================================================================
/* 								Question 1 									*/
/* Use the file jtrain2 													*/
*=============================================================================

use "https://raw.githubusercontent.com/sbernardoni/microeconometrics-ps/06b798693174efb8e85c8f805ac242c8fe9d2302/ps1/ps1_data/jtrain2.dta", clear

/* (a) Construct a table checking for balance across treatment and control for the following covariates: age educ black hisp nodegree re74 re75.
Name it TABLE 1.
Present for each variable: mean for treated, mean for controls, standard deviations for treated, standard deviations for control, difference in means between treatment and control, appropriate standard errors for difference in means.
Comment on how many variables are balanced or not. Is it what you expected? */

matrix table_1a = J(7,6,.)

local covars "age educ black hisp nodegree re74 re75"
local row_1a = 1

foreach var of local covars {
	ttest `var', by(train)
    
    local treated_mean = r(mu_2)
    local control_mean = r(mu_1)
    local treated_sd = r(sd_2)
    local control_sd = r(sd_1)
    local diff_mean = `treated_mean' - `control_mean'
    local se_diff = r(se)
    
    matrix table_1a[`row_1a',1] = `treated_mean'
    matrix table_1a[`row_1a',2] = `control_mean'
    matrix table_1a[`row_1a',3] = `treated_sd'
    matrix table_1a[`row_1a',4] = `control_sd'
    matrix table_1a[`row_1a',5] = `diff_mean'
    matrix table_1a[`row_1a',6] = `se_diff'
    
    local row_1a = `row_1a' + 1
}

matrix colnames table_1a = TreatedMean_j3 ControlMean_j3 TreatedSD_j3 ControlSD_j3 DiffMean_j3 SE_Diff_j3
matrix rownames table_1a = age educ black hisp nodegree re74 re75

matrix list table_1a

esttab matrix(table_1a) using "ps1/ps1_output/table_1.tex", replace tex ///
    title("Balance Check Across Treatment and Control") ///
    cells("result(fmt(3))") ///
	nomtitles
	
	/* A: The results indicate that, overall, most of the covariates are balanced between the treatment and control groups, but there are a couple of exceptions that merit further discussion.

    For five out of the seven variables—age, education (educ), the variable measuring past earnings (re74 and re75), and the indicator for being black—the differences in means are small and not statistically significant. This suggests that randomization (or the design of the study) has largely succeeded in equating the two groups on these characteristics. This outcome is what one would typically expect in a well-conducted randomized experiment, where random assignment should, in theory, yield balance across observed characteristics.

    However, the variables "nodegree" and, to a lesser extent, "hisp" show discrepancies. The "nodegree" variable has a statistically significant difference between the groups, meaning that the proportion of individuals without a degree is noticeably different in the treatment group compared to the control group. The "hisp" variable shows a borderline significant difference; while not as pronounced as "nodegree," it still hints at some imbalance that might be of concern.

    In summary, while the majority of the covariates (five out of seven) are balanced, the imbalance in "nodegree" and the marginal case of "hisp" indicate that there might be systematic differences that could affect the outcome if these variables are correlated with the treatment effect. This is a common issue in finite samples and may require further adjustment—such as including these covariates in the regression—to ensure that the estimates of the treatment effect are not biased. Overall, the results are mostly in line with expectations for a randomized study, with the caveat that the imbalances observed in "nodegree" (and possibly "hisp") should be addressed in subsequent analyses. 
	
	This is nonetheless understandable, as we are dealing with a subsample of the original experimental data. */

/* (b) Regress re78 on train.
Save the estimate and the standard error of the coefficient on train as scalars.
Interpret the coefficient. */

regress re78 train, vce(robust)

scalar coef1 = _b[train]
scalar se1 = _se[train]

	/* A: By running a regression of the real earnings in 1978 on training (which represents our treatment variable), we obtain a positive coefficient of 1.794343 significant at the 5% level; this implies that real earnings, in our experiment, were positive correlated with the training programme.
	
	We ought to keep in mind that, despite the coefficient being statistically significant and different from zero, the R^2 of our model (0.0178) suggests that there are other factors responsible for the vast portion of the variation of real earnings in 1978 */

/* (c) Construct a table by sequentially adding the output of the following regressions to each column:
(1) re78 on train;
(2) re78 on train age educ black hisp;
(3) re78 on train age educ black hisp re74 re75;
Add rows to the table with the number of controls and treated in each regression. Name it TABLE 2. */

regress re78 train age educ black hisp re74 re75, vce(robust)

count if e(sample) & train==0
scalar controls1 = r(N)
count if e(sample) & train==1
scalar treated1 = r(N)

regress re78 train, vce(robust)
outreg2 using table_2.tex, replace se bdec(3) sdec(3) ///
addstat("Treated", treated1, "Controls", controls1) ctitle("Regression 1c_1")

regress re78 train age educ black hisp, vce(robust)
outreg2 using table_2.tex, append se bdec(3) sdec(3) ///
addstat("Treated", treated1, "Controls", controls1) ctitle("Regression 1c_2")

regress re78 train age educ black hisp re74 re75, vce(robust)
outreg2 using table_2.tex, append se bdec(3) sdec(3) ///
addstat("Treated", treated1, "Controls", controls1) ctitle("Regression 1c_3")

	
/* Are your results sensitive to the introduction of covariates? */

	/* A: In the simplest model (Regression 1), where re78 is regressed solely on the treatment variable train, the estimated coefficient for train is approximately 1.79 with a standard error of 0.63; this entails that on average, the treatment effect was equal to approximately 1800USD per year. When we sequentially introduced additional covariates in Regression 2 (adding age, educ, black, and hisp) and then in Regression 3 (further adding re74 and re75), the estimated coefficient for train slightly decreases to around 1.69 and 1.68, respectively, while the standard errors remain nearly identical.

    These minimal changes in both the point estimates and standard errors suggest that the treatment effect is robust to the inclusion of additional covariates. In other words, the introduction of controls does not significantly alter the estimated effect of the training program on re78. This implies that omitted variable bias is likely not a major concern in this context, as the observable characteristics we controlled for do not substantially confound the relationship between the treatment and the outcome.

    Overall, our results indicate that the impact of the training program is not sensitive to the introduction of covariates, which reinforces the credibility of our baseline findings. */

/* (d) dfbeta is a statistic that measures how much the regression coefficient of a certain variable changes in standard deviations if the i-th observation is deleted.
If using Stata, type help dfbeta and discover how to estimate this statistic after a regression.
Generate a variable named influence_train storing the dfbetas of train of the last regression you did in point (c).
Redo the last regression you did in point (c) but removing the observations with the 3, 5, and 10 lowest and largest values in influence train.
Are your results sensitive to influential observations? */

regress re78 train age educ black hisp re74 re75
dfbeta, stub(influence_)

egen rank_influence = rank(influence_1), field

preserve
	summarize influence_1, meanonly
	local N = r(N)
	drop if rank_influence <= 3 | rank_influence >= (`N' - 3 + 1)
	regress re78 train age educ black hisp re74 re75, vce(robust)
	estimates store trim3
restore

preserve
    summarize influence_1, meanonly
	local N = r(N)
	drop if rank_influence <= 5 | rank_influence >= (`N' - 5 + 1)
	regress re78 train age educ black hisp re74 re75, vce(robust)
	estimates store trim5
restore

preserve
    summarize influence_1, meanonly
	local N = r(N)
	drop if rank_influence <= 10 | rank_influence >= (`N' - 10 + 1)
	regress re78 train age educ black hisp re74 re75, vce(robust)
	estimates store trim10
restore

* Questa just in case ci fossimo persi qualcosa, secondo me ha senso ma non è richiesta #SG *

esttab trim3 trim5 trim10 using "ps1/ps1_output/table_3.tex", replace tex ///
    title("Regression Results After Removing Extreme Influence Observations") ///
    stats(N, fmt(%9.0g) label("N")) ///
	nomtitles 
	
	
		/* A: When we look at the results of the regression after trimming the most influential observations, we notice some changes in the estimated effect of the treatment variable ("train") on re78. In the full sample, the estimated coefficient for train is about 1.68 and statistically significant (p = 0.008). After removing the 3 most extreme observations from each tail of the dfbeta distribution, the coefficient drops to about 1.36 (p = 0.009). As we trim more observations—first 5 from each tail (reducing the coefficient to about 1.22 with p = 0.015) and then 10 from each tail (bringing it down to around 1.02 with p = 0.029)—the estimated effect continues to decrease in magnitude, though it remains statistically significant in all cases.

		This pattern suggests that a few observations with large influence were pulling the original estimate upward. Although the treatment effect remains positive and statistically significant after trimming, the fact that its magnitude changes appreciably indicates that the results are somewhat sensitive to influential observations. In other words, while the overall conclusion (that the training program has a positive effect on re78) holds even when these outliers are removed, the precise size of the effect is affected by a small number of influential cases. */

*=============================================================================
/* 								Question 2 									*/
/* Use the jtrain3 															*/
*=============================================================================

use "https://raw.githubusercontent.com/sbernardoni/microeconometrics-ps/06b798693174efb8e85c8f805ac242c8fe9d2302/ps1/ps1_data/jtrain3.dta", clear

/* (a) Do a table with the same structure of TABLE 1 of item (a) in question 1 for the following covariates: age educ black hisp re74 re75 (note that nodegree is not present in the current dataset.) Add the corresponding columns to TABLE 1. */

matrix table_2a = J(6,6,.)

local covars "age educ black hisp re74 re75"
local row_2a = 1

foreach var of local covars {
	ttest `var', by(train)
    
    local treated_mean = r(mu_2)
    local control_mean = r(mu_1)
    local treated_sd = r(sd_2)
    local control_sd = r(sd_1)
    local diff_mean = `treated_mean' - `control_mean'
    local se_diff = r(se)
    
    matrix table_2a[`row_2a',1] = `treated_mean'
    matrix table_2a[`row_2a',2] = `control_mean'
    matrix table_2a[`row_2a',3] = `treated_sd'
    matrix table_2a[`row_2a',4] = `control_sd'
    matrix table_2a[`row_2a',5] = `diff_mean'
    matrix table_2a[`row_2a',6] = `se_diff'
    
    local row_2a = `row_2a' + 1
}

matrix list table_2a

matrix table_2a_new = J(7,6,.)
forvalues i = 1/4 {
    forvalues j = 1/6 {
        matrix table_2a_new[`i', `j'] = table_2a[`i', `j']
    }
}
forvalues i = 5/6 {
    forvalues j = 1/6 {
        matrix table_2a_new[`i'+1, `j'] = table_2a[`i', `j']
    }
}

matrix drop table_2a
matrix table_2a = table_2a_new

matrix list table_2a

matrix table_1a_2a = table_1a, table_2a

matrix colnames table_1a_2a = TreatedMean_treat_1a ControlMean_treat_1a TreatedSD_treat_1a ControlSD_treat_1a DiffMean_treat_1a SE_Diff_treat_1a TreatedMean_treat_2a ControlMean_treat_2a TreatedSD_treat_2a ControlSD_treat_2a DiffMean_treat_2a SE_Diff_treat_2a

matrix list table_1a_2a

esttab matrix(table_1a_2a) using "ps1/ps1_output/table_1.tex", replace tex ///
    title("Balance Check Across Treatment and Control") ///
    cells("result(fmt(3))") ///
	nomtitles

/* (b) Generate a variable named treated that randomly allocates half of observations to a (fake) treatment group and the other half to a (fake) control group. Fix a seed of 5 digits using the command set seed. */

set seed 20295
gen treated = runiform()

replace treated =0 if treated <= 0.5
replace treated =1 if treated > 0.5

tabulate treated

/* (c) If using Stata, type ssc install randtreat. Then, read randtreat help file. */
	
	/* (i) Redo point (b) using the command randtreat. (ii) Name treated 2 your new (fake) treatment variable.*/
	
randtreat, generate(treated_2) setseed(20295) misfits(strata) // check this out as one value results to this misfitted	
		
	/* (iii) Check whether the correlation between treated 2 and treated is statistically significant or not. (Hint: use pwcorr X Y, sig) */
	
pwcorr treated treated_2, sig

	/* A: The correlation is virtually null and statistically not significant. This is due to different algorithms inducing a random assignment: as the assignments are both random and rely on different “randomization algorithms”, they are uncorrelated even when using the same seed. */

/* (d) Do a table with the same structure of TABLE 1 of item (a) in question 1., but using treated instead of train. */ 

	/* (i) Use the same list of covariates of item (a) of this question. */
	
matrix table_2d = J(6,6,.)

local covars "age educ black hisp re74 re75"
local row_2d = 1

foreach var of local covars {
	ttest `var', by(treated)
    
    local treated_mean = r(mu_2)
    local control_mean = r(mu_1)
    local treated_sd = r(sd_2)
    local control_sd = r(sd_1)
    local diff_mean = `treated_mean' - `control_mean'
    local se_diff = r(se)
    
    matrix table_2d[`row_2d',1] = `treated_mean'
    matrix table_2d[`row_2d',2] = `control_mean'
    matrix table_2d[`row_2d',3] = `treated_sd'
    matrix table_2d[`row_2d',4] = `control_sd'
    matrix table_2d[`row_2d',5] = `diff_mean'
    matrix table_2d[`row_2d',6] = `se_diff'
    
    local row_2d = `row_2d' + 1
}

matrix colnames table_2d = TreatedMean_treat ControlMean_treat TreatedSD_treat ControlSD_treat DiffMean_treat SE_Diff_treat
matrix rownames table_2d = age educ black hisp re74 re75

matrix list table_2d
	
	/* (ii) Add the corresponding columns to TABLE 1. */
	
matrix table_2d_new = J(7,6,.)
	
forvalues i = 1/4 {
    forvalues j = 1/6 {
        matrix table_2d_new[`i', `j'] = table_2d[`i', `j']
    }
}
forvalues i = 5/6 {
    forvalues j = 1/6 {
        matrix table_2d_new[`i'+1, `j'] = table_2d[`i', `j']
    }
}

matrix list table_2d_new

matrix drop table_2d
matrix table_2d = table_2d_new

matrix list table_2d
	
matrix table_1a_2a_2d = table_1a_2a, table_2d

esttab matrix(table_1a_2a_2d) using "ps1/ps1_output/table_1.tex", replace tex ///
    title("Balance Check Across Treatment and Control") ///
    cells("result(fmt(3))") ///
	nomtitles
	
	/* (iii) What you find corresponds to your expectations? */
	
		/* A: As we randomly assigned the treatment one should expect almost all variables to be balanced with minor exceptions due to chance (1/20 with 95% level confidence intervals). This is indeed what we observe: all variables are statistically balanced when using random assignment to the fake treatment. It was also more likely to observe them all balanced as we are dealing with relatively few variables. Experimental data gave a different picture, almost completely imbalanced, a difference that is due to the very different nature of the data. */

/* (e)  */

	/* (i) Sequentially add the output of the following regressions to TABLE 2:
		(1) re78 on treated;
		(2) re78 on treated age educ black hisp;
		(3) re78 on treated age educ black hisp re74 re75. */

regress re78 treated age educ black hisp re74 re75, vce(robust)

count if e(sample) & train==0
scalar controls2 = r(N)
count if e(sample) & train==1
scalar treated2 = r(N)

regress re78 treated, vce(robust)
outreg2 using table_2.tex, append se bdec(3) sdec(3) ///
addstat("Treated", treated2, "Controls", controls2) ctitle("Regression 2e_1")

regress re78 treated age educ black hisp, vce(robust)
outreg2 using table_2.tex, append se bdec(3) sdec(3) ///
addstat("Treated", treated2, "Controls", controls2) ctitle("Regression 2e_2")

regress re78 treated age educ black hisp re74 re75, vce(robust)
outreg2 using table_2.tex, append se bdec(3) sdec(3) ///
addstat("Treated", treated2, "Controls", controls2) ctitle("Regression 2e_3")


	
	/* (ii) Add lines in the table with the number of controls and treated in each regression. */
	
		/* A: Done above */
	
	/* (iii) Comment on what you find. Is it what you expected? */
	
		/* A: We expect pseudo-treatment to have no effect as it is random in nature. Indeed, starting in regression 1) we find a slightly negative yet insignificant coefficient for the treatment variable. Adding covariates should improve efficiency by lowering standard errors: in regression 2) and 3) the point estimate of treatment moves closer to 0 but remains statistically insignificant and confidence intervals get progressively smaller. Some covariates become significant in explaining the outcome, namely age, education, and previous earnings, while ethnicities do not show any statistical association (Hispanic is significant only at the 10% level).  */

/* (f) */

	/* (i) Sequentially add the output of the following regressions to TABLE 2:
		(1) re78 on train;
		(2) re78 on train age educ black hisp;
		(3) re78 on train age educ black hisp re74 re75. */

regress re78 train age educ black hisp re74 re75, vce(robust)

count if e(sample) & train==0
scalar controls3 = r(N)
count if e(sample) & train==1
scalar treated3 = r(N)

regress re78 train, vce(robust)
outreg2 using table_2.tex, append se bdec(3) sdec(3) ///
addstat("Treated", treated3, "Controls", controls3) ctitle("Regression 2f_1")

regress re78 train age educ black hisp, vce(robust)
outreg2 using table_2.tex, append se bdec(3) sdec(3) ///
addstat("Treated", treated3, "Controls", controls3) ctitle("Regression 2f_2")

regress re78 train age educ black hisp re74 re75, vce(robust)
outreg2 using table_2.tex, append se bdec(3) sdec(3) ///
addstat("Treated", treated3, "Controls", controls3) ctitle("Regression 2f_3")

		

	/* (ii) Add lines in the table with the number of controls and treated in each regression. */

		/* A: Done above */
	
	/* (iii) Compare the results with the first three columns of TABLE 2.  Comment on what you find. Is it what you expected? Are your results sensitive to the introduction of covariates? */ 
	
		/* A: As jtrain3 includes a non-experimental control, we expect to see a different behavior compared to experimental data (jtrain2) and higher sensitivity to adding covariates that might be capturing some endogenous selection. Regression 1) yields a significant and strong negative coefficient for the training program (-15.20). This is different from the positive effect displayed in the first column (1.794), where the magnitude was also notably lower. Adding covariates in jtrain3 progressively reduces the point estimate: in regression 3) the estimated coefficient for train drops closely to 0 and becomes statistically insignificant, in a quite similar fashion to the previous subpoint. Conversely, the positive effect and the magnitude was robust to the introduction of other covariates when using jtrain2. Covariates also show change in magnitude and significance after adding controls. An example is "age" that changes sign and drops in magnitude after controlling for real earnings. */


*=============================================================================
/* 								Question 3 									*/
/* So far we have selected the covariates to be added to the regression ourselves. We will now use regularization methods to perform this selection in a data-driven approach.
You may use the lassopack package in Stata or the hdm package in R to perform your analysis. To answer the questions below, read Belloni et al. (2014) to understand the "double selection" procedure and check the help files of the commands above in the language you chose. */
*=============================================================================

use "https://raw.githubusercontent.com/sbernardoni/microeconometrics-ps/06b798693174efb8e85c8f805ac242c8fe9d2302/ps1/ps1_data/jtrain2.dta", clear

/* (a) Revisit your analysis of the data set jtrain2 in exercise 1 as a post-Lasso OLS estimation. */

	/* (i) To do this, in the first step you should perform a Lasso regression of re78 on age educ black hisp re74 re75. */
	
set seed 20295
	
rlasso re78 age educ black hisp re74 re75
	
		/* IMPORTANT NOTE: for the sake of clarity, our first attempt revolved around the robust lasso approach, here implemented by rlasso. Nonetheless, we faced difficulties as none of the variables would end up resulting statistically significant, exception made for the constant.
		
		Given this lack of results, we decided to relax our assumptions and implement a standard lasso approach, here exemplified by the command `lasso linear' */

lasso linear re78 age educ black hisp re74 re75
lassocoef
	
	/* (ii) Then, in a second step, run an OLS regression of re78 on train and all the variables selected in the first step. */
	
		/* A: The Lasso procedure selected all variables in the original covariate list with the exception of `hisp'. */
	
regress re78 train age educ black re74 re75
	
	/* (iii) Discuss your results. What are the issues of performing inference based on such a regression? */

		/* A: The resulting post-Lasso OLS regression yields a positive treatment coefficient of approximately 1.675, which not only reinforces the findings from Exercise 1(a) but also produces a slightly lower p-value, suggesting a marginally more robust model specification.

		Regarding the inferential approach, however, performing inference after post-Lasso OLS raises several important concerns that stem from the data-driven nature of the model selection step. This additional source of randomness is typically ignored by conventional OLS inference, resulting in standard errors and confidence intervals that are too narrow and p-values that may misrepresent the true level of uncertainty.

		Furthermore, applying Lasso solely to the outcome equation risks inadvertently dropping variables that, while only moderately predictive of the outcome, are strongly correlated with the treatment variable. Such omissions can introduce omitted-variable bias into the treatment effect estimate. Even when the model is subsequently re-estimated via OLS on the selected variables — the so-called post-Lasso step — the bias originating from the selection stage may persist. More broadly, Lasso's regularization not only shrinks coefficient estimates toward zero but also makes the selection process sensitive to the choice of penalty level, further complicating inference.

		In short, while post-Lasso OLS offers a useful dimensionality-reduction strategy in high-dimensional settings, the resulting inference is subject to important limitations. The additional variability introduced by the selection process, combined with the risks of omitted-variable bias and regularization bias, renders conventional OLS standard errors insufficient. The double selection procedure represents an improvement, but it too requires specialized inferential adjustments to yield reliable conclusions. */

/* (b) Now perform the "double selection" procedure as described by Belloni et al. (2014). We will perform this for two sets of variables in the exercises below. For each of these cases, you should first perform the "double selection" procedure directly using pdslasso in Stata or rlassoEffect in R and then check each step of this selection by running rlasso either in Stata or R.*/

	/* (i) In a first step, perform the "double selection" on the original variable list age educ black hisp re74 re75. Comment on your results. */
	
pdslasso re78 train (age black hisp re74 re75), rlasso loptions(robust)

rlasso re78 educ age black hisp re74 re75
rlasso train educ age black hisp re74 re75

		/* A: Nothing is selected */

	/* A: We implemented the double selection procedure following the approach described by Belloni et al. (2014). The procedure involves two selection steps: one targeting the outcome (re78) and one targeting the treatment variable (train). In both steps, none of the candidate controls — age, black, hisp, re74, and re75 — were selected by the Lasso. That is, the procedure did not add any extra controls beyond the constant term.

		The final structural equation, estimated using CHS lasso-orthogonalized variables, yields a statistically significant treatment coefficient of approximately 1.79 (standard error 0.63, p = 0.004). This indicates that, even after allowing for data-driven selection of additional controls, the estimated effect of the training program remains robust and significant.

		The absence of additional selected controls suggests that the candidate confounders do not contribute meaningfully to explaining variation in re78 or in treatment assignment beyond what is already captured by the baseline specification. The double selection procedure therefore confirms the adequacy of the original covariate set and the robustness of the estimated treatment effect. */
	
	/* (ii) Now increase the potential selected features by creating dummies for each of the age and educ levels (you're also free to add other variables, such as interactions between controls). Discuss your results and the improvements provided by the "double selection" procedure with respect to the one performed in Q3(a) */
	
		/* A1: Given the limited sample size, we first constructed broader age and education groupings to ensure sufficient observations within each category before attempting any interpretation of the selected controls. */
	
egen agegrp = cut(age), group(4)
tabulate agegrp, generate(agegrp_d)

egen educgrp = cut(educ), group(4)
tabulate educgrp, generate(educgrp_d)

pdslasso re78 train (age educ black hisp re74 re75 agegrp_d1 agegrp_d2 agegrp_d3 agegrp_d4 educgrp_d1 educgrp_d2 educgrp_d3 educgrp_d4), rlasso loptions(robust)

		/* A: We extended the pool of potential controls by introducing categorical dummies for age and education, dividing each variable into four groups using the egen command. The aim was to allow for more flexible, nonlinear effects of these variables and to test whether the richer functional form might lead the selection procedure to retain additional controls.

			The results, however, remain consistent with the previous specification: the double selection procedure again selected no additional controls, and the estimated treatment effect remained at approximately 1.79 with the same standard error and level of statistical significance. This finding suggests that, even when a richer set of functional forms is considered, the additional variables do not contribute meaningful explanatory power for either the outcome or treatment assignment. The original continuous measures of age and education appear to adequately capture the relevant variation, and the covariate balance between treatment and control groups appears sound. */

		/* A2: Upon reflection, it became clear that interpreting individual selected controls was overly ambitious given the sample size. We therefore reframed the double selection exercise primarily as a balance check rather than an inferential tool. To this end, we included the full set of age fixed effects (i.age), education fixed effects (i.educ) and all pairwise interaction terms (c.educ##c.age). */

pdslasso re78 train (i.educ i.age c.educ##c.age black hisp re74 re75), rlasso loptions(robust)

rlasso train i.educ i.age c.educ##c.age black hisp re74 re75
rlasso re78 i.educ i.age c.educ##c.age black hisp re74 re75

count if age == 34
count if age == 46

		/* A3: Under this expanded specification, the procedure selected only ages 34 and 46. However, given that there are only 6 observations for age = 34 and 3 observations for age = 46, no meaningful econometric interpretation can be offered for these selections. */
		
gen age_34 = (age == 34)
gen age_46 = (age == 46)

regress re78 train age_34 age_46
	
	/* (iii) What can you say about the balance of the characteristics of the treatment and control group based on the selected variables? */
	
		/* A: The results from the double selection procedure — both with the original covariate set and the expanded specification including dummies for age and education — consistently suggest that the treatment and control groups are well balanced with respect to observed characteristics. In neither case did the procedure select additional controls, indicating that none of the candidate predictors are strongly associated with either the outcome or treatment assignment beyond what the baseline specification already captures, with the limited exception of the nine observations tied to the selected age values.

			This absence of additional selected variables implies that the observable covariates are comparably distributed across the treatment and control groups, and that any differences in outcomes can be more confidently attributed to the training program rather than to pre-existing imbalances. It bears noting, however, that balance on observed characteristics does not rule out imbalance on unobserved factors. Nonetheless, the evidence from the data-driven selection process points to a strong degree of balance on measurable characteristics, thereby supporting the credibility of the estimated treatment effect. */


*=============================================================================
/* 								Question 4 									*/
/* Over time, several articles have revisited LaLonde's results and provided a rich discussion about the best practices when working with observational data. A recent article, "Comparing Experimental and Nonexperimental Methods: What Lessons Have We Learned Four Decades after LaLonde (1986)?", summarizes this debate and discusses some of the recent advances on the topic. Read Imbens and Xu (2025) and
answer the questions below. */
*=============================================================================



/* (b) The results from (Dehejia and Wahba, 1999) showed that observational methods could replicate the experimental results in LaLonde (1986)’s setting. Discuss the implications of the points discussed by Imbens and Xu (2025) on this debate. Can we interpret the results from Dehejia and Wahba (1999) as causal? How do you connect your results in point (a) to this discussion? */


		/* Imbens and Xu (2025) draw a crucial distinction between the *statistical estimand* and the *causal estimand*. The former is a function of observed data, and the adoption of modern methods — such as doubly robust estimators — has yielded more robust and precise estimates of it. The latter is the Average Treatment Effect on the Treated (ATT), which can only be recovered from the statistical estimand under an additional identifying assumption: unconfoundedness. Crucially, improving estimation of the statistical estimand does not guarantee convergence to the true causal effect. Whether it does depends on the plausibility of unconfoundedness, which must be assessed through diagnostic exercises such as placebo tests.

Dehejia and Wahba (1999) advanced the debate by introducing propensity score stratification and matching, paired with a principled trimming strategy to improve comparability between the nonexperimental control group and the treated units. This refined LaLonde's (1986) original ad hoc subsetting approach and, as Imbens and Xu (2025) note, anticipated the now-standard practice of assessing overlap through the propensity score distribution. However, two limitations prevent interpreting their estimates as causal. First, they focus exclusively on the ATT without examining treatment effect heterogeneity, which subsequent literature has shown to be substantial. Second, and more fundamentally, they conduct no formal placebo tests, which have since become standard practice for assessing unconfoundedness.

Our results in point (a) reinforce and sharpen this conclusion. Table 3 reports OLS estimates of the treatment effect on 1978 earnings (columns 1–3) and on 1975 earnings as a placebo outcome (columns 4–6), across the full sample and two trimmed samples based on logistic and random forest propensity scores. Using the experimental estimate from jtrain2 ($1,794) as the external benchmark, none of the re78 estimates are close to it: the full-sample estimate is −$929 (insignificant), the logit-trimmed estimate is −$982 (insignificant), and the random forest-trimmed estimate is −$4,518 (significant at 1%). Rather than converging toward the experimental benchmark, trimming based on the random forest propensity score worsens the estimate. This is consistent with the trimming statistics from part (a)(3), where the random forest specification discards a larger fraction of treated units than the logit, suggesting that its more aggressive trimming distorts the composition of the remaining sample in ways that amplify rather than attenuate selection bias.

The placebo results are unambiguous. The estimated effect on 1975 earnings — a pre-program outcome that could not have been causally affected by training — is −$2,006 (p < 0.01) in the full sample, −$2,263 (p < 0.01) with logit trimming, and −$3,550 (p < 0.01) with random forest trimming. The fact that trimming not only fails to eliminate but amplifies the placebo effect provides clear evidence against unconfoundedness: even after restricting to the region of common support, treated and PSID control units differ systematically on unobserved dimensions correlated with earnings. Neither propensity score specification resolves this fundamental identification problem.

In sum, while Dehejia and Wahba (1999) made a valuable methodological contribution by improving the statistical estimand through propensity score methods, their results should not be interpreted as causal. The evidence from Imbens and Xu (2025), corroborated by our own analysis in Table 3, strongly suggests that unconfoundedness does not hold in the PSID nonexperimental setting: the treatment and comparison groups differ on unobserved characteristics in ways that no trimming or reweighting strategy based on observed covariates can adequately address. */



*=============================================================================
/* 								Question 5 									*/
/* Use the jtrain2 data set.
Read Athey and Imbens (2017) (focus on those sections where the authors discuss how to perform inference in completely randomized experiments; in particular, section 4). */
*=============================================================================

/* (a) Under which conditions, allowing for heterogeneous treatment effects, is Neyman's inference unbiased? */

	/*A: The estimator suggested by Neyman for the average treatment effect for the sample being analysed is the difference in average outcomes by treatment status – that is, between the treatment and the control group. Allowing for heterogeneous treatment effects, the unbiasedness of this estimator relies on pure randomisation (independence of treatment assignment and potential outcomes), which implies that all potential outcomes are fixed and therefore that the expected value of the second term of the estimator is zero. The unbiasedness of the standard error of Neyman's estimator requires, on the other hand, that it is possible to view the sample under analysis as a random sample from an infinite population. In this case, Neyman's estimator is interpreted as an estimator for the population average treatment effect instead of the sample average treatment effect. */

/* (b) Describe Fisher's inference and replicate section 4.1 of Athey and Imbens (2017) in Stata. Do you arrive at their same p-value? If not, why? Hint: Note that you can draw motivation from third-parties for your own answer; for this case, we suggest that you read Heß (2017).*/ 

	/*A: Fisher's work on inference relies on the idea of testing the sharp null hypothesis – that is a null hypothesis under which it is possible to infer all the missing potential outcomes from the observed ones. Generally, the sharp null hypothesis is that the treatment has no effect, with the alternative being that there exists at least one unit such that the two potential outcomes are different. The advantage of this approach is that it allows the researcher to infer, for any statistic that is a function of the observed outcomes, the treatment, and the covariates, its exact distribution under the null. Hence, it is possible to calculate the probability of such statistic taking on a value that is as large in absolute value as the one observed in the experimental setting, and to eventually reject the null hypothesis (or fail to do so).*/

* (b)

use "https://raw.githubusercontent.com/sbernardoni/microeconometrics-ps/06b798693174efb8e85c8f805ac242c8fe9d2302/ps1/ps1_data/jtrain2.dta", clear

/* We set the seed as 20295 to ensure replicability of the results */

/* Maintaining the default of 100 permutations */
ritest train _b[train], seed(20295): ///
	reg re78 train 

/* Changing the number of permutations to 1000 */
ritest train _b[train], reps(1000) seed(20295): ///
	reg re78 train 

/* Changing the number of permutations to 10000 */
ritest train _b[train], reps(10000) seed(20295): ///
    reg re78 train 

	/* A: We replicated the results in section 4.1 of Athey and Imbens (2017) using the methodology described in Heß (2017). With 100 iterations (the default setting in the ritest command), we obtain an estimated p-value that is approximately 0. Repeating the analysis with 1000 iterations instead, we obtain an estimated p-value of 0.002, with a 95% confidence interval between 0.0002 and 0.0072. Finally, we repeated the test with 10000 random permutations: the estimated p-value is 0.0048, with a 95% confidence interval between 0.0035 and 0.0064. The point estimate is very similar to the one obtained by Athey and Imbens (2017) and suggests the rejection of the null hypothesis of no treatment effect. The slight numerical difference between the two results is most likely due to the randomness of the permutations. */

/* (c) Read again the randomization plan in LaLonde (1986). On which grounds Athey and Imbens (2017)'s illustration of Fisherian inference on LaLonde (1986)'s paper could be criticized? */

	/* A: Comparing the randomisation plan in LaLonde (1986) and in Athey and Imbens (2017), we can observe that there are some differences that may affect the estimation of the p-values. Indeed, in the original paper the treatment was administered by different agencies located in 10 separate sites, that provided different work experiences and even different types of work. On the other hand, the Fisherian inference approach assigns the treatment randomly across the sample without taking into account the role of the individual site. Hence, if there exist site-specific factors affecting the assignment process or the treatment outcomes, the approach by Athey and Imbens (2017) may lead to biased estimates of the p-values, and therefore to an incorrect evaluation of the significance of the treatment effect. */

/* (d) The article Channeling Fisher: Randomization Tests and the Statistical Insignificance of Seemingly Significant Experimental Results (Young, 2019) presents the results of an exercise to test the null hypothesis of no treatment effects in a series of experimental papers recently published in AEA journals, showing that many of the coefficients reported in those papers are no longer significant in a randomization test. A critique of this paper has been published by professors Uri Johnson, Leif Nelson and Joe Simmons in their blog, Data Colada. Read their post here and answer the questions below. */

	/* (i) Briefly explain the difference between the procedure used as the default in Stata for the calculation of standard errors (HC1) and the one proposed by the Data Colada post (HC3). */
	
		/* A: HC1 robust standard errors are the default setting in Stata. In this framework, the diagonal elements of the variance-covariance matrix are the squared residuals weighted by the coefficient n/(n-k), where k is the number of regressors. This allows the conditional variance of the errors to be non-constant, which departs from the classical assumptions of the linear regression model. On the contrary, with HC3 robust standard errors the diagonal entries of the variance-covariance matrix are the squared residuals weighted by (1-h)^2, where h is the leverage of each observation in the regression model and ranges from 0 to 1. This approach is widely used and considered as preferrable under conditional heteroskedasticity. */
	
	/* (ii) Using the dataset jtrain2, rerun the analysis you have performed in exercise 1, now calculating the standard errors based on HC3 (this is done in Stata using the option vce() in your regression command). */

/* First regression */
regress re78 train, vce(hc3)

/* Second regression */
regress re78 train age educ black hisp, vce(hc3) 

/* Third regression */
regress re78 train age educ black hisp re74 re75, vce(hc3)



	/* (iii) Perform a third version of your analysis, now based on bootstrapping (use the bootstrap command in Stata). Briefly describe how the standard errors are calculated in this approach. */

/* First regression */
bootstrap _b, reps(1000): regress re78 train

/* Second regression */
bootstrap _b, reps(1000): regress re78 train age educ black hisp

/* Third regression */
bootstrap _b, reps(1000): regress re78 train age educ black hisp re74 re75

		/* A: Bootstrapping is a non-parametric statistical method where the sampling variation of an estimate is obtained by resampling the data with replacement multiple times. For each resample obtained with this procedure (the standard on Stata is 50), the statistic of interest is then computed as usual. Finally, the standard deviation of the statistic across replications is obtained, which is called the bootstrap standard error. */
	
	/* (iv) Do any of your conclusions regarding the effect of the training program change based on the analysis performed in this exercise? Based on the discussion provided in the Data Colada post, can you think of a reason for why your results using HC3 should or shouldn't change for this exercise? 

		/* A: The results obtained in this exercise are essentially the same as the ones obtained in exercise 1. Indeed, the coefficients in the three specification are remarkably similar, and there is only a small variation in their standard errors, with no effect on their significance. This suggests that our analysis is robust, and that the results obtained – as well as the the conclusions that can be drawn from them – are not conditional on a certain specification.

As expected from their formulation, the robust standard errors with the HC3 specification are slightly higher when compared to the others. Nonetheless, as suggested by the Data Colada post, the fact that our results are unchanged after the application of HC3 robust standard errors is not surprising: the difference in terms of performance between the specifications happens in fact with samples under 250 observations, which is not the case in this context. */
	
