*============================================================================
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
ssc install rforest, replace
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

if ("`user'" == "flore") {
    global filepath 
"C:\Users\flore\OneDrive\Documents\Bocconi\Year 2\Microeconometrics\PS 1\files"
	global output 
"\Users\flore\OneDrive\Documents\Bocconi\Year 2\Microeconometrics\PS 1\files\outputs"
}


if ("`user'" == "gabrielemole") {
    global filepath "/Users/stealth/Documenti/GitHub/20295-microeconometrics-ps/ps1"
	global output "/Users/stealth/Documenti/GitHub/20295-microeconometrics-ps/ps1/ps1_output"
}

*=============================================================================
/* 								Question 1 									*/
/* Use the file jtrain2 													*/
*=============================================================================

use "https://github.com/sbernardoni/microeconometrics-ps/raw/06b798693174efb8e85c8f805ac242c8fe9d2302/ps1/ps1_data/jtrain2.dta", clear

/* (a) Construct a table checking for balance across treatment and control for the following covariates: age educ black hisp nodegree re74 re75.
Name it TABLE_1.
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

esttab matrix(table_1a) using "$output/table_1.tex", replace ///
    title("Balance Check Across Treatment and Control") ///
    cells("result(fmt(3))") ///
	nomtitles
	
	/* Comment: Overall, we observe that most of the 7 covariates are balanced between treatment and control groups.
	
In particular, 5 of the variables (age, education, past earnings (re74 and re75), and the indicator for being black) do not have significant differences in means between treatment and control group. By contrast, the proportion of individuals without a degree (variable "nodegree") differs significantly between treatment group and control group, as does, to a lesser extent (10% confidence), the "hisp" variable.

From a well-implemented experimental design, we would expect no noteworthy imbalances in the distribution of major covariates between treatment and control group. This is due to the random nature of treatment assignment in experimental designs. While we do observe balance between the two groups for 5 out of 7 covariates, the remaining imbalances in the "nodegree" and "hisp" variables raise concerns about systematic differences in covariates that might be correlated with treatment effects. While this is not an atypical observation in finite samples, we do have to consider adjusting our sample further to avoid biased estimates. For example, we could control for the concerned covariates in the regression. Furthermore, our observations do not limit the credibility of the original experimental design, as we are dealing with only a subsample of the original sample.
 */

/* (b) Regress re78 on train.
Save the estimate and the standard error of the coefficient on train as scalars.
Interpret the coefficient. */

regress re78 train, vce(robust)

scalar coef1 = _b[train]
scalar se1 = _se[train]

	/* Comment: The coefficient resulting from regressing real earning in 1978 on the treatment dummy is significantly positive (at 5% confidence) with a value of 1.794343; this implies that real earnings, in our experiment, were positively correlated with the training programme. Nevertheless, the rather low R^2 of 1.78% indicates that other factors than treatment were responsible for the vast portion of the variation in real earnings.*/

/* (c) Construct a table by sequentially adding the output of the following regressions to each column:
(1) re78 on train;
(2) re78 on train age educ black hisp;
(3) re78 on train age educ black hisp re74 re75;
Add rows to the table with the number of controls and treated in each regression. Name it TABLE 2. 
Are your results sensitive to the introduction of covariates?*/

regress re78 train age educ black hisp re74 re75, vce(robust)

count if e(sample) & train==0
scalar controls1 = r(N)
count if e(sample) & train==1
scalar treated1 = r(N)

regress re78 train, vce(robust)
outreg2 using "$output/table_2.tex", replace se bdec(3) sdec(3) ///
addstat("Treated", treated1, "Controls", controls1) ctitle("Regression 1c_1")

regress re78 train age educ black hisp, vce(robust)
outreg2 using "$output/table_2.tex", append se bdec(3) sdec(3) ///
addstat("Treated", treated1, "Controls", controls1) ctitle("Regression 1c_2")

regress re78 train age educ black hisp re74 re75, vce(robust)
outreg2 using "$output/table_2.tex", append se bdec(3) sdec(3) ///
addstat("Treated", treated1, "Controls", controls1) ctitle("Regression 1c_3")

	/* Comment: The regression of re78 on train yields an average treatment effect of 1.79 (in units: 1800USD per year) with a standard error of 0.63. When introducing additional covariates, first age, educ, black, and hisp in regression 2 and eventually re74 and re75 in regression 3, the estimated coefficient for train slightly decreases to around 1.69 and 1.68, respectively, while the standard errors remain essentially the same. As neither estimated average effects nor standard errors change substantially, the treatment effect is fairly robust to adding covariates. Omitted variable bias therefore does not seem to be a major limitation to our model, in the sense that the covariates we controlled for do not do not substantially confound the estimation of the relationship between treatment and outcome. */

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

esttab trim3 trim5 trim10 using "$output/reg_no_influence.tex", replace tex ///
    title("Regression Results After Removing Extreme Influence Observations") ///
    stats(N, fmt(%9.0g) label("N")) ///
	nomtitles 
	
	
/* Comment: The full-sample baseline effect of treatment on re78 is about 1.68 and statistically significant (p = 0.008). After removing the 3 most extreme observations from each sample tail, the coefficient drops to about 1.36 (p = 0.009).
After removing the 5 most extreme observations from each sample tail, the coefficient drops further to 1.22 (p = 0.015). 
After removing the 10 most extreme observations from each sample tail, the coefficient equals 1.02 (p = 0.029). 
Hence, we do observe a decrease in the estimated magnitude of the effect of treatment upon removal of extreme values. Nevertheless, significance and direction (i.e., positivity) of the effect do not hinge on limit observations. 
Thus, why we can indeed rely on effectiveness of the treatment (in the sense of a significant effect in the desired direction), we do have to keep in mind that the size of the effect was quite considerably influenced by a few extreme cases.*/

*=============================================================================
/* 								Question 2 									*/
/* Use the jtrain3 															*/
*=============================================================================

* SBLOCCA STO COSO PER AVERE ACCESSO ALL'ALTRO DATASET | NON USARLI ENTRAMBI, QUESTO CANCELLA IL DATASET PRECEDENTE *

use "https://github.com/sbernardoni/microeconometrics-ps/blob/06b798693174efb8e85c8f805ac242c8fe9d2302/ps1/ps1_data/jtrain3.dta", clear

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

esttab matrix(table_1a_2a) using "$output/table_1.tex", replace tex ///
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

	/* A: The correlation is close to 0 and not statistically significant. This is consistent with random assignments with different algorithms. If an assignment is truly random it should be uncorrelated with other random assignments adopting different techniques even when using the same seed.*/

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

esttab matrix(table_1a_2a_2d) using "$output/table_1.tex", replace tex ///
    title("Balance Check Across Treatment and Control") ///
    cells("result(fmt(3))") ///
	nomtitles
	
	/* (iii) What you find corresponds to your expectations? */
	
		/* A: All variables are statistically balanced when using random assignment to the fake treatment. This is coherent with theoretical expectations as the treatment is randomly assigned and one should expect that almost all variables are balanced, possibly with some exceptions due to chance. As we are dealing with relatively few variables it was more likely to observe them all balanced. Experimental data gave a different picture, almost completely imbalanced, a difference that is due to the very different nature of the data. */

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
outreg2 using "$output/table_2.tex", append se bdec(3) sdec(3) ///
addstat("Treated", treated2, "Controls", controls2) ctitle("Regression 2e_1")

regress re78 treated age educ black hisp, vce(robust)
outreg2 using "$output/table_2.tex", append se bdec(3) sdec(3) ///
addstat("Treated", treated2, "Controls", controls2) ctitle("Regression 2e_2")

regress re78 treated age educ black hisp re74 re75, vce(robust)
outreg2 using "$output/table_2.tex", append se bdec(3) sdec(3) ///
addstat("Treated", treated2, "Controls", controls2) ctitle("Regression 2e_3")


	
	/* (ii) Add lines in the table with the number of controls and treated in each regression. */
	
		/* A: Done above */
	
	/* (iii) Comment on what you find. Is it what you expected? */
	
		/* A: In the first regression, the treatment dummy is slightly negative yet insignificant. After controlling for other covariates, the point estimate moves closer to 0 and remains statistically insignificant. As expected, adding covariates slightly improves standard errors for the treatment dummy. This is in line with the assignment of a random pseudo-treatment, hence yielding a null effect due to his random nature. Some covariates become significant in explaining the outcome, namely age, education, and previous earnings, while ethnicities do not show any statistical association (Hispanic is significant only at the 10% level).  */

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
outreg2 using "$output/table_2.tex", append se bdec(3) sdec(3) ///
addstat("Treated", treated3, "Controls", controls3) ctitle("Regression 2f_1")

regress re78 train age educ black hisp, vce(robust)
outreg2 using "$output/table_2.tex", append se bdec(3) sdec(3) ///
addstat("Treated", treated3, "Controls", controls3) ctitle("Regression 2f_2")

regress re78 train age educ black hisp re74 re75, vce(robust)
outreg2 using "$output/table_2.tex", append se bdec(3) sdec(3) ///
addstat("Treated", treated3, "Controls", controls3) ctitle("Regression 2f_3")

		

	/* (ii) Add lines in the table with the number of controls and treated in each regression. */

		/* A: Done above */
	
	/* (iii) Compare the results with the first three columns of TABLE 2.  Comment on what you find. Is it what you expected? Are your results sensitive to the introduction of covariates? */ 
	
		/* A: The first regression of real earnings on the training program shows a significant and strong negative effect of the training program (-15.20), differently from the positive effect displayed in the first column (1.794), where the magnitude was also notably lower. While the positive effect and the magnitude was robust to the introduction of other covariates when using jtrain2, the "treatment effect" disappears in jtrain3 after controlling for the other variables. Adding controls to jtrain3 makes the point estimate gradually drop to a slightly positive value close to 0 and lose its statistical significance, partially resembling the result in the previous subpoint. Covariates also show change in magnitude and significance after adding controls. An example is "age" that changes sign and drops in magnitude after controlling for real earnings. This difference is due to the different nature of the datasets, namely experimental and non-experimental, and as such it was in line with our expectations to find discrepancies.  */

*=============================================================================
/* 								Question 3 									*/
/* So far we have selected the covariates to be added to the regression ourselves. We will now use regularization methods to perform this selection in a data-driven approach.
You may use the lassopack package in Stata or the hdm package in R to perform your analysis. To answer the questions below, read Belloni et al. (2014) to understand the "double selection" procedure and check the help files of the commands above in the language you chose. */
*=============================================================================

use "https://github.com/sbernardoni/microeconometrics-ps/blob/06b798693174efb8e85c8f805ac242c8fe9d2302/ps1/ps1_data/jtrain2.dta", clear

/* (a) Revisit your analysis of the data set jtrain2 in exercise 1 as a post-Lasso OLS estimation. */

	/* (i) To do this, in the first step you should perform a Lasso regression of re78 on age educ black hisp re74 re75. */
	
set seed 20295
	
rlasso re78 age educ black hisp re74 re75
	
		/* IMPORTANT NOTE: for the sake of clarity, our first attempt revolved around the robust lasso approach, here implemented by rlasso. Nonetheless, we faced difficulties as none of the variables would end up resulting statistically significant, exception made for the constant.
		
		Given this lack of results, we decided to relax our assumptions and implement a standard lasso approach, here exemplified by the command `lasso linear' */

lasso linear re78 age educ black hisp re74 re75
lassocoef
	
	/* (ii) Then, in a second step, run an OLS regression of re78 on train and all the variables selected in the first step. */
	
		/* A: According to the output, all of our previous variables were accepted, exception made for ``hisp'' */
	
regress re78 train age educ black re74 re75
	
	/* (iii) Discuss your results. What are the issues of performing inference based on such a regression? */

		/* A: For what it concerns our regression, we obtain a positive coefficient of 1.67495: this not only reinforces our previous analysis from point (1)a, but it also provides us with a slightly lower p-value which might indicate a more robust model specification.
		
		For what it concerns the approach, still, performing inference after a post‐Lasso OLS regression raises several challenges that arise largely from the fact that the model selection step is inherently data‐driven. This extra randomness is typically ignored by conventional OLS inference, leading to standard errors and confidence intervals that are too narrow and p‐values that might misrepresent the true level of uncertainty.

		Moreover, a naive approach that applies Lasso solely on the outcome equation can inadvertently drop variables that, while only moderately predictive of the outcome, are strongly correlated with the treatment variable; in this case, the issue is that such omissions risk introducing omitted‐variable bias into the treatment effect estimate. Even if one subsequently re-estimates the model using OLS on the selected variables—often called post‐Lasso—the initial selection step's bias can persist. Lasso's regularization not only shrinks coefficient estimates toward zero, but its selection process can also be sensitive to the penalty level, which further complicates the inference.

		In essence, while post‐Lasso OLS offers a useful strategy for reducing dimensionality in high-dimensional settings, the inference based on such a regression is fraught with complications. The additional variability introduced by the selection process, along with the risk of omitted-variable bias and regularization bias, means that traditional OLS standard errors are insufficient. Adopting the double selection method is a step in the right direction, but it also necessitates specialized adjustments in the inferential framework to yield reliable conclusions. */

/* (b) Now perform the "double selection" procedure as described by Belloni et al. (2014). We will perform this for two sets of variables in the exercises below. For each of these cases, you should first perform the "double selection" procedure directly using pdslasso in Stata or rlassoEffect in R and then check each step of this selection by running rlasso either in Stata or R.*/

	/* (i) In a first step, perform the "double selection" on the original variable list age educ black hisp re74 re75. Comment on your results. */
	
pdslasso re78 train (age black hisp re74 re75), rlasso loptions(robust)

rlasso re78 educ age black hisp re74 re75
rlasso train educ age black hisp re74 re75

		/* A: Nothing is selected */

	/* We implemented the double selection procedure following the approach described by Belloni et al. (2014). The procedure involves two key selection steps: one for the outcome (re78) and one for the treatment (train). In both steps, none of the candidate high-dimensional controls—age, black, hisp, re74, and re75—were selected. In other words, the lasso did not add any extra controls beyond the constant term.

		The final structural equation, estimated with CHS lasso-orthogonalized variables, yields a statistically significant coefficient for the treatment variable (train) of approximately 1.79 (standard error 0.63, p = 0.004). This result indicates that, even after allowing for a data-driven selection of additional controls, the estimated effect of the treatment remains robust and significant.

		The absence of additional selected controls suggests that the potential confounders in our original variable list do not contribute significantly to explaining the variation in re78 or the treatment assignment beyond what is already captured. Consequently, our original covariate specification appears adequate, and the double selection procedure confirms the robustness of the estimated treatment effect. */
	
	/* (ii) Now increase the potential selected features by creating dummies for each of the age and educ levels (you're also free to add other variables, such as interactions between controls). Discuss your results and the improvements provided by the "double selection" procedure with respect to the one performed in Q3(a) */
	
		/* A1: Given the limited size of our sample, we first decided to create larger groups that could include more observations in order to be able to interpret the controls. */
	
egen agegrp = cut(age), group(4)
tabulate agegrp, generate(agegrp_d)

egen educgrp = cut(educ), group(4)
tabulate educgrp, generate(educgrp_d)

pdslasso re78 train (age educ black hisp re74 re75 agegrp_d1 agegrp_d2 agegrp_d3 agegrp_d4 educgrp_d1 educgrp_d2 educgrp_d3 educgrp_d4), rlasso loptions(robust)

		/* A1preliminary: In this part, we increased the pool of potential controls by creating categorical dummies for age and education. Specifically, we divided age and education into 6 groups each (using the egen command and then generating dummies), and then added these new variables to the existing list of controls. The purpose was to allow for more flexible (nonlinear) effects of age and education and to test whether these additional features might improve the selection process.

			After running the double selection procedure with this expanded set, the results remained consistent with the previous specification: the procedure still did not select any extra controls beyond those already included in the original model with robust lasso (the one we initially decided to discard as it was not providing statistically significant variables), and the estimated effect of the treatment variable remained at approximately 1.79 (with the same standard error and level of significance).

			This finding is quite informative. It suggests that even when we allow for a richer set of functional forms (through dummies for age and educ), the additional variables do not add explanatory power for predicting either the outcome or the treatment assignment. In other words, the balance between the treatment and control groups with respect to these characteristics appears to be good, and the original continuous measures of age and education already capture the necessary variation. Thus, the double selection procedure confirms the robustness of my original model and implies that the covariate balance is adequate. */

		/* A2: Then, it became apparent that it was too optimistic to interpret the controls, so we decided to use the pdslasso procedure more as of a "balance check" than as a tool to make inference for the variables/controls; we hence include all of the values for age (i.age), all of the values for education (i.educ) and all of the interaction terms (c.educ##c.age) */

pdslasso re78 train (i.educ i.age c.educ##c.age black hisp re74 re75), rlasso loptions(robust)

rlasso train i.educ i.age c.educ##c.age black hisp re74 re75
rlasso re78 i.educ i.age c.educ##c.age black hisp re74 re75

count if age == 34
count if age == 46

		/* A3: Only the ages of 34 and 46 are selected: nonetheless, we are unable to offer an econometric interpretation due to the fact that there are only 6 observations for age = 34 and 3 observations for age = 46. */
		
gen age_34 = (age == 34)
gen age_46 = (age == 46)

regress re78 train age_34 age_46
	
	/* (iii) What can you say about the balance of the characteristics of the treatment and control group based on the selected variables? */
	
		/* A: The results from the double selection procedure—both with the original covariate set and with the expanded set including dummies for age and educ—suggest that the treatment and control groups are well balanced with respect to the observed characteristics. Specifically, the procedure did not select any additional controls beyond those initially specified. This outcome indicates that none of the extra potential predictors (whether in their continuous form or as categorical dummies) were strongly associated with either the outcome (re78) or the treatment assignment (train) beyond what was already captured.

			This absence of additional selected variables, exception made for the 9 observations for age, implies that the observable covariates are comparably distributed between the treatment and control groups; In other words, the groups do not differ systematically on these characteristics. Consequently, any differences in the outcome can be more confidently attributed to the training program rather than to pre-existing imbalances. This is an important confirmation because one of the key challenges in observational studies is ensuring that the treatment and control groups are similar in their observable traits.

			Nonetheless, while the balance in observed characteristics is reassuring, it is still crucial to acknowledge that this balance does not rule out the possibility of imbalance in unobserved factors. However, based on the data-driven selection process, the evidence points to a strong level of balance between the groups on the characteristics we can measure, thereby supporting the credibility of the estimated treatment effect. */
			
			
*=============================================================================
/* 								Question 4 									*/
/* Over time, several articles have revisited LaLonde's results and provided a rich discussion about the best practices when working with observational data. A recent article, "Comparing Experimental and Nonexperimental Methods: What Lessons Have We Learned Four Decades after LaLonde (1986)?", summarizes this debate and discusses some of the recent advances on the topic. Read Imbens and Xu (2025) and
answer the questions below. */
*=============================================================================

use"https://github.com/sbernardoni/microeconometrics-ps/raw/refs/heads/main/ps1/ps1_data/jtrain3.dta", clear

* init covariates
local X "age educ black hisp re74"

* Create an id for merges after H2O predictions
capture drop id_q4
gen id_q4 = _n


/* (a) Follow the approach from Imbens and Xu (2025) and estimate the ATT for the
training program based on jtrain3. For this, estimate propensity scores using
(i) logistic regression and (ii) a random forest classifier. Use the same covariate set X in both models: X = {age educ black hisp re74}                             */

/* (a)(1) Propensity score via logistic regression: Estimate \hat e logit(X) via logistic regression. Report summary statistics of \hat e logit separately for treated and controls (min/25%/median/75%/max), and produce an overlap plot (treated vs. control).*/

* logit regress train on covariates
logit train `X', vce(robust)
* extract \hat e
predict double pscore_logit, pr

* Summary statistics for pscore_logit by treatment status
matrix q4_logit_sum = J(2,5,.)
quietly summarize pscore_logit if train==1, detail
matrix q4_logit_sum[1,1] = r(min)
matrix q4_logit_sum[1,2] = r(p25)
matrix q4_logit_sum[1,3] = r(p50)
matrix q4_logit_sum[1,4] = r(p75)
matrix q4_logit_sum[1,5] = r(max)

quietly summarize pscore_logit if train==0, detail
matrix q4_logit_sum[2,1] = r(min)
matrix q4_logit_sum[2,2] = r(p25)
matrix q4_logit_sum[2,3] = r(p50)
matrix q4_logit_sum[2,4] = r(p75)
matrix q4_logit_sum[2,5] = r(max)

matrix rownames q4_logit_sum = Treated Controls
matrix colnames q4_logit_sum = Min P25 Median P75 Max
matrix list q4_logit_sum

* save
esttab matrix(q4_logit_sum) using "$output/q4_logit_ps_summary.tex", replace tex ///
    title("Logistic propensity score summary") ///
    cells("result(fmt(4))") nomtitles

* Overlap plot
twoway ///
    (histogram pscore_logit if train==1 & !missing(pscore_logit), ///
        fraction start(0) width(0.05) ///
        color(navy%40) lcolor(navy) ///
        legend(label(1 "Treated"))) ///
    (histogram pscore_logit if train==0 & !missing(pscore_logit), ///
        fraction start(0) width(0.05) ///
        color(maroon%40) lcolor(maroon) ///
        legend(label(2 "Controls"))), ///
    legend(order(1 "Treated" 2 "Controls")) ///
    xtitle("Estimated propensity score - logistic") ///
    ytitle("Share") ///
    title("Overlap plot: logistic propensity score")
graph export "ps1_output/q4_overlap_logit.png", replace


/* (a)(2) Propensity score via random forest classifier: Estimate \hat e RF(X) via a random forest (there are several specificities regarding random forests that we will not discuss in this exercise. For our scope, use the probability forest command from grf if using R or the h2oml rfbinclass command in Stata). Report the same summaries and overlap plot.        */

* The code below uses h2o. Please make sure that you have downloaded the h2o.jar file and placed it into an ado folder.

tempfile jtrain3_base rf_preds
save `jtrain3_base', replace

* initialise h2o procedure
capture noisily h2o init

_h2oframe put, into(h2o_jtrain3)
_h2oframe change h2o_jtrain3
_h2oframe factor train, replace

* Important: the second predicted probability corresponds to Pr(train==1) which we want
h2oml rfbinclass train `X', h2orseed(20295) ntrees(500)
h2omlpredict rf_pr0 rf_pr1, pr

* bring h2o frame back to Stata
clear
_h2oframe get h2o_jtrain3
keep id_q4 rf_pr0 rf_pr1
rename rf_pr1 pscore_rf
save `rf_preds', replace

use `jtrain3_base', clear
merge 1:1 id_q4 using `rf_preds', nogen

capture noisily h2o shutdown, force

* Summary statistics for pscore_rf by treatment status
matrix q4_rf_sum = J(2,5,.)
quietly summarize pscore_rf if train==1, detail
matrix q4_rf_sum[1,1] = r(min)
matrix q4_rf_sum[1,2] = r(p25)
matrix q4_rf_sum[1,3] = r(p50)
matrix q4_rf_sum[1,4] = r(p75)
matrix q4_rf_sum[1,5] = r(max)

quietly summarize pscore_rf if train==0, detail
matrix q4_rf_sum[2,1] = r(min)
matrix q4_rf_sum[2,2] = r(p25)
matrix q4_rf_sum[2,3] = r(p50)
matrix q4_rf_sum[2,4] = r(p75)
matrix q4_rf_sum[2,5] = r(max)

matrix rownames q4_rf_sum = Treated Controls
matrix colnames q4_rf_sum = Min P25 Median P75 Max
matrix list q4_rf_sum

* save
esttab matrix(q4_rf_sum) using "$output/q4_rf_ps_summary.tex", replace tex ///
    title("Question 4(a)(2): Random forest propensity score summary") ///
    cells("result(fmt(4))") nomtitles

* Overlap plot: random forest propensity score
twoway ///
    (histogram pscore_rf if train==1 & !missing(pscore_rf), ///
        fraction start(0) width(0.05) ///
        color(navy%40) lcolor(navy) ///
        legend(label(1 "Treated"))) ///
    (histogram pscore_rf if train==0 & !missing(pscore_rf), ///
        fraction start(0) width(0.05) ///
        color(maroon%40) lcolor(maroon) ///
        legend(label(2 "Controls"))), ///
    legend(order(1 "Treated" 2 "Controls")) ///
    xtitle("Estimated propensity score - random forest") ///
    ytitle("Share") ///
    title("Overlap plot: random forest propensity score")
graph export "ps1_output/q4_overlap_rf.png", replace


/* (a)(3) ATT-style trimming rule: keep if pscore <= 0.8: Following a similar strategy to the one used by Imbens and Xu (2025), apply the following ATT-style trimming rule separately for each estimator: keep unit i if pscore (i) <= 0.8. For each estimator, report: (i) the implied cutoff, (ii) the number
and fraction of treated units trimmed, and (iii) a brief characterization of which
treated units are trimmed (compare covariate means for trimmed vs. kept treated,
or provide a short table). */

* init binary vectors for kept individuals
capture drop keep_logit keep_rf trimmed_logit trimmed_rf
gen byte keep_logit = (pscore_logit <= 0.8)
gen byte keep_rf    = (pscore_rf    <= 0.8)

* init binary vectors for trimmed treated individuals 
gen byte trimmed_logit = (train==1 & keep_logit==0)
gen byte trimmed_rf    = (train==1 & keep_rf==0)

* Implied cutoff max_{W=0} \hat e(X): extract max propensity score of controls
quietly summarize pscore_logit if train==0, detail
scalar cutoff_logit_controls = r(max)

quietly summarize pscore_rf if train==0, detail
scalar cutoff_rf_controls = r(max)

display "Implied control cutoff, logistic = " cutoff_logit_controls
display "Implied control cutoff, RF       = " cutoff_rf_controls

* Number and fraction of treated units trimmed
quietly count if train==1
scalar Ntreat_total = r(N)

quietly count if trimmed_logit==1
scalar Ntreat_trim_logit = r(N)
scalar Frac_trim_logit = Ntreat_trim_logit / Ntreat_total

quietly count if trimmed_rf==1
scalar Ntreat_trim_rf = r(N)
scalar Frac_trim_rf = Ntreat_trim_rf / Ntreat_total

display "Treated trimmed (logit): " Ntreat_trim_logit " out of " Ntreat_total " = " Frac_trim_logit
display "Treated trimmed (RF):    " Ntreat_trim_rf " out of " Ntreat_total " = " Frac_trim_rf

* compare covariate means for trimmed vs kept treated
matrix q4_trim_chars = J(5,4,.)
local row = 1
foreach var of local X {
    quietly summarize `var' if train==1 & keep_logit==1, meanonly
    matrix q4_trim_chars[`row',1] = r(mean)
    quietly summarize `var' if trimmed_logit==1, meanonly
    matrix q4_trim_chars[`row',2] = r(mean)

    quietly summarize `var' if train==1 & keep_rf==1, meanonly
    matrix q4_trim_chars[`row',3] = r(mean)
    quietly summarize `var' if trimmed_rf==1, meanonly
    matrix q4_trim_chars[`row',4] = r(mean)

    local row = `row' + 1
}

matrix rownames q4_trim_chars = age educ black hisp re74
matrix colnames q4_trim_chars = ///
    KeptTreated_logit TrimmedTreated_logit ///
    KeptTreated_rf TrimmedTreated_rf
matrix list q4_trim_chars

* save
esttab matrix(q4_trim_chars) using "$output/q4_trimmed_treated_characterization.tex", replace tex ///
    title("Kept vs. trimmed treated units") ///
    cells("result(fmt(3))") nomtitles

* store a compact trimming summary
matrix q4_trim_summary = J(2,3,.)
matrix q4_trim_summary[1,1] = cutoff_logit_controls
matrix q4_trim_summary[1,2] = Ntreat_trim_logit
matrix q4_trim_summary[1,3] = Frac_trim_logit

matrix q4_trim_summary[2,1] = cutoff_rf_controls
matrix q4_trim_summary[2,2] = Ntreat_trim_rf
matrix q4_trim_summary[2,3] = Frac_trim_rf

matrix rownames q4_trim_summary = Logistic RF
matrix colnames q4_trim_summary = MaxControlPS NumTreatedTrimmed FracTreatedTrimmed
matrix list q4_trim_summary

esttab matrix(q4_trim_summary) using "$output/q4_trimming_summary.tex", replace tex ///
    title("Trimming summary") ///
    cells("result(fmt(4))") nomtitles
	
/* (a)(4) Now output TABLE_3 comparing results of the full sample and the trimmed
samples based on both propensity score measures. Do this both for the actual
dependent variable re78 and the placebo regressions for variable re75. Always
include the covariate set X as controls. */

/* Regression of re78 using full sample and controls */
regress re78 train age educ black hisp re74, vce(robust)
outreg2 using "$output/table_3.tex", replace se bdec(3) sdec(3) ctitle("re78 full")

/* Regression of re78 using trimmed sample based on logistic regres
sion and controls. */
regress re78 train age educ black hisp re74 if keep_logit == 1, vce(robust)
outreg2 using "$output/table_3.tex", append se bdec(3) sdec(3) ctitle("re78 logit trim")

/* Regression of re78 using trimmed sample based on random forest
and controls. */
regress re78 train age educ black hisp re74 if keep_rf == 1, vce(robust)
outreg2 using "$output/table_3.tex", append se bdec(3) sdec(3) ctitle("re78 RF trim")

/* Regression of re75 using full sample and controls. */
regress re75 train age educ black hisp re74, vce(robust)
outreg2 using "$output/table_3.tex", append se bdec(3) sdec(3) ctitle("re75 full")

/* Regression of re75 using trimmed sample based on logistic regres
sion and controls. */
regress re75 train age educ black hisp re74 if keep_logit == 1, vce(robust)
outreg2 using "$output/table_3.tex", append se bdec(3) sdec(3) ctitle("re75 logit trim")

/* Regression of re75 using trimmed sample based on random forest
and controls. */
regress re75 train age educ black hisp re74 if keep_rf == 1, vce(robust)
outreg2 using "$output/table_3.tex", append se bdec(3) sdec(3) ctitle("re75 RF trim")


/* (5)  Compare the two propensity-score estimators in this application. Your discussion must address: (i) flexibility (nonlinearities/interactions), (ii) tail behavior/calibration and its consequences for trimming, (iii) interpretability and reproducibility, and (iv) how the choice of estimator affects overlap diagnostics and the set of observations discarded. */

/* A: In terms of flexibility, the two propensity-score estimators – the logit model and the random forest – are rather different. The former requires the researcher to impose a linear specification, where any eventual non-linearities or interactions must be modelled explicitly, requiring additional assumptions regarding the relationship between the regressors and the treatment reception. The latter, on the other hand, is a non-parametric method that automatically captures any non-linearities and interactions between the variables. This makes it more flexible and possibly more accurate, if the assignment mechanism is suspected not to be linear in parameters. 

When it comes to the trimming, the graph and the summary statistics regarding the propensity score suggest a greater overlap between the treatment and the control with the logit model with respect to the random forest. This has of course an effect on the trimming and the resulting sample size: while with the first method only 39 observations are excluded, with the second the number of exclusions increases to 120. For the estimation at the tails of the distribution, the logit model may have a better performance at the tails, as it is based on a sigmoidal curve, hence forcing observations into having a smoother density. This is indeed confirmed by our estimation, as observations assigned to the extremes of the distribution of the propensity score are relatively less when compared to the random forest, which does not have a predetermined functional form. On the other hand, the calibration refers to the percentage of individuals that actually receive the treatment, which may be different from the estimated propensity score given their covariates. As the evaluation of the calibration is highly sensitive to the data at hand, without further testing we do not have a strong preference for either method.

Both methods can be considered to be replicable: the logit model can be easily reproduced using the full model specification, while the specification of a random seed for the random forest ensures replicability despite the randomness that characterises this methodology by construction. Nonetheless, the logit model remains more interpretable, since the coefficients have a clear meaning and show explicitly how each of the regressors affects the treatment assignment. On the other hand, the random forest remains a "black box", where the exact way in which every covariate affects the assignment is not specified.

Overall, the differences in the outcomes of the regressions (and therefore the differences in the trimming) are caused by the different way in which the models capture the relationship between the treatment assignment and the variables affecting it. When non-linear relationships and interactions are automatically taken into account (i.e. with the random forest) the effect of the treatment becomes much larger in magnitude, even in the placebo test. This may be influenced by the fact that, with this method, the propensity score for both groups is more polarised, and therefore the subset may include comparatively more treated individuals whose probability of receiving the treatment was lower. If the treatment is more effective for individuals "at the margin", this could have inflated the coefficients in both specifications, and therefore led to the observed difference in estimates.

In addition, the fact that the coefficient of train is significant even in the placebo test suggests that the estimate of the treatment effect may be biased by the self-selection of subjects into the treatment or control group, implying that the identification strategy is not appropriate. The larger magnitude of the coefficient using the random forest may be due to existence of a non-linear relationship between the covariates and being in the treatment group, which may be driving the self-selection process.

*/
	
*=============================================================================
/* 								Question 5 									*/
/* Use the jtrain2 data set.
Read Athey and Imbens (2017) (focus on those sections where the authors discuss how to perform inference in completely randomized experiments; in particular, section 4). */
*=============================================================================

/* (a) Under which conditions, allowing for heterogeneous treatment effects, is Neyman's inference unbiased? */

	/*A: Neyman, in the context of inference from random experiments, proposed as an estimator the difference in average outcomes by treatment status, so for treatment and control groups. Allowing heterogeneous treatment effects, for the estimator proposed by Neyman to be unbiased pure randomisation must hold, so it must be a completely randomized experiment. This holds when there is independence of treatment assignement and potential outcomes. On the other hand, if we are considering the estimation of the standard error, for the estimator to be unbiased under heterogeneous treatment effects, it must be possible to view the sample analyzed as a random sample from an infinite population. */

/* (b) Describe Fisher's inference and replicate section 4.1 of Athey and Imbens (2017) in Stata. Do you arrive at their same p-value? If not, why? Hint: Note that you can draw motivation from third-parties for your own answer; for this case, we suggest that you read Heß (2017).*/ 

	/*A: Fisher's inference is based on testing the sharp null hypothesis, which is the null hypothesis under which we can infer all the missing potential outcomes from the observed ones. A typical choice is the null hypothesis that the treatment has no effect. The alternative hypothesis is that there exists at least one unit such that this does not hold. This type of inference, also called Fishearian Randomization Inference, produces a distribution of a test statistic under a null hypothesis, and it helps the researcher understand if the observed value of the statistic is "extreme", and so it helps understand whether the null hypothesis must be rejected. Fisher's inference makes it possible to infer, for any statistic that is a function of the Y^obs (observed outcomes), W (treatment) and X (covariates), the exact distribution of that statistic under the null hypothesis.*/

* (b) 

use "https://github.com/sbernardoni/microeconometrics-ps/blob/06b798693174efb8e85c8f805ac242c8fe9d2302/ps1/ps1_data/jtrain2.dta", clear

*calculating the simple difference in means
*seed set at 20295 to mantain coherence

*default of 100 permutations
ritest train _b[train], seed(20295): ///
	reg re78 train 

*running same test with 1000 permutations
ritest train _b[train], reps(1000) seed(20295): ///
	reg re78 train 

*running the same test with 10000 permutations
ritest train _b[train], reps(10000) seed(20295): ///
    reg re78 train 

	/* A: We followed the approach of Heß (2016) and we replicated section 4.1 from Athey and Imbens (2017). We conducted the resampling with 100 (default) iterations, 1000 and 10000 iterations. With 100 iterations, the p-value is approximately zero. With 1000 iterations, the p-value varies between 0.0030 and 0.0070. With the last specification, with 10000 resampling replications the p-value is 0.0039, which is slightly smaller than the one found by Athey and Imbens (2017). The difference is to be expected, because of the random nature of the permutation sampling.*/

/* (c) Read again the randomization plan in LaLonde (1986). On which grounds Athey and Imbens (2017)'s illustration of Fisherian inference on LaLonde (1986)'s paper could be criticized? */

	/* A: The main critique that could be moved against Athey and Imbens (2017) illustration of Lalonde (1986)'s paper is how randomization was carried out in the original experiment versus how it was reproduced in the Athey and Imbens paper. In particular, the treatment in the data analyzed by Lalonde was given out by 10 different sites of the project, while in the Athey and Imbens (2017) Fisherian inference illustration, the data is treated as if the treatment was randomly assigned across the sample, without the intervention of the single sites. An important assumption when conducting Fisherian inference is that all treatment assignments are equally likely, so if site-specific factors influence outcomes or the assignment process, this assumption might not hold, and for this reason it could lead to incorrect p-values, and so to incorrect conclusions about statistical significance. */

/* (d) The article Channeling Fisher: Randomization Tests and the Statistical Insignificance of Seemingly Significant Experimental Results (Young, 2019) presents the results of an exercise to test the null hypothesis of no treatment effects in a series of experimental papers recently published in AEA journals, showing that many of the coefficients reported in those papers are no longer significant in a randomization test. A critique of this paper has been published by professors Uri Johnson, Leif Nelson and Joe Simmons in their blog, Data Colada. Read their post here and answer the questions below. */

	/* (i) Briefly explain the difference between the procedure used as the default in Stata for the calculation of standard errors (HC1) and the one proposed by the Data Colada post (HC3). */
	
		/* A: In HC1 Robust Standard Errors, the diagonal elementes of the variance-covariance matrix are substitued with Robust Standard error, based on non-constant variance, which are the squared residuals, weighted by the following coefficient n/(n-k). HC1 robust standard errors are the default in Stata. 

HC3 Robust Standard Errors, on the other hand, are widely used and considered as the best standard errors when heteroskedasticity is present. The diagonal elements of the variance-covariance matrix are replaced by the squared residuals divided by (1-h)^2, h being the hat values that range from 0 to 1. */
	
	/* (ii) Using the dataset jtrain2, rerun the analysis you have performed in exercise 1, now calculating the standard errors based on HC3 (this is done in Stata using the option vce() in your regression command). */

*First regression
regress re78 train, vce(hc3)

*second regression
regress re78 train age educ black hisp, vce(hc3) 

*third regression
regress re78 train age educ black hisp re74 re75, vce(hc3)



	/* (iii) Perform a third version of your analysis, now based on bootstrapping (use the bootstrap command in Stata). Briefly describe how the standard errors are calculated in this approach. */

*first regression
bootstrap _b, reps(1000): regress re78 train
*second regression
bootstrap _b, reps(1000): regress re78 train age educ black hisp
*third regression
bootstrap _b, reps(1000): regress re78 train age educ black hisp re74 re75

		/* A: Bootstrapping is a non-parametric statistical method that uses random sampling with replacement to determine the sampling variation of an estimate. In particular, standard errors in a bootstrap procedure are calculated by resampling the data multiple times (the standard on stata is 50 times) , recalculating the statistic of interest for each resample, and finally computing the standard deviation of the replications. The standard deviation of the bootstrap replications is the bootsrap standard error */
	
	/* (iv) Do any of your conclusions regarding the effect of the training program change based on the analysis performed in this exercise? Based on the discussion provided in the Data Colada post, can you think of a reason for why your results using HC3 should or shouldn't change for this exercise? 

		/* A: The regressions performed in this exercise yield the same results as the regressions performed in exercise 1. In particular, the coefficients for all three specifications (non-robust standard errors, HC3 and bootstrapping) are substantially the same, and the only difference between the results of the two analyses are the standard errors, even though the variation is small and does not change the significance of the results found. HC3 standard errors are slightly higher, in line with theoretical expectations.
		
		The fact that coefficients remain consistent across specifications, with only slight change in the confidence intervals, is an indicator of the robustness of the analysis performed. 
		
		Based on the discussion in the Data Colada post, it was to be expected that the results do not change, since the sample size is larger than 250 observations, and we know that HC3 performs much better than the HC1 default standard error option when the sample size is small. 
		
		Finally, our conclusion regarding the effect of the training program did not change based on the analysis performed in this exercise. */
	