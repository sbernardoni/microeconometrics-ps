*=============================================================================

/*						20295 MICROECONOMETRICS							   	*/

/*							Problem Set 3								   	*/

*=============================================================================

/* Group number: 5 */

/* Group composition: Sara Bernardoni, Gabriele Molè, Florens Schlosser */
*=============================================================================

*=============================================================================
/* 								Setup 										*/
*=============================================================================

clear

set more off

/* First time running this code? Please remove the comment marks from the code below and install all of the necessary packages. */

/* Analysis packages */
/*
ssc install outreg2, replace
ssc install rdrobust, replace
ssc install estout, replace
ssc install rddensity, replace
ssc install lpdensity, replace 
*/

/* Graphics packages */
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
    global filepath "C:\Users\Sara\Documents\ESS\20295 - Microeconometrics\microeconometrics-ps\ps3"
	global output "C:\Users\Sara\Documents\ESS\20295 - Microeconometrics\microeconometrics-ps\ps3\ps3_output"
}

if ("`user'" == "flore") {
    global filepath "C:\Users\flore\OneDrive\Documents\Bocconi\Year 2\Microeconometrics\PS 3\files"
	global output "\Users\flore\OneDrive\Documents\Bocconi\Year 2\Microeconometrics\PS 3\files\outputs"
}


if ("`user'" == "gabrielemole") {
    global filepath "C:\Users\Stealth\Desktop\microeconometrics-ps\ps3"
	global output "C:\Users\Stealth\Desktop\microeconometrics-ps\ps3\ps3_output"
}

*==============================================*
/*                   Setup                    */
*==============================================*

	/* This problem set is composed of two exercises, each exercise focusing on a different regression discontinuity design (RDD). In Exercise 1, we follow a standard RDD application, Meyersson (2014), to study the effect that Islamic political representation had on the educational attainment of women in Turkey during the late 1990s. In Exercise 2, we turn to a spatial RDD, Gonzalez (2021), to study the effect of cell phone coverage on electoral frauds. */
	
/*								Commands									*/

	/* Regression discontinuity designs are implementable with packages such as rdrobust, rddensity and lpdensity, among others, in both R and Stata. You should install these before proceeding. */
	
/*								Instructions								*/

	/* (1) rdrobust reports estimates from different estimation methods: (1) Conventional, (2) Bias-corrected, and (3) Robust. In this problem set, any rdrobust output should be reported with Conventional betas and standard errors. Nonetheless, note that in your own research it is recommended that you report Conventional betas and Robust standard errors */
	
	/* (2) Unless asked otherwise, use as default options for your rdrobust estimates:
		
		-> kernel(triangular) p(1) bwselect(mserd)
		
		*/
	
	/* (3) Have in mind that some commands have different default procedures in Stata and R. Since we are not asking you to specify some of these procedures, it is normal that sometimes the results are not exaclty the same between the two languages. */

*============================================*
*  				Exercise 1					*/
/*        Use the file pset_3.dta           */
*============================================*

use "https://github.com/sbernardoni/microeconometrics-ps/raw/refs/heads/main/ps3/ps3_data/pset_3.dta", clear

* Question (a) * 

	/* (i) Generate a RD Plot of ``T - Islamic mayor in 1994'' - against ``X - Islamic Vote Margin in 1994'' - when the Islamic party wins and lose an election. 
	Call the y-axis - ``Treatment Variable'' ; call the x-axis - ``Running variable'' */
	
rdplot T X, graph_options(title(RD Plot) ytitle("Treament Variable") xtitle("Running Variable") legend(off)) graph rename T_X replace
graph export "$output/disc_plot.png", replace
	
	/* (ii) Is the current design a sharp or a fuzzy RD? Why? */
	
		/* Our RD design is sharp by nature. A RDD is sharp if treatment changes deterministically at a cutoff, and fuzzy when the cutoff only induces a probabilistic change in treatment. As the mayoral elections are deterministically won by an Islamic party if the running variable is nonnegative, our RDD must be sharp. The graph reflects this with a clear jump from non-treatment (no Islamic governing party) to treatment (Islamic governing party) at zero. */

* Question (b)

	/* (i) Create a macro named `covariates' containing the baseline variables: ``hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk''. Create a table named ``Table_1'', summarizing RD estimates for all baseline variables. Table_1 should have the following columns: Label, MSE-Optimal Bandwidth, RD Estimator, p-value, and Effective Number of Observations */

local covariates hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk

local num: list sizeof covariates
mat balance = J(`num',4,.)
mat list balance 
local row = 1
foreach z in `covariates' {
    qui rdrobust `z' X
	*bandwidth (average of left and right)
	mat balance[`row',1] = round((e(h_l) + e(h_r))/2, .001) 
	*estimated jump at cutoff
	mat balance[`row',2] = round(e(tau_cl), .001)
	*p-value
	mat balance[`row',3] = round(e(pv_cl), .001)
	*effective number of observations
	mat balance[`row',4] = round(e(N_h_l) + e(N_h_r), .001)
	local ++row
}
mat rownames balance = "Share Men High School Education" "Islamic Mayor in 1989" "Islamic vote share 1994" "N parties receiving votes 1994" " Log Population in 1994" " District center" " Province center" "Sub-metro center" "Metro center"
mat colnames balance = "MSE-Optimal Bandwidth (avg)" "RD Estimator" "p-value" "Effective Number of Observations"
mat list balance 

putexcel set "$output/table1.csv", replace
putexcel A1=matrix(balance), names nformat(number_d2)
putexcel (A2:A10), overwr bold border(right thick) 
putexcel (B1:E1), overwr bold border(bottom thick)

esttab matrix(balance) using "$output/table1.csv", replace

		/* Note: None of the RD estimates are significant, hence we can assume that the covariates are balanced along the cutoff. This implies that treatment assignment around the cutoff is "as good as" random, and our RDD approach is valid. */
	
* Question (c) *

	/* (i) Generate a RD plot for each of the baseline variables on `covariates`. */
	
local covariates hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk

local label1 "Men High School Education (1989)"
local label2 "Islamic Mayor in 1989"
local label3 "Islamic Vote Share (1994)"
local label4 "Parties Receiving Votes (1994)"
local label5 "Log of Population (1994)"
local label6 "District Center Dummy"
local label7 "Province Center Dummy"
local label8 "Sub-Metro Center Dummy"
local label9 "Metro Center Dummy"

local i = 1
foreach var of local covariates {
    local title "`label`i''"
    rdplot `var' X, graph_options(title("`title'") ytitle("Treament Variable", size(3)) xtitle("Running Variable", size(3)) legend(off) name(`var'_X, replace))
    local ++i
}
	
	/* (ii) Use `graph combine` to generate a unique graphic containing all 9 RD plots. */
	
graph combine hischshr1520m_X i89_X vshr_islam1994_X partycount_X lpop1994_X merkezi_X merkezp_X subbuyuk_X buyuk_X, cols(3)
	
	/* (iii) Title each RD subplot so that the reader is able to identify each subplot to the corresponding outcome. Save the unique graphic as `Graph 1`. */
	
graph export "$output/Graph_1.png", replace
	
* Question (d) *

	/* (i) Generate a graphic with histograms for the observations to the left and the observations to the right of our cutoff. Choose contrasting colors for the histograms on each side of our cutoff. */

twoway ///
    (histogram X if X < 0, color(navy%70)) ///
    (histogram X if X >= 0, color(orange%70)), ///
    xline(0, lcolor(black) lpattern(solid)) ///
    graphregion(color(white)) ///
	title("Vote margin distribution below/above cutoff") ///
    xtitle("Islamic Vote Margin") ///
    ytitle("Density") ///
    legend(order(1 "X < 0" 2 "X >= 0")) ///
    name(hist_1, replace)
	
	/* (ii) Use `rddensity` to generate a graphic of our running variable X's estimated density. In both graphics, plot a vertical line to signal our cutoff. */
	
rddensity X, plot ///
    graph_opt(name(hist_2, replace) ///
    legend(off) ///
    xline(0, lcolor(black) lpattern(solid)) ///
	title("Estimated density of vote margins") ///
    xtitle("Islamic Vote Margin") ///
    ytitle("Density"))
	
	/* (iii) Save a graphic named `Graph_2` containing the histogram plot and the estimated density plot side-by-side. */
	
graph combine hist_1 hist_2, cols(2) ///
    graphregion(color(white)) ///
    name(graph2, replace)
graph export "$output/Graph_2.png", replace
	
* Question (e) *

	/* (i) Use `rddensity` to test if a discontinuity in our running variable X's density does not exist in our cutoff. */
	
rddensity X , all plot
graph export "$output/Graph_3.png", replace
	
	/* (ii) What are we able to conclude from such test? Is it favorable or against the validity of our RD design? */
	
		/* For a valid RDD design we require that the running variable is continuously distributed around the cutoff. This is, for example, not the case if  individuals around the cutoff are able to manipulate the running variable. In such situations, the  distribution of individuals around the cutoff is no longer "as-good-as-random", hence the units below the cutoff are no longer a valid counterfactual for the units above the cutoff and our RDD fails. The rddensity command gives us a test for (H_1) the existence of a discontinuity in the density of the running variable (in our case the islamic vote margin) at the cutoff.  

		We reject the null hypothesis based on the "conventional" test statistic, which yields a p-value of 0.0145, at 5% confidence level. We therefore have to assume that the running variable is not continuously distributed around the cutoff, which may pose a serious threat to the validity of a RD approach. This threat is mitigated by the fact that a robust version of the test yields a p-value of 0.1634, suggesting that after removal of potential bias, we have no indication of disconinuity.

		We also note that the p-values of the test differ considerably based on the window used for fitting the polynomial to the density of X. We do not identify significant discontinuities for small windows (~<= 3.5), for large windows however, p-values are much  smaller. */
	
* Question (f) *

	/* (i) Test if alternative discontinuities do not exist in the following alternative cutoffs:

		-10, -5, 5, 10. */
		
*** Baseline Test
rdrobust Y X, c(-10)
display "Conventional p-value: " e(pv_cl)
display "Robust p-value: " e(pv_rb) 
*conv. p-value: 0.003, robust: 0.006
rdrobust Y X, c(-5) 
display "Conventional p-value: " e(pv_cl)
display "Robust p-value: " e(pv_rb) 
*conv. p-value: 0.246 , robust: 0.268
rdrobust Y X, c(5) 
display "Conventional p-value: " e(pv_cl)
display "Robust p-value: " e(pv_rb) 
*conv. p-value: 0.624 , robust: 0.472
rdrobust Y X, c(10) 
display "Conventional p-value: " e(pv_cl)
display "Robust p-value: " e(pv_rb) 
*conv. p-value: 0.193 , robust: 0.162
	
	/* (ii) Did we find any evidence in favor of the absence of alternative discontinuities? */
		
		/* There is no evidence for discontinuities at the cutoffs -5, +5, +10. 
		
		At the cutoff -10 our test identifies a statistically significant jump in outcomes, which violates the fourth requirement for the validity of our RDD, namely that discontinuities do not exist away from the cutoff. While this can arise when checking for arbitrary discontinuities, we need to keep it in mind when discussing the validity of our results. */
	
/* After validating our RD design, we can estimate our treatment's effect on our outcomes and check for the robustness of our results. That is what we will do in the following questions. */
	
**# Question (g)

	/* (i) Generate a RD Plot of ``Y - Share Women aged 15-20 with High School Education'' - against ``X - Islamic Vote Margin in 1994'' - when the Islamic party wins and loose an election. 
	
		Use 40 Evenly-Spaced Bins.
		
		Call the y-axis - `Outcome; call the x-axis - `Running Variable' */
		
rdplot Y X, nbins(20 20) binselect(es) graph_options(title("RD plot") ytitle(Outcome) xtitle(Running Variable))
*note that c(0) is the default

graph export "$output/RD_Plot.pdf", replace

	
**# Question (h) 

	/* (i) Use rdrobust to estimate the effect of ``T - Islamic mayor in 1994'' - on ``Y - Share Women aged 15-20 with High School Education'' using a linear polynomial. Try both an uniform and triangular kernel. Does electing a mayor from an Islamic party has a significant effect on the educational attainment of women? Do results differ significantly for different kernel choices? */
	
*uniform
rdrobust Y X, p(1) kernel(uni) bwselect(mserd)
outreg2 using table_1.tex, append se bdec(3) sdec(3) ///
ctitle("Uniform Kernel")

*triangular
rdrobust Y X, p(1) kernel(tri) bwselect(mserd)
outreg2 using table_1.tex, replace se bdec(3) sdec(3) ///
ctitle("Triangular Kernel")


/* A: We find a positive and statistically significant effect of the election of a muslim party on the share of women with high school education. 
The estimated Treatment Effect at the cutoff is 3.2019 percentage points for the uniform kernel and 3.0195 percentage points for the triangular kernel. Both estimates are significant at the 5% level using conventional standard errors (as requested in the guidelines of the problem set). It is worth mentioning that using robust standard errors the p-value for the triangular kernel increases to 0.076, hence granting significance only at the 10% level. 
 The uniform kernel gives equal weights to the observations in the bandwith, while the triangular one gives linearly less weight as observations get further from the cutoff. The results are yet fairly comparable between different approaches.  */
	
/* MANDATORY: Use a triangular kernel for these next items. */
	
**# Question (i) Estimate the effect of T on Y but using a global approach.

	/* (i) Do not choose any bandwidth. Use a polynomial of order 4. */
	
	/* (ii) Run a regular linear regression instead of rdrobust. */
	
*as the cutoff is 0 then X-c = X
gen X2 = X^2
gen X3 = X^3
gen X4 = X^4

reg Y T X X2 X3 X4 i.T#c.X i.T#c.X2 i.T#c.X3 i.T#c.X4
outreg2 using table_1.tex, append se bdec(3) sdec(3) ///
ctitle("Unweighted Global Regression")

*Triangular weights 
gen wght= .
sum X, d
scalar min = r(min)
scalar max = r(max)
replace wght = (1-abs(X/min)) if X<0
replace wght = (1-abs(X/max)) if X>=0

reg Y T X X2 X3 X4 i.T#c.X i.T#c.X2 i.T#c.X3 i.T#c.X4 [aw = wght]
outreg2 using table_1.tex, append se bdec(3) sdec(3) ///
ctitle("Triangular Global Regression")

	
**# Question (j) Estimate the effect of T on Y but using a local approach by restricting our sample to a window within an optimal bandwidth that we should have obtained with rdrobust (mserd bandwidth).

	/* (i) Run a regular linear regression. Use a linear polynomial. */
	
	/* (ii) Do we get the exact same result as in item (h)? If not, explain why. 
	
		HINT: In the `rdrobust` post-estimate, save our optimal bandwidth in a local using:

			`local opt i = e(h l)` */
			
preserve
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
local opt_i e(h_l)
display `opt_i'
drop if X>`opt_i' | X <-`opt_i'
reg Y T X i.T#c.X
*Save the files
outreg2 using table_1.tex, append se bdec(3) sdec(3) ///
ctitle("Unweighted Local Regression")
restore

*triangular weights
preserve
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
local opt_i = e(h_l)
display `opt_i'
drop if X>`opt_i' | X <-`opt_i'
gen whgt2 = .
replace whgt2 = (1 - abs(X/`opt_i')) if X < 0 & X >= -`opt_i'
replace whgt2 = (1 - abs(X/`opt_i')) if X >= 0 & X <= `opt_i'

reg Y T X i.T#c.X [aw = whgt2]
outreg2 using table_1.tex, append se bdec(3) sdec(3) ///
ctitle("Triangular Local Regression")
restore

/* A: The global unweighted regression yields a point estimate of 3.683 (statistically significant at the 5% level). This is quite different from the one found in point h). If we adopt triangular weights in the regression, the estimates returns a value of 3.028359 (statistically significant at the 5% level), comparable to the one in h) but probably still a bit noisy due to the large number of observations far from the cutoff used and the high-order polynomial that might be overfitting. An alternative strategy is then restricting the sample to a bandwith close to the cutoff and using linear first order polynomials. 

Using the uweighted local regression, the estimated treatment effect at the cutoff is 3.06. Results are not the same in h) yet are not dramatically different. The slight disrepancy might be due to the different weights given to the observations. The triangular kernel used in h) gives less weight to observation far from the cutoff. These are equally weighted in the regression, hence capturing some noise the kernel was cancelling out. Indeed if we estimate the regression in j) using triangular kernels the estimated treatment effect at the cutoff is virtually identical to h). */

	
**# Question (k) Save item (h)'s bandwidth as a scalar named opt i.

	/* (i) Re-estimate item (h)'s RD using as alternative bandwidths:
	
		`0.5*opt i, 0.75*opt i, 1.25*opt i, and 1.5*opt i`*/

rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
local opt_i = e(h_l)
estimates store reg_band_3
rdrobust Y X, p(1) kernel(triangular) h(0.5*`opt_i' 0.5*`opt_i')
estimates store reg_band_1
rdrobust Y X, p(1) kernel(triangular) h(0.75*`opt_i' 0.75*`opt_i')
estimates store reg_band_2
rdrobust Y X, p(1) kernel(triangular) h(1.25*`opt_i' 1.25*`opt_i')
estimates store reg_band_4
rdrobust Y X, p(1) kernel(triangular) h(1.5*`opt_i' 1.5*`opt_i')
estimates store reg_band_5
	
	/* (ii) Plot each five RD point estimates, including that from item (h), with their respective confidence intervals in a graphic named Graph 3. */
	
coefplot ///
    (reg_band_1, label("Bandwidth 0.5") msymbol(O) mcolor(navy)) ///
    (reg_band_2, label("Bandwidth 0.75") msymbol(D) mcolor(maroon)) ///
    (reg_band_3, label("Bandwidth 1.0") msymbol(S) mcolor(forest_green)) ///
    (reg_band_4, label("Bandwidth 1.25") msymbol(T) mcolor(orange_red)) ///
    (reg_band_5, label("Bandwidth 1.5") msymbol(H) mcolor(magenta)) ///
, ///
    title("Coefficient Estimates Across Bandwidths") ///
    ytitle("Coefficient Estimate") ///
    xtitle("Variable") ///
    msiz(medium) ///
	xlabel(, grid) ///
    ciopts(recast(rcap) color(gs8)) ///
    scheme(s1color) ///
    legend(on)
	
graph export "$output/Graph_3.pdf", replace

	/* (iii) What can we say about the robustness of our results with respect to bandwidth choice? */
	
		/* A: Relying on various intervals for the bandwith shows the bias-variance trade off in the estimation of the local average treatment effect. 
The graph shows the point estimates coming from the adoption of different bandwiths and the 95% level confidence intervals. If we keep a very small bandwith by taking 0.5*opt_i the estimates is likely to be less biased but it is highly volatile. Indeed, the coefficient is not statistically significant, with wide standard errors. The variance diminishes as we increase the width of the bandwith reaching statistical significance when using the values of opt_i. The coefficient slightly increases (from 1.8 to 3.02) compared to the 0.5*opt_i interval, showing that the cost of smaller variance comes with an estimate that is likely to be marginally biased. Increasing the bandwith does not come with a great increase in variance while yielding point estimates comparable to the baseline case of opt_i */


*=============================================================================
**#								Exercise 2 									*/

/* Assume Gonzalez (2021) did not have the exact longitude of each voting center in his sample, only a proxy. Instead, latitude was correctly measured. Endowed with the latitude and the proxy for longitude of each polling center, Gonzalez (2021) went on and measured the distance between each polling center "location" and the closest point with 2G coverage. In addition, Gonzalez (2021) has a coverage indicator for each polling center that has been collected by ECC officials.

Both variables can be found in fraud_pcenter_final. The distance between the polling centers and their closest points with 2G coverage is titled " dist"; the cell phone coverage indicator is titled "cov". */
*=============================================================================

use "https://github.com/sbernardoni/microeconometrics-ps/raw/refs/heads/main/ps3/ps3_data/fraud_pcenter_final.dta", clear

* Generating the variables required to carry out the analysis

gen runvar = cond(cov==1, _dist, -_dist)
label variable runvar "Signed distance to boundary (neg=outside, pos=inside)"

gen D = runvar>=0 
label variable D "Indicator: inside coverage"

gen fraud1 = (frnum_comb>0)
label variable fraud1 "1 if ≥1 Category C station"

gen outcome_a = vote_comb_ind
label variable outcome_a "at least one station with category C fraud"

gen outcome_b = vote_comb
label variable outcome_b "share of votes with category C fraud"

**# Question (a)

	/* (i) Plot the treatment variable used at Gonzalez (2021) as a function of this new running variable. In addition, compute the RD estimate for a regression where you model the 
	same treatment variable as a function of the new running variable. */
	
twoway ///
  (lpolyci cov runvar if runvar<0, bwidth(2) lcolor(blue)      ) ///
  (lpolyci cov runvar if runvar>=0, bwidth(2) lcolor(red)     ) ///
  (scatter cov runvar,     ///
      msymbol(circle) msize(vsmall) mcolor(gs14%40) jitter(0.002)) ///
  , xline(0, lpattern(dash) lwidth(thin)) ///
    xtitle("Signed distance (km)", size(medium)) ///
    ytitle("Pr(Coverage = 1)", size(medium)) ///
    title("First-Stage Coverage Probability", size(large)) ///
    scheme(s1color) ///
    graphregion(color(white)) bgcolor(white)
	
	/* rdplot T X if X>-20 & X<20 + opzioni grafico*/
	
graph save "$output/g_covprob.gph", replace
graph export "$output/first_stage_coverage.pdf", as(pdf) replace


	*-----Panel A------------------*
	
rdrobust outcome_a runvar, p(1) kernel(triangular) bwselect(mserd)

foreach v in elevation slope {
	local fname = "`v'_rdplot"
	
	rdplot outcome_a runvar if runvar> -20 & runvar< 20, p(4) kernel(triangular) bwselect(mserd) ///
		title("rdrobust / rdplot with optimal bandwidth") ///
		name(`fname', replace)
	
	graph save "$output/`fname'.gph", replace
    graph export "$output/`fname'.pdf", as(pdf) replace
	
	reg `v' outcome_a runvar if abs(runvar)<=5, vce(cluster province_id)
	di as txt "`v' jump = " as res %6.3f
}

	*-----Panel B------------------*
	
rdrobust outcome_b runvar, p(1) kernel(triangular) bwselect(mserd)

foreach v in elevation slope {
	local fname = "`v'_rdplot"
	
	rdplot outcome_b runvar if runvar> -20 & runvar< 20, p(4) kernel(triangular) bwselect(mserd) ///
		title("rdrobust / rdplot with optimal bandwidth") ///
		name(`fname', replace)
	
	graph save "$output/`fname'.gph", replace
    graph export "$output/`fname'.pdf", as(pdf) replace
	
	reg `v' D runvar if abs(runvar)<=5, vce(cluster province_id)
	di as txt "`v' jump = " as res %6.3f _b[D]
}

* Density (McCrary) test
rddensity runvar, c(0)
	
	/* (ii) Is the current design a sharp or a fuzzy RD? */
	
		/* A: In the original study by González (2021), the treatment was modelled as if determined exactly by the location with respect to the mobile-coverage frontier: a polling centre that fell inside the coverage area is defined as "treated", while one that fell outside as "untreated". This approach is what allowed him to implement a sharp regression discountinuity design to estimate the effect of mobile coverage on the outcome.

In this situation, however, we are dealing with one coordinate (the longitude) that is measured through a proxy, implying that the computation of the distance from the mobile-coverage frontier is prone to measurement error due to the additional noise. This has an effect on the determination of the treatment status, as some places with no coverage may be indicated to be a positive distance, while others that do enjoy mobile coverage may present a negative distance. The threshold of zero proxy distance, therefore, does not imply a clear change in treatment status from zero to one, but simply indicates a sudden jump in the probability of treatment, which remains strictly less that one at that point. This suggests that the current approach, instead, deals with a fuzzy regression discontinuity design. The position with respect to the new frontier becomes an instrument (strong albeit imperfect) for the actual mobile coverage, rather than the treatment itself, and the causal parameter of interest becomes the local Wald ratio estimated in a narrow bandwidth around the threshold.

Our identification still requires that potential outcomes are smooth with respect to the true but unobserved distance, with the addition that the measurement error for the longitude must be random, unrelated to the outcome variable, and must satisfy the monotonicity assumption, meaning that the error remains negligible enough that crossing the threshold does not make a centre less likely to receive coverage. In this context, the causal effect of mobile coverage for centres whose treatment status is actually affected by being measured to be just inside or outside the threshold is obtained by estimating two local linear regressions (one with the fraud variable and another with the treatment indicator as functions of the proxy distance and latitude, including boundary-segment fixed effects) and then computing the ratio of the two discontinuities obtained. */

	
	/* (iii) Which assumptions must hold in order for the one-dimensional RD estimates of Gonzalez (2021) to be valid? */
	
		/*A: The assumptions required for the validity of the one-dimensional RD estimates are related to the RD framework per se, as well as to the specific geographic setting under analysis.

First of all, the running variable cannot be strategically manipulated to fall before or after the threshold, meaning that polling stations or their villages cannot be deliberately located just inside or outside the coverage in view of future elections. This is supported by a recent Cattaneo-Jansson-Ma density test, which has shown no bunching of observations on either side of the threshold of zero distance.

Secondly, the potential outcomes must vary smoothly with location, such that any jump at the threshold must be attributable only to the treatment. For this purpose, the author replicates the discontinuity test using several electoral, demographic, development and topographic covariates in lieu of the main outcome variable, showing that – after narrowing the sample to polling centres lying near the frontier – most differences remain small and statistically insignificant as the bandwidth decreases, unlike for the fraud measure.

Thirdly, since the geographical environment is rugged – which may affect phone coverage – the smooth functions used to partial out latitude and longitude must be flexible enough within the selected bandwidth. Hence, Gonzalez uses the Calonico-Cattaneo-Titiunik bandwidth selector, and estimates separate low-order polynomials on each side of the cutoff, showing that different windows or polynomial orders do not affect significantly the treatment coefficient estimate.

Finally, the comparisons to estimate the treatment effect must be local, and for this purpose the author only includes sections of the frontier for which there exist observations for both sides, discarding the rest. This ensures that for every segment there it is actually possible to compare the treated and control units.

If these assumptions hold, then the observed discontinuity in fraud can be interpreted as the effect of mobile coverage, which enables voters to report irregularities in real time. */




**# Question (b)

	/* (i) Point out in which setting does having a proxy for longitude does not require you to change RD design (relative to Gonzalez, 2021). HINT: Read the "Additional Results" section of Gonzalez (2021) and reflect on which type of cell phone coverage boundary would deliver you this result. */
	
		/* A: In this setting, having a proxy for the longitude would not require us to change the regression discountinuity design only if the treatment frontier were not to depend on it – that is, if it were a horizontal line from east to west.

In this case, the distance from the boundary of the polling centre can be computed as ``runvar = latitude – φ0'' where φ0 is the latitude of the coverage edge. Since the longitude never enters the equation, any possible measurement error cannot affect the classification of a centre as "treated" or "untreated". Therefore, the first-stage discontinuity in the probability of treatment would still jump from 0 to 1, as the indicator `D = 1(runvar ≥ 0)' remains a deterministic function of the running variable.

This is indeed illustrated by Gonzalez' test with placebo boundaries defined by randomly chosen latitudes, where the map is divided into horizontal sections. In practice, however, as the actual Afghan 2G footprint changes both in latitude and longitude, the imprecision in the estimation of the longitude does affect the change in probability due to the threshold (as the coverage becomes probabilistic instead of deterministic), implying that the right specification remains the fuzzy regression discontinuity design. */

**# Question (c)

	/* (i) Use fraud pcenter final to partially replicate Columns 1, 3 and 5 of Table 2 under this new RD setting (present only point estimates). Interpret your new estimates. HINT: use ``Table_onedim_results.do'' and review your RDD slides. */

* Estimating the optimal bandwidth

foreach var in 600 95 ecc comb comb_ind {
		rdbwselect vote_`var' runvar if ind_seg50==1, vce(cluster segment50)
		scalar hopt_`var'=e(h_mserd)
		forvalues r=1/2 {
			rdbwselect vote_`var' runvar if ind_seg50==1 & region2==`r', vce(cluster segment50)
			scalar hopt_`var'_`r'=e(h_mserd)
	}
}

xtset, clear
xtset segment50 pccode

*Local Linear Regression 

gen T=cov	
label variable T "Coverage dummy"
gen instrument_T=0
replace instrument_T=1 if runvar>0
gen interaction=T*runvar
label variable interaction "Interaction between coverage dummy and outcome variable"
gen instrument_interaction=runvar*instrument_T
 
*** Only using the treatment instrument 

foreach var in comb_ind comb {	
	* All regions
	xtivreg vote_`var' (T = instrument_T) runvar if ind_seg50==1 & _dist<=hopt_`var', fe  vce(robust)
		est store col1_a1_`var'
		

	* Southeast
	xtivreg vote_`var' (T = instrument_T) runvar if ind_seg50==1 & _dist<=hopt_`var'_1 & region2==1, fe vce(robust)  
		est store col1_b1_`var'
		

	* Northwest
	xtivreg vote_`var' (T = instrument_T) runvar if ind_seg50==1 & _dist<=hopt_`var'_2 & region2==2, fe vce(robust)
		est store col1_c1_`var'
	
 }



foreach var in comb_ind comb {	
	* All regions
	xtivreg vote_`var' (T interaction = instrument_T instrument_interaction) runvar if ind_seg50==1 & _dist<=hopt_`var', fe  vce(robust)
		est store col1_a2_`var'
		
	* Southeast
	xtivreg vote_`var' (T interaction = instrument_T instrument_interaction) runvar if ind_seg50==1 & _dist<=hopt_`var'_1 & region2==1, fe vce(robust)  
		est store col1_b2_`var'
		
	* Northwest
	xtivreg vote_`var' (T interaction = instrument_T instrument_interaction) runvar if ind_seg50==1 & _dist<=hopt_`var'_2 & region2==2, fe vce(robust)
		est store col1_c2_`var'
	
 }


* Panel A
estout col1_a1_comb_ind  col1_a2_comb_ind  col1_b1_comb_ind  col1_b2_comb_ind col1_c1_comb_ind  col1_c2_comb_ind   ///
using "$output/Table_2.tex", replace style(tex) ///
keep(T) label cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
mlabels(, none) collabels(, none) eqlabels(, none) ///
stats(N, fmt(a3) ///
labels("Observations")) ///
prehead("\begin{table}[H]" "\centering" "\begin{tabular}{lcccccc}" ///
	"\noalign{\smallskip} \hline \hline \noalign{\smallskip}" ///
	"& \multicolumn{6}{c} {RDD - Optimal Bandwidth}"   ///
	"\noalign{\smallskip} \\ " ///
	"& \multicolumn{2}{c} {All regions} & \multicolumn{2}{c} {SE region} & \multicolumn{2}{c} {NW region}\\" ///
	" & (1) & (2) & (3) & (4) & (5) & (6) \\" ) ///
	posthead("\hline \noalign{\smallskip}" "\multicolumn{6}{l}{\emph{Panel A. At least one station with Category C fraud}} \\" "\noalign{\smallskip} \noalign{\smallskip}" ) ///
	prefoot("\noallign{\smallskip}" "Interaction & & \checkmark & & \checkmark & & \checkmark \\")

* Panel B
estout col1_a1_comb  col1_a2_comb  col1_b1_comb  col1_b2_comb col1_c1_comb  col1_c2_comb  ///
using "$output/Table_2.tex", append style(tex) ///
posthead("\noalign{\smallskip} \noalign{\smallskip} \noalign{\smallskip}" "\multicolumn{6}{l}{\emph{Panel B.  Share of votes under Category C fraud}} \\" "\noalign{\smallskip} \noalign{\smallskip}" ) ///
keep(T) label cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
mlabels(, none) collabels(, none) eqlabels(, none) ///
stats(N, fmt(a3) ///
labels("Observations")) ///
prefoot("\noallign{\smallskip}" "Interaction & & \checkmark & & \checkmark & & \checkmark \\") ///
postfoot("\noalign{\smallskip} \hline \hline \noalign{\smallskip}" ///
	"\end{tabular} \end{table}")

	/* A: After changing the estimation strategy, both the point estimates and their significance levels change: the point estimates generally become lower and less significant compared to those reported in Table 2 of the paper. Indeed, the estimated effect on having at least one station with category C fraud (panel A) is negative but not significant (-11.1 percentage points), while the effect on the share of votes under category C fraud (panel B) remains negative and statistically significant (-10.1 percentage points). Similarly to Table 2 in Gonzalez (2021), we also find heterogeneous effects: in panel A, while regions in the South-East present a negative and significant effect (-29 percentage points), those in the North-West yield positive yet not statistically significant point estimates (+2.5 percentage points). Likewise, in panel B, South-Eastern regions have a negative and significant effect (-28.2 percentage points), while the others have remarkably small, not significant coefficients. 

In general, the results of the two approaches are rather similar: the point estimates are close, and we only observe a slight decrease in statistical significance in South-East regions in both panels – which supports the hypothesis of a constant slope of the outcome variable with respect to the running variable. It is worth remarking that the instrumental variable estimates the local average treatment effect (LATE), assuming that there are no "defiers"; this, however, seems plausible given the figures reported in Gonzalez (2021), which suggest that the coverage areas are wide enough such that the connection persists for various kilometers after stepping into them. Hence, this can be seen to support the assumption that the treatment dummy does not decrease at the cutoff.
		
The remaining differences with respect to Table 2 in Gonzalez (2021) are likely attributable to the fact that we are estimating the treatment effect using the assignment to the treatment as an instrument, which likely increases the standard errors. The effect of the treatment is estimated by relying on fitted values on the assignment to treatment, hence increasing overall uncertainty in the second stage estimation. Overall, the instrumental variable seems to capture well the "true" direction of table 2 estimates of the original paper. 

 */
		

