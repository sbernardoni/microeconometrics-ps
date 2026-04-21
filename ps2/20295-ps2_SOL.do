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
ssc install estout, replace
ssc install avar, replace
ssc install eventstudyinteract, replace
ssc install bacondecomp, replace
ssc install egenmore, replace
ssc install _gwmean, replace
ssc install twowayfeweights, replace
ssc install ftools, replace
ssc install moremata, replace
ssc install reghdfe, replace
ssc install gtools, replace
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
    global filepath "C:\Users\Sara\Documents\ESS\20295 - Microeconometrics\microeconometrics-ps\ps2"
	global output "C:\Users\Sara\Documents\ESS\20295 - Microeconometrics\microeconometrics-ps\ps2\ps2_output"
}

if ("`user'" == "flore") {
    global filepath 
"C:\Users\flore\OneDrive\Documents\Bocconi\Year 2\Microeconometrics\PS 2\files"
	global output 
"\Users\flore\OneDrive\Documents\Bocconi\Year 2\Microeconometrics\PS 2\files\outputs"
}


if ("`user'" == "gabrielemole") {
    global filepath "C:\Users\Stealth\Desktop\microeconometrics-ps\ps2"
	global output "C:\Users\Stealth\Desktop\microeconometrics-ps\ps2\ps2_output"
}

*=============================================================================
/* 								Question 1 									*/
/*                      													*/
*=============================================================================

use "https://github.com/sbernardoni/microeconometrics-ps/raw/refs/heads/main/ps2/ps2_data/pset_2.dta", clear

/* (a) Note that one of the variables in the data set is stpop, the state population. In the next exercises, you should follow Wolfers (2006) in weighting both your descriptive output and your analysis by the state population. A short summary of the different weighting procedures in Stata is provided here ([1,2]). Given that divorce rates are an average computed in each state and the variable stpop provides the population in each of these states, which is the weight you should use when reporting the evolution of divorce rates or a regression of divorce rates on unilateral divorce laws to match the analysis in Wolfers (2006)? */

	/* A: The framework adopted in Wolfers (2006) relies on a population-weighted least squares model, both in the descriptive and the regression analysis. Since divorce rates are an average at the state level, following Dupraz (2013), we should use the state population (stpop) as analytical weights, together with standard errors robust to heteroskedasticity. Indeed, this would greatly improve computational efficiency while taking into account that the random unit, in this setting, is the state and not the individual.*/
	
	
	
/* (b) The article relies on the timing of the introduction of unilateral divorce laws to compare divorce rates in the two possible regimes. One of the assumptions of this analysis is that states with the previous divorce law and the ones that introduced unilateral divorce laws would both follow parallel trends in their divorce rates in the absence of the changes to the legislation. Create 2 different graphs to support this assumption: (i) the first graph should convey the same message as the one in Figure 1 of the original paper, comparing states that did not change their divorce laws during 1968 - 1988 (Friedberg's sample) and the ones that did; (ii) the second graph should perform the same description, but focusing on the simpler analysis we will perform in the next exercise: compare the states adopting the unilateral divorce law between 1969 and 1973 to the ones that introduced it in the year 2000, only reporting the time trend up to 1978 and including a vertical line between 1968 and 1969 (when the first reforms in our sample started). Do your results support the assumption of parallel trends? */

preserve
	egen total_pop_by_year = total(stpop), by(year)
	gen wgt = stpop/total_pop_by_year

	gen TREATED = (lfdivlaw >= 1968 & lfdivlaw <= 1988)
	sum TREATED 

	egen div_rate_tre = wmean(div_rate) if TREATED == 1, by(year) weight(wgt)
	egen div_rate_con = wmean(div_rate) if TREATED == 0, by(year) weight(wgt)

	collapse div_rate_tre div_rate_con, by(year)

	gen div_rate_dif = div_rate_tre - div_rate_con
	

* Graph 1
	
	#delimit ;

	graph set window fontface "Times New Roman";

	graph twoway	
		(line div_rate_tre year, lcolor(black) lwidth(thick))
		(line div_rate_con year, lcolor(gs5) lwidth(thick))
		(line div_rate_dif year, lcolor(black) lp(dash)) 
		(function y = 0.2, range(1968 1988) lcolor(black) lpattern(solid) lwidth(medium))
		,
		xline(1968 1988, lp(solid)) 
		ylabel(0(1)7, grid glstyle(solid))
		yline(0, lp(solid))
		xlabel(1956(2)1998, nogrid angle(45))
		xmtick(1957(2)1999)
		legend(
			pos(12)
			order(
				1 "Reform states"
				2 "Control states"
				3 "Difference in divorce rates: Reform states less controls"
				)
			region(lstyle(solid) lcolor(black) lwidth(thin))
		)
		xtitle("Year")
		text(0.3 1982 "Friedberg's sample", size(small) color(black) place(n))	
		text(6.6 1973 "Reform period", size(small) color(black) place(n))
		ytitle("Divorce rate" "Divorces per 1,000 persons per year")

	;

	#delimit cr

	graph export "$output/Graph_1.pdf", replace

restore


* Graph 2


preserve
egen total_pop_by_year = total(stpop), by(year)
gen wgt = stpop/total_pop_by_year

gen TREATED = (lfdivlaw >= 1969  & lfdivlaw <= 1973)

egen div_rate_tre = wmean(div_rate) if TREATED == 1, by(year) weight(wgt)
egen div_rate_con = wmean(div_rate) if TREATED == 0 & lfdivlaw == 2000, by(year) weight(wgt)

collapse div_rate_tre div_rate_con, by(year)

gen div_rate_dif = div_rate_tre - div_rate_con

drop if year > 1978


#delimit ;

graph set window fontface "Times New Roman";

graph twoway	
    (line div_rate_tre year, lcolor(black) lwidth(thick))
    (line div_rate_con year, lcolor(gs5) lwidth(thick))
    (line div_rate_dif year, lcolor(black) lp(dash)) 
    
    ,
    xline(1968.5, lp(solid))
    ylabel(0(1)7, grid glstyle(solid))
    yline(0, lp(solid))
    xlabel(1956(2)1978, nogrid angle(45))
    xmticks(1957(2)1979)
    legend(
        pos(12)
        order(
            1 "Reform states"
            2 "Control states"
            3 "Difference in divorce rates: Reform states less controls"
            )
        region(lstyle(solid) lcolor(black) lwidth(thin))
    )
    xtitle("Year")
    ytitle("Divorce rate" "Divorces per 1,000 persons per year")

;

#delimit cr

graph export "$output/Graph_2.pdf", replace
restore 


	/* A: In both graphs, we can see that countries in the treatment group have higher divorce rates at the baseline. The trend in divorce rates in the two groups seems rather similar, even more so in graph 2, which at first might support the parallel trend assumption.

However, in both graphs we can observe a steady increase in the difference between the two groups, which may contradict the assumption. Indeed, it might simply be the case that, since treated states already had higher divorce rates in the first place, the reform simply freed up the marriages that could not break up under bilateral divorce laws that had been cumulating over the years.

Overall, it is not possible to determine a priori whether the parallel trends assumption is satisfied. To understand whether the increase in the difference between the two groups before the reform is statistically significant, one would have to carry out an event study or any other formal test. */


	
/* Let us now start an analysis of the effects of the introduction of unilateral divorce laws. As a first step, let us perform a 2-period difference-in-difference analysis using "long differences", focusing on the evolution of divorces between 1968 and 1978. Keeping only these 2 years in our sample, you should compare states adopting the unilateral divorce law between 1969 and 1973 to the ones that introduced it in the year 2000. On this restricted sample, you should create: (i) a variable UNILATERAL equal to 1 if a state introduced the unilateral divorce law during this period (as signaled by variable lfdivlaw); (ii) a variable POST equal to 1 if the year is 1978; and (iii) a variable POST UNILATERAL when both POST and UNILATERAL are equal to 1. */
	
/* (c) Now estimate the following regressions: */

preserve

	gen UNILATERAL = (lfdivlaw >= 1969  & lfdivlaw <= 1973)
	keep if year == 1968 | year == 1978
	drop if lfdivlaw != 2000 & UNILATERAL == 0
	gen POST = (year == 1978)
	gen POST_UNILATERAL = (POST == 1 & UNILATERAL == 1)
	
	encode st, generate(state)
	
	/* (i) A pooled OLS regression of the divorce rate per 1,000 people (div rate) on POST UNILATERAL and POST; */

	reg div_rate POST POST_UNILATERAL [aweight = stpop], vce(robust)
	
	/* (ii) A full Difference-in-Differences specification, including POST, UNILATERAL and POST UNILATERAL as regressors; */
	
	reg div_rate POST UNILATERAL POST_UNILATERAL [aweight = stpop], vce(robust)

	
	/* (iii) Based on the graphs you created in section (a), could you say something about the difference in the coefficients from regressions (i) and (ii)? What is the effect of introducing unilateral divorce laws according to this analysis? */

	/* A: Regression 1 does not control for the initial differences between the groups: however, graphs 1 and 2 clearly display a difference in the baseline divorce rates, which is not being taken into account here. When including UNILATERAL as a regressor, indeed, the estimated treatment effect changes both in size and magnitude, moving from 1.7 to -0.005. Moreover, the effect goes from being statistically significant at any conventional level to not being significant at all.

Hence, this analysis would suggest that introducing unilateral divorce laws did not cause an increase in divorce rates, provided that the parallel trends assumption holds.*/
	
	
	
/* (d) Generate a 3 by 3 matrix with row and column labels as follows: SEE PS2 
Difference 1 should show differences across columns while Difference 2 across lines. Complete this matrix with the averages of div rate, replicating the results you have found in the previous regression. Then, export the matrix to an Excel table named TABLE 1.*/

	matrix table_1 = J(3,3,.)

	* g=1, t=1
	sum div_rate if UNILATERAL==1 & POST==1 [aweight = stpop]
	scalar AVG_Y_1_1 = r(mean)
	matrix table_1[1,1] = AVG_Y_1_1 
	* g=1, t=0
	sum div_rate if UNILATERAL==1 & POST==0 [aweight = stpop]
	scalar AVG_Y_1_0 = r(mean)
	matrix table_1[2,1] = AVG_Y_1_0 

	* g=0, t=1
	sum div_rate if UNILATERAL==0 & POST==1 [aweight = stpop]
	scalar AVG_Y_0_1 = r(mean)
	matrix table_1[1,2] = AVG_Y_0_1 

	* g=0, t=1
	sum div_rate if UNILATERAL==0 & POST==0 [aweight = stpop]
	scalar AVG_Y_0_0 = r(mean)
	matrix table_1[2,2] = AVG_Y_0_0

	*
	scalar DiD = (AVG_Y_1_1 - AVG_Y_1_0) - (AVG_Y_0_1 - AVG_Y_0_0)
	scalar list DiD
	matrix table_1[3,3] = DiD
	matrix table_1[3,1] = AVG_Y_1_1 - AVG_Y_1_0
	matrix table_1[3,2] = AVG_Y_0_1 - AVG_Y_0_0
	matrix table_1[1,3] = AVG_Y_1_1 - AVG_Y_0_1
	matrix table_1[2,3] = AVG_Y_1_0 - AVG_Y_0_0

	matrix colnames table_1 = UNILATERAL=1 UNILATERAL=0 Difference_2
	matrix rownames table_1 = POST=1 POST=0 Difference_1

	matrix list table_1
	
	putexcel set "$output/Table_1.xlsx", replace
	
	putexcel A1=matrix(table_1), names
	putexcel C1:C4, border(right)
	putexcel A3:D3, border(bottom)
	putexcel A1:D1, border(bottom)
	putexcel A1:D1, border(top)
	putexcel A4:D4, border(bottom)
restore

	
/* (e) We will now perform the analysis using our complete data set, as in the main results of Wolfers (2006). For this, always focus on the same sample as the one used in Table 2 of the original paper (keeping observations between 1956 and 1988). Load once again our data set and create the dummy variable IMP UNILATERAL, which equals 1 whenever a state has already introduced unilateral divorce laws (as signaled by variable lfdivlaw). Now run the following regressions: */

preserve 
	encode st, generate(state)
	keep if year >= 1956 & year <= 1988
	gen IMP_UNILATERAL = (lfdivlaw <= year)
	gen time = year - 1955
	gen time2 = time*time


	/* (i) A regression of div rate on state and year dummies and the dummy IMP UNILATERAL that you created. */
		
	reg div_rate i.year i.state IMP_UNILATERAL [aweight = stpop], vce(robust)
	
	/* (ii) Perform the same regression as the one described above, now including state-specific linear time trends.  */
	
	reg div_rate i.year i.state i.state##(c.time) IMP_UNILATERAL [aweight = stpop], vce(robust)
	
	/* (iii) In addition to state-specific linear time trends, include also quadratic state-specific time trends. */
	
	reg div_rate i.year i.state i.state##(c.time c.time2) IMP_UNILATERAL [aweight = stpop], vce(robust)
restore

	/* (iv) Interpret the results of all 3 regressions. Can you think of a reason for the results to change across specifications? Under which assumption should these results be the same? */ 
	
	/* A: Assuming that the parallel trends assumption holds, in all the specifications of the regression the coefficient of IMP_UNILATERAL represents the average treatment effect of the introduction of unilateral divorce laws on divorce rates.

The first specification, controlling for time-invariant state fixed effects and time variant effects shared across states, supports the findings in the previous analysis: the point estimate is -0.055 and is not statistically significant, implying that the treatment effect is null.

The second specification, including state-specific linear time trends, however, yields different results: the coefficient increases to 0.477 and becomes statistically significant at every conventional level. This result mirrors closely the findings of Friedberg (1998), where the baseline specification suggested no change in divorce rates, while the regression controlling for state-specific linear trends implied a positive and significant effect. Such a dramatic change might suggest that the baseline specification suffers from omitted variable bias, where the divorce rate is correlated with different trends among the subgroups.

Finally, the third specification – controlling for state-specific quadratic time trends – leads to similar estimates: the coefficient remains positive (0.334) and statistically significant, similarly to Friedberg's results. Overall, since the estimates of the average treatment effect change so drastically after accounting for state-specific time trends, it seems to be the case that the parallel trends assumptions does not hold in this specific setting. */
	
	

/* (f) In our current case study, unilateral divorce laws have been introduced subsequently in different states at different points in time. In such cases, we say that there was a staggered implementation of the treatment. Regressions with a single coefficient, as the ones performed in exercise e), may be biased in this setting. Let us now check some of the properties of these regressions. We will create a simulated data set of 3 periods and 2 states, where one state receives a treatment in the 2nd period and the other state only receives it in the 3rd period. The code below reproduces this simulation: */

/* Created simulated observations */

preserve
	clear

	set obs 6 
	gen obs = _n 
	gen state = floor(.9 + obs/3)
	bysort state : gen year = _n
	gen D = state == 1 & year == 3
	replace D = 1 if state == 2 & ( year == 2 | year == 3 )

	* Generate Y
	gen Y = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + runiform() / 100

	* Generate Y2
	gen Y2 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.3 * (state == 2 & year == 3) + runiform() / 100

	* Generate Y3
	gen Y3 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.4 * (state == 2 & year == 3) + runiform() / 100

	* Generate Y4
	gen Y4 = 0.1 + 0.02 * (year == 2) + 0.05 * (D == 1) + 0.5 * (state == 2 & year == 3) + runiform() / 100

		/* (i) Now perform regressions analogous to the one performed in exercise e question (i) for all 4 dependent variables created (that is, a state and year fixed-effects regression with an absorbing treatment dummy). Is it possible to estimate the treatment coefficient consistently in each of these cases? */
		
	reg Y i.state i.year D, vce(robust)
	twowayfeweights Y state year D, type(feTR) 
	reg Y2 i.state i.year D, vce(robust)
	twowayfeweights Y2 state year D, type(feTR) 
	reg Y3 i.state i.year D, vce(robust)
	twowayfeweights Y3 state year D, type(feTR) 
	reg Y4 i.state i.year D, vce(robust)
	twowayfeweights Y4 state year D, type(feTR) 


	/*extra: controlling for state specific time effects */
	reg Y2 i.state##i.year D, vce(robust)
	reg Y3 i.state##i.year D, vce(robust)
	reg Y4 i.state##i.year D, vce(robust)
restore
	
		/* A: No, only the first specification delivers a consistent estimate of the average treatment effect. Indeed, we can observe a great difference between the estimate given by the first regression and the others: the former is positive (0.057) and statistically significant at the 5% level, while the others are negative and not significantly different from zero at any conventional level.

The likely reason for this is the construction of the simulated data itself: the variable "Y" only includes a year fixed effect, whereas the others have a time-varying, state-specific effect that cannot be controlled for in the adopted specification. In fact, once the construction of the variables is taken into account (in the additional regressions above), the estimated average treatment effect resembles the one obtained using "Y" as the outcome variable, implying that the previous estimates were downward biased due to the omitted variables. */


	
/* (g) Use the Stata package "twowayfeweights" (or its R version, "TwoWayFEWeights"), based on De Chaisemartin and d'Haultfoeuille (2020), to estimate the weights attached to the regressions you estimated before. Can you explain why the sign of the estimated effect has changed between the regression on Y and the one on Y4? */

	/* A: The code was executed in part f).
	
	De Chaisemartin and d'Haultfoeuille (2020) show that in a staggered treatment design with heterogeneous treatment effects, the two-way fixed effects (TWFE) estimator β_fe can be decomposed as the expectation of a weighted sum of group-period average treatment effects Δg,t, coming from all pairwise difference in differences. The key problem is that some of these weights can be negative, which introduces a downward bias and may even produce a negative estimated coefficient when all true ATTs are positive. Negative weights arise because, under staggered adoption, some 
	units that serve as controls are themselves already treated, undermining the standard DiD logic.

	This is precisely what appears to occur here. The twowayfeweights output confirms that β_fe is a weighted combination that includes at least one negative weight. As the authors note, negative weights are more likely to be assigned to periods where many groups are treated and to groups treated for a long time — in this simulation, period 3 and state 2 
	(which enters treatment already in period 2). These are exactly the observations that drove the bias in the earlier regressions.
	
	A plausible explanation for the negative coefficient in the regression on Y4 is that the negatively-weighted ATE at (state 2, period 3) grows large enough to outweigh all the positive contributions, flipping the sign of β_fe. Since the outcome Y4 assigns a coefficient of 0.5 to the (state 2, period 3) cell — larger than the 0.3 and 0.4 used 
	in Y2 and Y3 — the downward bias compounds as we move from Y to Y4.

	This is consistent with the analytical example in De Chaisemartin and d'Haultfoeuille (2020). Using the residual decomposition 
		εg,t = Dg,t − Dg,. − D.,t + D.,.
(where εg,t is the residual error to compute the weights, Dg,t the treatment status dummy, Dg, the average treatment status of the group, D.,t, the average treatment status at that time, and D.,. the average treatment status overall) the authors show that the weight on Δ2,3 is negative. Decomposing β_fe as (DID1 + DID2)/2, the bias operates through
		DID2 = E[Δ1,3] − (E[Δ2,3] − E[Δ2,2]):
larger values of E[Δ2,3] − E[Δ2,2] push β_fe downward, which is why the estimated effect becomes increasingly negative across Y2, Y3, and Y4.
	*/
	
/* (h) Let us now revisit our analysis following Wolfers (2006). We will do this based on the decomposition proposed by Goodman-Bacon (2021). The author provides commands in both Stata and R for his decomposition. To install it in Stata, run the code below: */

preserve 

	/* (i) create a modified population variable init stpop equal to the population of each state in the first observed period of each state. */
	
	sort st year
	by st: gen first_obs = _n == 1
	gen init_stpop = stpop if first_obs
	replace init_stpop = init_stpop[_n - 1] if init_stpop == .

	/* (ii) Rerun regression i of exercise (e) (a regression of div rate on state and year dummies and the dummy IMP UNILATERAL that you created) using init stpop as your weights. */
	
	encode st, generate(state)
	keep if year >= 1956 & year <= 1988
	gen IMP_UNILATERAL = (lfdivlaw <= year)
	
	xtset state year
	xtreg div_rate IMP_UNILATERAL i.year [aweight = init_stpop], fe robust
	
	/* (iii) Run the command bacondecomp to analyze the decomposition of the treatment effect. Plot the graph showing the relationship between the treatment effect estimates and the corresponding weights. Briefly explain what is the analysis proposed by Goodman-Bacon (2021). Is there evidence of issues regarding negative weights? */
	
		bacondecomp div_rate IMP_UNILATERAL [aweight = init_stpop], robust  mcolors(blue red green)
	graph rename bacondecomp22
	graph export "$output/Bacon_decomposition_graph.pdf", replace
restore
	
	/* Goodman-Bacon (2021) decomposes the TWFE DiD estimator under staggered treatment into a variance-weighted average of all possible 2×2 DiD comparisons. Each pairwise DiD is formed between two groups and two time periods, and they differ in what serves as the control group: units that are never treated, units that are  always treated, or "timing groups" — units treated at a different time that temporarily serve as controls for one another.
	
	The weights attached to each 2×2 DiD depend on the size of the groups involved and the within-pair variance of the treatment indicator, which tends to be largest for groups treated near the middle of the panel. When treatment effects are constant over time, all weights are given a positive value depending on the variance and the estimator recovers an average of cross-group treatment. When treatment effects are heterogeneous, however, using already-treated units as controls can produce negative weights, since their potentially evolving treatment effects get subtracted from the comparison. The authors stress that this does not violate the parallel trends assumption per se, but it does mean the TWFE coefficient can be a misleading statistic. They recommend complementary strategies such as event studies, stacked DiD, or reweighting estimators.
	
	More formally, the probability limit of the TWFE estimator decomposes into the variance-weighted average treatment effect on the treated (VWATT), the variance-weighted common trends term (VWCT, assumed to equal zero under pairwise parallel trends), and a term ΔATThat captures within-cohort changes in treatment effects over time. When treatment effects evolve, this last term is non-zero and biases the estimate, which is when negative weights can appear.
	
	In our sample, the Bacon decomposition graph shows all estimated weights are positive — every point lies to the right of the y-axis. The negative TWFE coefficient for IMP_UNILATERAL is therefore likely driven by a concentration of negative 2×2 DiD estimates, rather than by negative weights per se. The vast majority of the total weight (approximately 88%) is allocated to never-treated units used as controls, while timing groups and always-treated units receive near-zero weight. Since already-treated controls are the main source of negative weighting in theory, and their role here is negligible, the decomposition does not point to a negative-weights problem in this application.
	*/
	
/* (i) Let us now perform an event-study regression, allowing for the unilateral divorce law coefficients to vary across time. Your analysis will follow table 2 in Wolfers (2006). We will have the period right before the introduction of the law as our basis of comparison, creating dummies for leads and lags for all other distances between our observation period and the law introduction in that state. This means that for any time period t and state s, the dummy Dτ st will be equal to one if in that specific period, state s has introduced unilateral divorce laws τ years before. Following the analysis in the main paper, we will set 
	
	SEE FORMULA ON PDF
	
That is, the dummy will be equal to one for all observations with 15 or more years of unilateral divorce law. For the lead dummies, let us restrict

	SEE FORMULA ON PDF
	
So that this dummy will equal 1 for all observations 10 or more years before the introduction of the unilateral divorce law in that state. Notice that this specification has some deviations from the one performed in table 2 of the original paper. */

	/* (i) Run the regresson below, using the unilateral divorce dummies Dτ st you created and state (πs) and year (γt) fixed effects. */
	
preserve
	use "https://github.com/sbernardoni/microeconometrics-ps/raw/refs/heads/main/ps2/ps2_data/pset_2.dta", clear


	encode st, generate(state)
	keep if year >= 1956 & year <= 1988
	gen IMP_UNILATERAL = 0
	replace IMP_UNILATERAL = 1 if lfdivlaw <= year
	gen no_law = 0 
	replace no_law=1 if lfdivlaw==2000
	xtset state year

	gen tau = year - lfdivlaw
	tab tau

	gen lead10 = 0
	replace lead10 = 1 if tau <=-10

	*Lead and lag dummies
	forvalues k = 9(-1)2 {
	gen lead`k' = tau == -`k'
	}

	forvalues k = 0/14 {
	gen lag`k' = tau == `k'
	}
	gen lag15 = 0
	replace lag15 = 1 if tau >= 15

	*Generate the linear and the squared time trends
	forval i=1/51{
		bysort state (year): gen time_trend_`i'=_n if state==`i' 
		replace time_trend_`i'=0 if time_trend_`i'==.
	}

	forval i = 1/51 {
		gen timetrend_square_`i' = time_trend_`i'^2
	}

	reghdfe div_rate lead* lag* [aweight = stpop], absorb(i.year i.state) cluster(state)
	estimates store reg_simple
	outreg2 using "$output/Reg1.xlsx", title("regression ex i point i") label excel replace

		
		/* (ii) Perform the same regression as the one described above, now including state-specific linear time trends. */
		
	reghdfe div_rate lead* lag* time_trend_* [aweight = stpop], absorb(i.year i.state) cluster(state)
	estimates store reg_timetrend
	outreg2 using "$output/Reg2.xlsx", title("regression ex i point ii") label excel replace

		
		/* (iii) In addition to state-specific linear time trends, include also quadratic state-specific time trends. */
		
	reghdfe div_rate lead* lag* time_trend_* timetrend_square_* [aweight = stpop], absorb(i.year i.state) cluster(state)
	estimates store reg_sqtime
	outreg2 using "$output/Reg3.xlsx", title("regression ex i point iii") label excel replace


	
	/* (iv) Interpret the results of all 3 regressions. What can we see in the behaviour of divorce rates through this analysis that was not possible in the single coefficient analysis? */
	
	/* The event-study regressions provide a richer picture of how divorce rates responded to the reform than the single-coefficient specifications in exercise (e). 
Across all three specifications, the pre-reform lead coefficients are small and statistically indistinguishable from zero. This supports the parallel trends assumption: absent the law change, treated and untreated states appear to have been on comparable trajectories. In the simple specification (no time trends), divorce rates rise sharply and significantly in the first two years after adoption — a plausible "pent-up demand" effect, as couples who had long wished to divorce were suddenly able to do so. 
Beyond the third post-reform year, coefficients become statistically insignificant, and there is a suggestion of a decline after year nine, though this is not robust to the inclusion of the linear and quadratic terms.

Adding state-specific linear trends extends the duration of the positive post-reform effect to roughly five years. This is consistent with the fact that if states were already on diverging trajectories before adoption, omitting trends would cause the regression to understate the true short-run response.

When quadratic trends are also included, the pattern reverts to a short-lived spike that dissipates by the third post-reform year, broadly resembling the simple specification.

Across all three models, most coefficients remain statistically insignificant. Only a handful of early post-treatment estimates are robustly positive, and precision declines as linear and quadratic trends are added. The overall picture is likely suggesting a genuine but transitory short-run response, with no strong evidence of a lasting effect on divorce rates. */


/* (j) Use the Stata command coefplot (or any other command of your choosing) to create a graph reporting the coefficients and the 95% confidence intervals of your 3 event-study regressions. */

coefplot ///
    (reg_simple, label("Simple Regression") msymbol(O) mcolor(blue)) ///
    (reg_timetrend, label("Linear Trend") msymbol(D) mcolor(red)) ///
    (reg_sqtime, label("Quadratic Trend") msymbol(T) mcolor(green)) ///
    , drop(_cons) ///
    keep(lead* lag*) ///
    xline(11, lpattern(dash) lcolor(gs10)) ///
    ciopts(recast(rcap) lwidth(medthin)) ///
    xlabel(1 "L10" 2 "L9" 3 "L8" 4 "L7" 5 "L6" 6 "L5" 7 "L4" 8 "L3" 9 "L2" 10 "L1" ///
           11 "0" 12 "1" 13 "2" 14 "3" 15 "4" 16 "5" 17 "6" 18 "7" 19 "8" 20 "9" 21 "10" ///
           22 "11" 23 "12" 24 "13" 25 "14" 26 "15", angle(45)) ///
    ylabel(, angle(horizontal)) ///
    xtitle("Event Time") ///
    ytitle("Coefficient") ///
    title("Event-Study Estimates with 95% Confidence Intervals") ///
    vertical
graph export "$output/event_study_regression.pdf", replace

/* (k) Wolfers (2006) presents a summary of the debate regarding the influence of the unilateral divorce law in the divorce rates. How do the conclusions of the paper differ from Friedberg (1998)? How does the author rationalize the difference in his findings? */

	/* Wolfers (2006) and Friedberg (1998) reach substantially different conclusions. Friedberg finds that unilateral divorce laws account for roughly one-sixth of the rise in divorce rates observed since the late 1960s, implying a persistent and sizeable effect. Wolfers, by contrast, argues that this estimate conflates the policy's true impact with pre-existing, state-specific trends in divorce behavior.

	Wolfers documents that divorce rates do increase sharply immediately after adoption, but that this effect is short-lived: early adopters tend to show lower divorce rates approximately 15 years after the reform. He rationalises the discrepancy by arguing that Friedberg's specification does not adequately control for underlying state-level dynamics, causing her single-coefficient DiD estimate to absorb trend differences that predate or are unrelated to the legislative change.*/

/* (l) Several different procedures to estimate a staggered Difference-in-Differences analysis have been proposed recently. Let us now perform one of these procedures. You will use command eventstudyinteract in Stata, based on Sun and Abraham (2021) 

Now perform an analogous analysis to the event-study regression in exercise (i) based on the Sun and Abraham (2021) estimation. Once again, report your results in an event-study graph. Are your results consistent with the ones from the original paper? Briefly explain what kind of correction your proposed algorithm is performing.*/

drop time_trend_*
drop timetrend_square_*

*simple
eventstudyinteract div_rate lead* lag* [aweight=stpop], cohort(lfdivlaw) control_cohort(no_law) absorb(i.year i.state) vce( cluster state)
estimates store reg_interact_simple
outreg2 using "$output/Reg4.xlsx", title("regression ex l 1") label excel replace
matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(lag* lead*) vertical yline(0) xtitle("Years after law") ytitle("Estimated effect") ///
				title("Simple Event Study") xlabel(, alternate)
graph export "$output/interact_simple.pdf", replace

*linear time trends

forval i=1/51{
	bysort state (year): gen time_trend_`i'=_n if state==`i' 
	replace time_trend_`i'=0 if time_trend_`i'==.
}
local lineartime time_trend_*

eventstudyinteract div_rate lead* lag* [aweight=stpop], cohort(lfdivlaw) covariates(`lineartime') control_cohort(no_law) absorb(i.year i.state ) vce(cluster state)
estimates store reg_interact_linear
outreg2 using "$output/Reg5.xlsx", title("regression ex l 2") label excel replace
*graph
matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(lag* lead*) vertical yline(0) xtitle("Years after law") ytitle("Estimated effect") ///
				title("Event Study with Linear Time Trends") xlabel(, alternate)
graph export "$output/interact_linear.pdf", replace
				
*squared time trends

forval i=1/51{
	bysort state (year): gen timetrend_sq_`i'=_n^2 if state==`i'
	replace timetrend_sq_`i'=0 if timetrend_sq_`i'==.
}
local squaretrend timetrend_sq_*

eventstudyinteract div_rate lead* lag* [aweight=stpop], cohort(lfdivlaw) control_cohort(no_law) covariates(`lineartime' `squaretrend') absorb(i.year i.state) vce(cluster state)
estimates store reg_interact_squared
outreg2 using "$output/Reg6.xlsx", title("regression ex l 3") label excel replace

*graph

matrix C = e(b_iw)
mata st_matrix("A",sqrt(diagonal(st_matrix("e(V_iw)"))))
matrix C = C \ A'
matrix list C
coefplot matrix(C[1]), se(C[2]) keep(lag* lead*) vertical yline(0) xtitle("Years after law") ytitle("Estimated effect") ///
				title("Event Study with Square Time Trends") xlabel(, alternate)
graph export "$output/final_graph.pdf", replace
				
restore
				
	/* Sun and Abraham (2021) address a core limitation of standard event-study regressions: under heterogeneous treatment effects, the coefficients on lead and lag dummies are contaminated by treatment effects from other cohorts and periods, rather than cleanly identifying the effect at each relative time.
	
The eventstudyinteract command resolves this through an interaction-weighted (IW) estimator. In a first step, it estimates cohort-specific average treatment effects on the treated (CATTs) for each adoption cohort separately. In a second step, it aggregates these CATTs across cohorts at each relative period, weighting by cohort shares. Because the CATTs are estimated within-cohort, the resulting estimates are robust to treatment effect heterogeneity across both cohorts and time.

Comparing our results to Wolfers (2006): the simple and linear-trend specifications reproduce the paper's main finding — a positive short-run effect in the first few years after adoption that fades toward zero within a few years. The quadratic-trend specification yields an anomalous pattern, with a significantly positive pre-trend coefficient ten years before adoption and a significantly negative coefficient beyond year eleven post-adoption, which might reflect over-fitting from the high-dimensional set of quadratic state trends rather than a genuine economic dynamic.