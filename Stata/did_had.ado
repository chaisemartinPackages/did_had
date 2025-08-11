*** Program to implement the heterogenous adoption design estimator ***

*** Changes Felix:
* Adapt _no_updates, same as in did_multiplegt_dyn
* Add test on quasi stayers

* This Version: 05.08.2025


capture program drop did_had

program did_had, eclass
	version 12.0
	syntax varlist(min=4 max=4 numeric) [if] [in] [, effects(integer 1) placebo(integer 0) level(real 0.05) kernel(string) bw_method(string) graph_off dynamic trends_lin yatchew _no_updates graph_opts(string)]

	if "`_no_updates'" == "" {
		if uniform() < 0.01 {
			noi ssc install did_had, replace
		}
	}

	
****The path of the initial dataset
local dataset_name_XX `c(filename)'
//>>
preserve
mata: mata clear
	
*** Check if auxillary packages are installed
qui cap which lprobust
if _rc{
	di ""
	di as error "You have not installed the nprobust command which is used within the did_had command."
	di `"{stata "net install nprobust, from(https://raw.githubusercontent.com/nppackages/nprobust/master/stata) replace": Click here to install nprobust}"'
	di ""
	
	exit
}

qui cap which gtools
if _rc{
	di ""
	di as error "You have not installed the gtools package which is used within the did_had command."
	di `"{stata "ssc install gtools": Click here to install gtools}"'
	di as input _continue ""
	
	exit
}	

if "`yatchew'" != "" {
qui cap which yatchew_test
	if _rc {
		di ""
		di as error "You have not installed the yatchew_test command which is used within the did_had command with the yatchew option."
		di `"{stata "ssc install yatchew_test replace": Click here to install yatchew_test}"'
		di ""
		exit
	}
}

	
qui{
	
*** set default for kernel if not specified
if "`kernel'"==""{
	local kernel="epa"
}	

*** set default bandwidth selection method if not specified 
if "`bw_method'"==""{
	local bw_method="mse-dpi"
}
	
*** drop all variables that will be generated -> some not used anymore???
capture drop outcome_XX
capture drop group_XX
capture drop time_XX
capture drop treatment_XX
capture drop F_g_XX
capture drop F_g_XX_int

ereturn clear

tokenize `varlist' // Specify Variables as y g t d

//dropping observations not included in the if condition
	if "`if'" !=""{
	keep `if'
	}
// dropping observations not included in the in condition
	if "`in'" !=""{
	keep `in'
	} 
	
*** Import the 4 input variables 
gen outcome_XX=`1'
gegen group_XX=group(`2')
gegen time_XX=group(`3')
gen treatment_XX=`4'	

*** Sort the data 
sort group_XX time_XX

*** determine the switching period. Ensure F_g only 1 in the first period
gen F_g_XX_int=(treatment_XX[_n]!=treatment_XX[_n-1] & group_XX[_n]==group_XX[_n-1]) // dropped this condition
gen temp_F_g_XX=time_XX if F_g_XX_int==1
replace temp_F_g_XX=0 if temp_F_g_XX==.
bys group_XX: gegen F_g_XX=min(temp_F_g_XX if temp_F_g_XX>0)
drop temp_F_g_XX

*** check if all groups switch at the same date
sum F_g_XX
if r(sd)!=0{
	di as error ""
	di as error "Not all groups change their treatment"
	di as error "at the same period for the first time."
	di as error "The estimator from de Chaisemartin & D'Haultfoeuille (2024)"
	di as error "is only valid if this condition is met."
	exit
}
scalar F_XX=r(mean)

// Modif Felix (29.07.25): Allow to include stayers in the estimation
gen d_miss_XX = (treatment_XX==.)
bys group_XX: gegen no_d_miss_XX=max(d_miss_XX)
bys group_XX: gen no_d_miss_g_XX=no_d_miss_XX if _n==1
count if F_g_XX==. & no_d_miss_g_XX==0
local n_stayers=r(N)
replace F_g_XX=F_XX if F_g_XX==. & no_d_miss_XX==0
cap drop d_miss_XX no_d_miss_XX no_d_miss_g_XX

if `n_stayers'>0{
	noi di ""
	noi di as input "NOTE: `n_stayers' groups are untreated at period two"
}

*** checking number of effects/placebos

* compute max effects 
sum time_XX
scalar t_min_XX=r(min)
scalar T_max_XX=r(max)

scalar l_XX=T_max_XX-F_XX+1

* compute max placebos 
scalar l_placebo_XX=F_XX-2


*** Modif Felix: new option allowing for linear trends
if "`trends_lin'"!=""{
	
	if F_g_XX<3{
		di as error "Your data has less than 3 pre-treatment periods so it is impossible for the command to account for linear trends."
		
		exit
	}
	
	* Adjust the maximum number of placebos 
	scalar l_placebo_XX=l_placebo_XX-1

	* Transform the outcomes; generate "proxy for linear trend"
	gen lin_trend_int_XX=outcome_XX-outcome_XX[_n-1] if time_XX==F_g_XX-1
	bys group_XX: gegen lin_trend_XX=mean(lin_trend_int_XX)
	drop lin_trend_int_XX
	
	* Then take this into account when calculating the effects
}


* check again the number of specified effects 
if `effects'>l_XX{
	di as error ""
	di as error "The number of effects requested is too large."
	di as error "The number of effects which can be estimated is at most " l_XX "."
	di as error "The command will therefore try to estimate " l_XX " effect(s)."
	
	* adjust the number of effects
	local effects=l_XX
}


if `placebo'!=0{
	if l_placebo_XX<`placebo'&`effects'>=`placebo'{
		di as error ""
		di as error "The number of placebos which can be estimated is at most " l_placebo_XX "."
		di as error "The command will therefore try to estimate " l_placebo_XX " placebo(s)."
		
		* adjust the number of placebos
		local placebo=l_placebo_XX
	}

	if `effects'<`placebo'{
		di as error ""
		di as error "The number of placebo requested cannot be larger than the number of effects requested."
		di as error "The command cannot compute more than " l_placebo_XX " placebo(s)."
		
		* adjust the number of placebos
		local placebo=min(l_placebo_XX,`effects')
	}
}


*** Sample preparation for the inner estimation command
sort group_XX time_XX

* replace treatment symmetrically for the pre-treatment periods
if `placebo'!=0{
	
if "`trends_lin'"==""{
	forvalues i=1/`placebo'{
		replace treatment_XX=treatment_XX[_n+2*`i'] if time_XX==F_g_XX-`i'-1
	}
}

if "`trends_lin'"!=""{
	forvalues i=2/`=`placebo'+1'{
		replace treatment_XX=treatment_XX[_n+2*`i'-1] if time_XX==F_g_XX-`i'-1
	}
}

}


* Modif Felix: option to scale by cummulative treatment change ("dynamic")
if "`dynamic'"!=""{
	
	cap drop cummulative_XX
	gen cummulative_XX=treatment_XX if F_g_XX==time_XX
	
	* Generate the cummulative treatment change	
	replace cummulative_XX=cummulative_XX[_n-1]+treatment_XX if group_XX==group_XX[_n-1] & cummulative_XX[_n-1]!=.
	
	if `placebo'!=0{
		
		if "`trends_lin'"==""{
		forvalues i=1/`placebo'{
			replace cummulative_XX=cummulative_XX[_n+2*`i'] if time_XX==F_g_XX-`i'-1
		}
		}
		
		if "`trends_lin'"!=""{
		forvalues i=2/`=`placebo'+1'{
			replace cummulative_XX=cummulative_XX[_n+2*`i'-1] if time_XX==F_g_XX-`i'-1
		}
		}
		
	}
	
}

* matrix storing all results
mat res_XX=J(`effects'+`placebo',10,.)
local rownames ""

if "`yatchew'" != "" {
	mat y_res_XX = J(`effects'+`placebo',5, .)
	local sigma = ustrunescape("\u03c3")
	local squared = ustrunescape("\u00b2")
    matrix colnames y_res_XX = "`sigma'`squared'_lin" "`sigma'`squared'_diff" "T_hr" "p-value" "N"
}

*** call the estimation program inside loops over the number of effects/placebos
if `placebo'!=0{
forvalue i=1/`placebo'{
	
	cap drop placebo_`i'
	if "`trends_lin'"==""{
	gen placebo_`i'=outcome_XX-outcome_XX[_n+`i'] if group_XX==group_XX[_n+`i'] & F_g_XX==time_XX[_n+`i'+1]
	}
	
	// Modif Felix: addition for trends_lin 
	if "`trends_lin'"!=""{
		gen placebo_`i'=outcome_XX-outcome_XX[_n+`i'] if group_XX==group_XX[_n+`i'] & F_g_XX==time_XX[_n+`i'+2]
		
		replace placebo_`i'=placebo_`i'+`i'*lin_trend_XX
	}
	
	did_had_est placebo_`i' group_XX treatment_XX if placebo_`i'!=., level(`level') kernel(`kernel') bw_method(`bw_method') `dynamic' `yatchew' placebo_yatchew

	matrix res_XX[`i',1]=scalar(ß_qs_XX)
	matrix res_XX[`i',2]=scalar(se_naive_XX)
	matrix res_XX[`i',3]=scalar(low_XX)
	matrix res_XX[`i',4]=scalar(up_XX)
	matrix res_XX[`i',5]=scalar(G_XX)
	matrix res_XX[`i',6]=scalar(h_star)
	matrix res_XX[`i',7]=scalar(within_bw_XX)
	matrix res_XX[`i',10]=-`i'
	
	if "`yatchew'" != "" {
		forv j = 1/5 {
			mat y_res_XX[`i', `j'] = yat_res[1, `j']
		}
	}
	
	local rownames "`rownames' Placebo_`i'"
}	
}

forvalue i=1/`effects'{
	
	cap drop effect_`i'
	gen effect_`i'=outcome_XX-outcome_XX[_n-`i'] if group_XX==group_XX[_n-`i'] & F_g_XX==time_XX[_n-`i'+1] 
	
	// MOdif Felix: addition for trends_lin 
	if "`trends_lin'"!=""{
		replace effect_`i'=effect_`i'-`i'*lin_trend_XX
	}	
	
	did_had_est effect_`i' group_XX treatment_XX if effect_`i'!=., level(`level') kernel(`kernel') bw_method(`bw_method') `dynamic' `yatchew'
	
	matrix res_XX[`placebo'+`i',1]=scalar(ß_qs_XX)
	matrix res_XX[`placebo'+`i',2]=scalar(se_naive_XX)
	matrix res_XX[`placebo'+`i',3]=scalar(low_XX)
	matrix res_XX[`placebo'+`i',4]=scalar(up_XX)
	matrix res_XX[`placebo'+`i',5]=scalar(G_XX)
	matrix res_XX[`placebo'+`i',6]=scalar(h_star)
	matrix res_XX[`placebo'+`i',7]=scalar(within_bw_XX)
	matrix res_XX[`placebo'+`i',8]=np_test[1,1]
	matrix res_XX[`placebo'+`i',9]=np_test[2,1]
	matrix res_XX[`placebo'+`i',10]=`i'
	
	if "`yatchew'" != "" {
		forv j = 1/5 {
			mat y_res_XX[`placebo'+`i', `j'] = yat_res[1, `j']
		}
	}
	
	local rownames "`rownames' Effect_`i'"
}

matrix colnames res_XX = "Estimate" "SE" "LB CI" "UB CI" "N" "BW" "N in BW" "T" "p-val"
matrix rownames res_XX = `rownames'
if "`yatchew'" != "" {
	matrix rownames y_res_XX = `rownames'
}

} // end qui

*** Display the results 
display _newline
di as input "{hline 112}"
di as input _skip(36) "Effect Estimates"_skip(46) "QUG* Test"
di as input "{hline 90}"_skip(1)"{hline 21}"
matlist res_XX[`placebo'+1...,1..9]
di as input "{hline 112}"
di as input _skip(90) "*Quasi-untreated group"

if `placebo'!=0{
* Only shown when some placebos are requested
display _newline
di as input "{hline 90}"
di as input _skip(36) "Placebo Estimates"
di as input "{hline 90}"
matlist res_XX[1..`placebo',1..7]
di as input "{hline 90}"
}

if "`yatchew'" != "" {
* Only shown when yatchew_test is requested
display _newline
di as input "{hline 70}"
di as input _skip(15) "Heteroskedasticity-robust Yatchew Test"
di as input "{hline 70}"
matlist y_res_XX[`placebo'+1...,1..5]
if `placebo' != 0 {
	matlist y_res_XX[1..`placebo',1..5]
}
di as input "{hline 70}"

}

qui{

*** Adapt matrix for Graph
matrix mat_graph_XX=J(1,10,0) \ res_XX
if `placebo'!=0{
mata: res_new_XX=st_matrix("res_XX")
mata: res_graph_XX=res_new_XX[range(`placebo',1,1),.] \ J(1,10,0) \ res_new_XX[(`placebo'+1::`placebo'+`effects'),]
mata: st_matrix("mat_graph_XX", res_graph_XX)
}


*** Store ereturns (including e(b) and e(V))
ereturn clear	

** Store e(b) and e(V)
* e(b)
matrix b=res_XX[1...,1]'

* e(V)
local nc=`placebo'+`effects'
matrix V_int = J(`nc', `nc', 0)
forv i=1/`=`nc'' {
	mat V_int[`i', `i'] = res_XX[`i', 2]
}

* Now we have SE's not variances
matrix V=V_int*V_int

matrix rownames V = `rownames'
matrix colnames V = `rownames'

* post ereturn
ereturn post b V

* other ereturns 
ereturn matrix estimates=res_XX

if "`yatchew'" != "" {
	mat fy_res_XX =  y_res_XX[`placebo'+1...,1..5]
	if `placebo' != 0 {
		mat fy_res_XX = (fy_res_XX\y_res_XX[1..`placebo',1..5])
	}
	ereturn matrix yatchew_res = fy_res_XX
}
	
//>>
restore	

} // end qui

* Output ERC line 
display _newline
di as text "The development of this package was funded by the European Union (ERC, REALLYCREDIBLE,GA N°101043899)."	

qui{

if "`graph_off'"==""{
preserve
drop _all
svmat mat_graph_XX
twoway (rcap mat_graph_XX3 mat_graph_XX4 mat_graph_XX10, lcolor(maroon)) ///
	(scatter mat_graph_XX1 mat_graph_XX10, msize(medlarge) msymbol(o) mcolor(navy) legend(off)) ///
	(line mat_graph_XX1 mat_graph_XX10, lcolor(navy)) , ///
	 title("Estimates from did_had") xtitle("Relative time to treatment change") ///
	 ytitle("Effect") xlabel(-`placebo'(1)`effects') `graph_opts'	 
restore
}

} // end qui
	
end // end did_had	
	
	
	
*** Second interior program that runs the estimation ***

capture program drop did_had_est	
	
program did_had_est, eclass
version 12.0
syntax varlist(min=3 max=3 numeric) [if] [in] [, level(real 0.05) kernel(string) bw_method(string) dynamic yatchew placebo_yatchew]	

qui{
	
capture drop y_diff_XX	
capture drop group_est_XX
capture drop treatment_1_XX
capture drop treatment_2_XX
capture drop treatment_3_XX
capture drop treatment_4_XX
capture drop y_diff_2_XX
capture drop g_XX
capture drop n_group_XX
capture drop mean_y_diff_XX
capture drop mean_treatment_XX
capture drop D_bar_temp_XX
capture drop D_bar_h_XX
capture drop V_hat_h_temp_XX
capture drop V_hat_h_XX
capture drop cov_hat_h_temp_XX
capture drop cov_hat_h_XX
capture drop mu_hat_temp1_XX
capture drop mu_hat_temp2_XX
capture drop mu_hat_XX
capture drop ß_qs_XX
capture drop MSE_XX
capture drop percentile_XX
capture drop grid_XX

preserve

//dropping observations not included in the if condition
	if "`if'" !=""{
	keep `if'
	}
// dropping observations not included in the in condition
	if "`in'" !=""{
	keep `in'
	} 	
	
	
*** Define important Variables
gen double y_diff_XX=`1' // Note that this has to be in first differences
gegen double group_est_XX=group(`2')
gen double treatment_1_XX=`3'	 

*** generate squared of the treatment
gen double treatment_2_XX=treatment_1_XX^2
gen double treatment_3_XX=treatment_1_XX^3
gen double treatment_4_XX=treatment_1_XX^4
gen double y_diff_2_XX=y_diff_XX^2

***** Compute test that there are quasi stayers
mata: D_2 = st_data(., "treatment_1_XX")
mata: D_2_np = sort(select(D_2, D_2 :> 0), 1)
mata: T = D_2_np[1,1]/(D_2_np[2,1] - D_2_np[1,1])
mata: st_matrix("np_test", (T\(1-(1+T^(-1))^(-1))))
mata: mata drop D_2 D_2_np T
* Compute p-values 
// Both test statistics converge in distribution to (E_1/E_2) where E_1 and E_2 are iid random variables from an Exponential(1) distribution
// This yields the CDF P(T<t) = (\alpha)/(\alpha + (\beta/t)) where \alpha and \beta are the parameters of the two exponential distributions, so in this case both are 1

***** Compute optimal bandwidth, mu_hat and its bias using "lprobust" *****

gen grid_XX=0 if _n==1

lprobust y_diff_XX treatment_1_XX, eval(grid_XX) kernel(`kernel') bwselect(`bw_method')

scalar h_star=e(Result)[1,2] 
scalar mu_hat_XX_alt=e(Result)[1,5] 
scalar mu_hat_XX_alt_ub=e(Result)[1,6] 
scalar M_hat_hG_XX=e(Result)[1,5]-e(Result)[1,6]
scalar se_mu_XX=e(Result)[1,8] 
scalar coverage_mu_XX=(e(Result)[1,9]<=0&e(Result)[1,10]>=0) 


*** Number of Groups 
gegen n_group_XX=group(group_est_XX)
sum n_group_XX
scalar G_XX=r(max)
capture drop n_group_XX

*** consruct all the parts for ß_qs
sum y_diff_XX
scalar mean_y_diff_XX=r(mean)

if "`dynamic'"==""{
	sum treatment_1_XX
	scalar mean_treatment_XX=r(mean)
}
if "`dynamic'"!=""{
	sum cummulative_XX
	scalar mean_treatment_XX=r(mean)
}

*** Consruct ß_qs
scalar ß_qs_XX=(mean_y_diff_XX-mu_hat_XX_alt)/mean_treatment_XX

*** Estimate Bias for CI
** B_hat_Hg_XX
scalar B_hat_Hg_XX=-M_hat_hG_XX/mean_treatment_XX

*** SE's and CI's 
** SE
scalar se_naive_XX=se_mu_XX/mean_treatment_XX

** CI
local alpha=`level'

scalar low_XX=ß_qs_XX-B_hat_Hg_XX-invnormal(1-(`alpha'/2))*scalar(se_naive_XX)
scalar up_XX=ß_qs_XX-B_hat_Hg_XX+invnormal(1-(`alpha'/2))*scalar(se_naive_XX)

*** Count number of groups within the bandwidth
count if (treatment_1_XX<=scalar(h_star))
scalar within_bw_XX=r(N)

if "`yatchew'" != "" {
	local ordn = "`placebo_yatchew'" == ""
	cap yatchew_test y_diff_XX treatment_1_XX, het_robust order(`ordn')
	if _rc != 0 {
		ssc install yatchew_test, replace
		yatchew_test y_diff_XX treatment_1_XX, het_robust order(`ordn')
	}
	mat yat_res = r(results)
}

restore

} // end of quiet

end // end did_had_est


