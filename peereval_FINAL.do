************************************************************************************
* This Do file generates the empirical results in the current version of the draft *
************************************************************************************

*********************************
* PART I: Rating-level Analysis *
*********************************

* Prepare the rating-level dataset

use perfeval_final.dta, clear

gen self = .
replace self = 1 if rateecode == ratorcode
replace self = 0 if rateecode != ratorcode

gen peer = .
replace peer = 1 if rank_rator == rank_ratee & self == 0
replace peer = 0 if rank_rator != rank_ratee & rank_rator != . & rank_ratee != . & self == 0

gen rator_head = 0
replace rator_head = 1 if ratorcode - yr * 1000 - dep * 100 == 1

keep if perfscore != . & P_4thresholds_rator != . & P_4thresholds_ratee != .

save perfeval_final_A.dta, replace

* Define variables for rating-level analysis (P_4thresholds)

gen rateepass = P_4thresholds_ratee
gen ratorpass = P_4thresholds_rator

gen peer_rateepass = peer * rateepass
gen peer_ratorpass = peer * ratorpass

gen ratorpass_rateepass = ratorpass * rateepass
gen peer_ratorpass_rateepass = peer * ratorpass * rateepass

gen p5_ratee = P_4thresholds_ratee * P_license_ratee 
gen p5_rator = P_4thresholds_rator * P_license_rator


* Define variables for rating-level analysis (certif)

gen rateecertif = certif_ratee
gen ratorcertif = certif_rator

gen peer_rateecertif = peer * rateecertif
gen peer_ratorcertif = peer * ratorcertif

gen ratorcertif_rateecertif = ratorcertif * rateecertif
gen peer_ratorcertif_rateecertif = peer * ratorcertif * rateecertif

save rating_level_analysis.dta, replace

* Ratee Qualification and Strategic Manipulation *
* (Results in TABLE 2)
use rating_level_analysis.dta, clear

eststo Rratee_pass: xi: regress perfscore peer rateepass peer_rateepass ib(12).rank_ratee i.ratee i.yr i.dep if self == 0 & ratorpass != ., cluster(rateecode) // Table 2: P_4thresholds
gen obs_tab2 = 0
replace obs_tab2 = 1 if e(sample) // 7346 obs

*eststo Ratee_certif: xi: regress perfscore peer rateecertif peer_rateecertif  ib(12).rank_ratee i.yr i.dep if self == 0, cluster(rateecode) // In Appendix (Robustness check): Certif

* Adding Rater Qualification *
* (Results in TABLE 3)

xi: regress perfscore peer rateepass peer_rateepass ratorpass peer_ratorpass ratorpass_rateepass peer_ratorpass_rateepass ib(12).rank_ratee ib(12).rank_rator i.ratee i.yr i.dep if self == 0, cluster(rateecode) // Table 3: P_4thresholds
gen obs_tab3 = 0
replace obs_tab3 = 1 if e(sample) // 7346 obs

tab rateepass if obs_tab2 == 1 & obs_tab3 == 0

*xi: regress perfscore peer rateecertif peer_rateecertif ratorcertif peer_ratorcertif ratorcertif_rateecertif peer_ratorcertif_rateecertif ib(12).rank_ratee ib(12).rank_rator i.yr i.dep if self == 0, cluster(rateecode) 
// Not reported-- less than 15% of individual-level observations obtain certificate, not enough to estimate so many interaction terms


**************************************
* PART II: Individual-level Analysis *
**************************************

*************************************
* Aggregate to the individual-level *
*************************************

use perfeval_final_A.dta, clear

* ratee_numratings
bysort yr dep rateecode (ratorcode): gen ratee_numratings = _N 
label var ratee_numratings "the total # of in-department ratings the ratee receives (incl self-rating)"
	*bysort yr dep rateecode (ratorcode): keep if _n == 1
	*The data is of very high quality in the sense that for people in the same dep and year, they receive the same number of ratings. 
	*That is, no asymmetry due to missing data.

* ratee_numratings_peers
bysort yr dep rateecode (ratorcode): egen ratee_numratings_peers = sum(peer)
label var ratee_numratings_peers "the # of ratings from peers (excl self-rating)"

* ratee_numratings_peers_ratio
gen ratee_numratings_peers_ratio = ratee_numratings_peers/ratee_numratings
label var ratee_numratings_peers_ratio "the percentage of ratings from peers"

* perfscore_avg
bysort yr dep rateecode (ratorcode): egen perfscore_avg = mean(perfscore)
label var perfscore_avg "the average score received (incl self-rating)"

* perfscore_avg_se
drop if self == 1
bysort yr dep rateecode (ratorcode): egen perfscore_avg_se = mean(perfscore)
label var perfscore_avg_se "the average score received (excl self-rating)"

* Collapse to individual level
bysort rateecode: keep if _n == 1 
drop *rator rator* self peer withindep
rename rateecode code

bysort yr dep: egen prank_dep_se = rank(perfscore_avg_se) //excl self-rating
bysort yr dep: egen prank_dep = rank(perfscore_avg) //incl self-rating
bysort yr dep rank_ratee: egen prank_peers_se = rank(perfscore_avg_se) //excl self-rating
bysort yr dep rank_ratee: egen prank_peers = rank(perfscore_avg) //incl self-rating

// default option of rank, which assign the same rank to tied values such that the sum of the trnaks is preserved

bysort yr dep: gen percentile_dep_se = (prank_dep_se - 1)/(_N-1)
bysort yr dep: gen percentile_dep = (prank_dep - 1)/(_N-1)
bysort yr dep rank_ratee: gen percentile_peers_se = (prank_peers_se - 1)/(_N-1) 
bysort yr dep rank_ratee: gen percentile_peers = (prank_peers - 1)/(_N-1)

save individual_A.dta, replace

**********************
* Promotion Analysis *
**********************

* Prepare the dataset, promotion_analysis.dta
use promotion-attributes-threshold-workload_10-14.dta, clear
merge 1:1 code using individual_A
keep if _merge == 3
drop _merge
gen promotion = .
replace promotion = 1 if postrank < prerank
replace promotion = 0 if prerank <= postrank

forvalues r = 2/12{
	gen rank`r' = (prerank == `r')
	gen certif_rank`r' = certif * rank`r'
	gen pass_rank`r' = P_4thresholds * rank`r'
}

gen rank_manager = (prerank < 8 & prerank > 2)
gen certif_rank_m = certif * rank_manager

gen percentile_i_pass = percentile_dep_se * P_4thresholds


saveold promotion_analysis.dta, replace

* Promotion analysis
use promotion_analysis.dta, clear



xi: logit promotion percentile_dep_se P_4thresholds##P_license certif_rank8 ib(12).prerank i.yr i.dep if 


xi: logit promotion percentile_dep_se P_4thresholds P_4thresholds#c.percentile_dep_se ib(12).prerank i.yr i.dep if rank != 8


xi: logit promotion perfscore_avg_se P_4thresholds certif_rank8 ib(12).prerank i.yr i.dep 


eststo promo_1: xi: logit promotion percentile_dep_se P_4thresholds certif certif_rank8 rank8 ib(12).prerank i.yr i.dep 
eststo promo_2: xi: logit promotion percentile_dep_se P_4thresholds certif pass_rank8 certif_rank8 rank8 ib(12).prerank i.yr i.dep 
esttab promo_1 promo_2 using county_level.tex, label replace booktabs title(Determinants of Promotion\label{tab1}) starlevels(* 0.10 ** 0.05 *** 0.01) se pr2

eststo promo_3: xi: logit promotion percentile_dep_se P_4thresholds certif_rank8 rank8 ib(12).prerank i.yr i.dep 
eststo promo_3: xi: logit promotion percentile_dep_se P_4thresholds   ib(12).prerank i.yr i.dep if rank != 8
eststo promo_3: xi: logit promotion percentile_dep_se P_4thresholds   ib(12).prerank i.yr i.dep 


eststo promo_3: xi: logit promotion percentile_dep_se P_4thresholds certif_rank9 certif_rank8 certif_rank_m certif ib(12).prerank i.yr i.dep 

*************************************
* PART III: Counterfactual Analysis *
*************************************

****************************************************************
* CS_self: Counterfactual scenario using only self-evaluations *
****************************************************************

use perfeval_final_A.dta, clear
bysort ratorcode: egen prank_self = rank(perfscore)
bysort ratorcode: egen totno_self = count(rateecode)
gen percentile_self = (prank_self - 1)/(totno_self - 1)
keep if self == 1
gen perfscore_self = perfscore
rename ratorcode code
keep code percentile_self perfscore_self
saveold CS_self, replace



use perfeval_final_A.dta, clear
bysort ratorcode: egen prank_self = rank(perfscore), track
bysort ratorcode: egen prank_self_N = max(prank_self)
gen percentile_self = (prank_self - 1)/(prank_self_N - 1)
keep if self == 1
gen perfscore_self = perfscore
rename ratorcode code
keep code percentile_self perfscore_self
saveold CS_self, replace

******************************************************************************
* CS_head: Counterfactual scenario using only department heads's evaluations *
******************************************************************************

use perfeval_final_A.dta, clear
keep if rator_head == 1
gen perfscore_head = perfscore
bysort yr dep rank_ratee: egen prank_peers_head = rank(perfscore_head), track
bysort yr dep rank_ratee: egen prank_peers_head_N = max(prank_peers_head)
bysort yr dep rank_ratee: gen percentile_peers_head = (prank_peers_head - 1)/(prank_peers_head_N - 1)

bysort yr dep: egen prank_dep_head = rank(perfscore_head), track
bysort yr dep: egen prank_dep_head_N = max(prank_dep_head)
bysort yr dep: gen percentile_dep_head = (prank_dep_head - 1)/(prank_dep_head_N - 1)

rename rateecode code
keep code perfscore_head percentile_peers_head percentile_dep_head prank_dep_head dep yr
saveold CS_head.dta, replace

***************************************************************
* CS_peer: Couterfactual scenario using only peer evaluations *
***************************************************************

use perfeval_final_A.dta, clear
postutil clear

levelsof(yr), local(l_yr)
levelsof(dep), local(l_dep)
levelsof(rank_ratee), local(l_rank)

tempname c_peer

postfile `c_peer' yr dep rank code cpeer_perfscore_avg_se cpeer_percentile_dep_se cpeer_percentile_peers_se using CS_peer.dta, replace 
// data generated will be stored in CS_peer.dta

foreach yr of local l_yr{
	foreach dep of local l_dep{
		foreach rank of local l_rank{
			display "yr:" `yr'
			display "dep:"`dep'
			display "rank:"`rank'

			* Keep only the peer group to aggregate peer group measures
			use perfeval_final_A.dta, clear
			keep if yr == `yr' & dep == `dep' & rank_rator == `rank' & self == 0
			
			if _N == 0{
				display "No records for the peer group: " `yr' "-" `dep' "-" `rank'
			}
			
			else{
				bysort yr dep rateecode (ratorcode): egen cpeer_perfscore_avg_se = mean(perfscore)
				bysort rateecode: keep if _n == 1
				bysort yr dep: egen cpeer_prank_dep_se = rank(cpeer_perfscore_avg_se) , track
				bysort yr dep: egen cpeer_prank_dep_se_N = max(cpeer_prank_dep_se) 

				bysort yr dep rank_ratee: egen cpeer_prank_peers_se = rank(cpeer_perfscore_avg_se), track
				bysort yr dep rank_ratee: egen cpeer_prank_peers_se_N = max(cpeer_prank_peers_se) 

				gen cpeer_percentile_dep_se = (cpeer_prank_dep_se-1)/(cpeer_prank_dep_se_N-1)
				bysort rank_ratee: gen cpeer_percentile_peers_se = (cpeer_prank_peers_se-1)/(cpeer_prank_peers_se_N-1)
			
				keep if rank_ratee == `rank'
				
				local num_obs = _N
				display "totalobs: " `num_obs'
			
				forvalues j = 1/`num_obs'{
					display "observation:" `j'
					post `c_peer' (yr[`j']) (dep[`j']) (rank_ratee[`j']) (rateecode[`j']) (cpeer_perfscore_avg_se[`j']) (cpeer_percentile_dep_se[`j']) (cpeer_percentile_peers_se[`j'])
			}
			}
		}
	}
}
postclose `c_peer'




*********************************************************************
* CS_nonpeer: Couterfactual scenario using only nonpeer evaluations *
*********************************************************************

use perfeval_final_A.dta, clear
drop if self == 1
drop if peer == 1
bysort yr dep rateecode (ratorcode): egen perfscore_avg_np = mean(perfscore)
bysort rateecode: keep if _n == 1
bysort yr dep: egen prank_dep_np = rank(perfscore_avg_np) 
bysort yr dep: egen prank_dep_np_N = max(prank_dep_np) 
bysort yr dep: gen percentile_dep_np = (prank_dep_np - 1)/(prank_dep_np_N -1)
rename rateecode code
keep code perfscore_avg_np percentile_dep_np
save CS_nonpeer.dta, replace




******************************************* 
* Couterfactual Analysis: Self-evaluation *
******************************************* 

* Merge counterfactual promotion probabilities
use promotion_analysis.dta, clear
merge 1:1 code using CS_head
drop _merge
merge 1:1 code using CS_peer
drop _merge
merge 1:1 code using CS_nonpeer
drop _merge
merge 1:1 code using CS_self
drop _merge
gen pct_self_premium = percentile_self - percentile_dep_se
save counterfactual.dta, replace

* Analyzing strategic behavior in self-evaluations
use counterfactual, clear

hist pct_self_premium, start(-1) w(0.025) fraction // Histogram of ÆPR_self
sum pct_self_premium percentile_self percentile_dep_se if pct_self_premium *percentile_self* percentile_dep_se != ., detail // Table 1

*************************************** 
* Couterfactual Analysis: Regressions *
***************************************


* CS_peer

use counterfactual, clear
xi: logit promotion percentile_dep_se P_4thresholds certif certif_rank8  ib(12).prerank i.yr i.dep
predict pr_prom_fitted 

gen that_original_peer = exp(_b[ P_4thresholds]* P_4thresholds + _b[percentile_dep_se]*percentile_dep_se + _b[certif]*certif + _b[certif_rank8]*certif_rank8	+ _b[8.prerank]* (prerank == 8)	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14  	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[4.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_original_peer  = 1/(1+1/that_original_peer)
gen that_c_peer = exp(_b[ P_4thresholds]* P_4thresholds + _b[percentile_dep_se]*cpeer_percentile_dep_se  + _b[certif]*certif	+ _b[certif_rank8]*certif_rank8  + _b[8.prerank]* (prerank == 8)	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14   	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[4.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])


gen p_c_peer  = 1/(1+1/that_c_peer)
gen p_c_peer_delta = p_c_peer - pr_prom_fitted 
scatter pr_prom_fitted p_original_peer // check the computed probability is correct
saveold p_c_peer, replace
xi: reg p_c_peer_delta P_4thresholds  certif  i.yr i.dep

xi: reg p_c_peer_delta P_4thresholds  certif certif_rank8  ib(12).prerank i.yr  i.dep


* CS_head_score
use counterfactual.dta, clear
xi: logit promotion perfscore_avg_se P_4thresholds certif certif_rank8  ib(12).prerank i.yr i.dep
predict pr_prom_fitted 

gen that_original_head = exp(_b[ P_4thresholds]* P_4thresholds + _b[perfscore_avg_se]*perfscore_avg_se  + _b[certif]*certif 	+ _b[certif_rank8]*certif_rank8	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14  	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[3.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[8.prerank]* (prerank == 8) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_original_head  = 1/(1+1/that_original_head)
gen that_c_head = exp(_b[ P_4thresholds]* P_4thresholds + _b[perfscore_avg_se]*perfscore_head 	  + _b[certif]*certif   + _b[certif_rank8]*certif_rank8	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14   	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[3.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[8.prerank]* (prerank == 8) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_c_head  = 1/(1+1/that_c_head)
gen p_c_head_delta = p_c_head - pr_prom_fitted 
scatter pr_prom_fitted p_original_head // check the computed probability is correct
saveold p_c_head, replace
xi: reg p_c_head_delta P_4thresholds certif certif_rank8 ib(12).prerank i.yr i.dep

* CS_head_pct
use counterfactual.dta, clear
xi: logit promotion percentile_dep_se P_4thresholds certif certif_rank8  ib(12).prerank i.yr i.dep
predict pr_prom_fitted 

gen that_original_head = exp(_b[ P_4thresholds]* P_4thresholds + _b[percentile_dep_se]*percentile_dep_se  + _b[certif]*certif 	+ _b[certif_rank8]*certif_rank8	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14  	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[3.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[8.prerank]* (prerank == 8) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_original_head  = 1/(1+1/that_original_head)
gen that_c_head = exp(_b[ P_4thresholds]* P_4thresholds + _b[percentile_dep_se]*percentile_dep_head 	  + _b[certif]*certif   + _b[certif_rank8]*certif_rank8	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14   	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[3.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[8.prerank]* (prerank == 8) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_c_head  = 1/(1+1/that_c_head)
gen p_c_head_delta = p_c_head - pr_prom_fitted 
scatter pr_prom_fitted p_original_head // check the computed probability is correct
saveold p_c_head, replace
xi: reg p_c_head_delta P_4thresholds certif certif_rank8   ib(12).prerank  i.yr  

* CS_nonpeer
use counterfactual.dta, clear
xi: logit promotion percentile_dep_se P_4thresholds certif certif_rank8 ib(12).prerank i.yr i.dep
predict pr_prom_fitted 

gen that_original_np = exp(_b[ P_4thresholds]* P_4thresholds + _b[percentile_dep_se]*percentile_dep_se 	+ _b[certif]*certif + _b[certif_rank8]*certif_rank8 	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14  	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[3.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[8.prerank]* (prerank == 8) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_original_np  = 1/(1+1/that_original_np)
gen that_c_np = exp(_b[ P_4thresholds]* P_4thresholds + _b[percentile_dep_se]*percentile_dep_np 	 + _b[certif]*certif  + _b[certif_rank8]*certif_rank8   	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14   	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[3.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[8.prerank]* (prerank == 8) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_c_np  = 1/(1+1/that_c_np)
gen p_c_np_delta = p_c_np - pr_prom_fitted 
scatter pr_prom_fitted p_original_np // check the computed probability is correct
saveold p_c_np, replace
xi: reg p_c_np_delta P_4thresholds certif certif_rank8   ib(12).prerank   i.yr i.dep

* CS_self
use counterfactual.dta, clear
xi: logit promotion percentile_dep_se P_4thresholds certif certif_rank8 ib(12).prerank i.yr i.dep
predict pr_prom_fitted 

gen that_original_self = exp(_b[ P_4thresholds]* P_4thresholds + _b[percentile_dep_se]*percentile_dep_se + _b[certif]*certif	+ _b[certif_rank8]*certif_rank8 	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14  	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[3.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[8.prerank]* (prerank == 8) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_original_self  = 1/(1+1/that_original_self)
gen that_c_self = exp(_b[ P_4thresholds]* P_4thresholds + _b[percentile_dep_se]*percentile_self   + _b[certif]*certif + _b[certif_rank8]*certif_rank8   	+  _b[_Iyr_11]*_Iyr_11 + _b[_Iyr_12]*_Iyr_12 + _b[_Iyr_13]*_Iyr_13 + _b[_Iyr_14]*_Iyr_14   	+ _b[_Idep_2]*_Idep_2 + _b[_Idep_3]*_Idep_3 + _b[_Idep_4]*_Idep_4 + _b[_Idep_5]*_Idep_5 + _b[_Idep_6]*_Idep_6 + _b[_Idep_7]*_Idep_7 + _b[3.prerank]* (prerank == 3) + _b[3.prerank]* (prerank == 4) + _b[5.prerank]* (prerank == 5) + _b[6.prerank]* (prerank == 6) + _b[7.prerank]* (prerank == 7) + _b[8.prerank]* (prerank == 8) + _b[9.prerank]* (prerank == 9) + _b[10.prerank]* (prerank == 10) + _b[11.prerank]* (prerank == 11) + _b[12.prerank]* (prerank == 12) + _b[_cons])
gen p_c_self  = 1/(1+1/that_c_self)
gen p_c_self_delta = p_c_self - pr_prom_fitted 
scatter pr_prom_fitted p_original_self // check the computed probability is correct
saveold p_c_self, replace
xi: reg p_c_self_delta P_4thresholds certif certif_rank8  ib(12).prerank  i.yr  
************************************************ 
* Couterfactual Analysis: Correlation Matrices *
************************************************

* Merging counterfactual changes of promotion probabilities
use counterfactual.dta, clear
merge 1:1 code using p_c_head
drop _merge
merge 1:1 code using p_c_peer
drop _merge
merge 1:1 code using p_c_np
drop _merge
merge 1:1 code using p_c_self
drop _merge
save counterfactual_delta.dta, replace

* Correlation Matrix: Average Ratings from Different Components
corrtex perfscore_avg_se perfscore_head cpeer_perfscore_avg_se perfscore_avg_np perfscore_self , file(myfile.tex) replace sig dig(4) nb case // Table 5

* Correlation Matrix: Counterfactual Changes of Promotion Probability
corrtex P_4thresholds p_c_head_delta p_c_peer_delta p_c_np_delta p_c_self_delta, file(myfile.tex) replace sig dig(4) nb case

pwcorr P_4thresholds p_c_head_delta p_c_peer_delta p_c_np_delta p_c_self_delta if P_4thresholds *p_c_head_delta *p_c_peer_delta *p_c_np_delta *p_c_self_delta != ., sig obs

  
pwcorr P_4thresholds p_c_head_delta p_c_peer_delta p_c_np_delta p_c_self_delta if P_4thresholds *p_c_head_delta *p_c_peer_delta *p_c_np_delta  != ., sig obs

*********************************
* Additional Summary Statistics *
*********************************

sum  perfscore if perfscore != . & P_4thresholds_rator != . & P_4thresholds_ratee != . & yr == 10, detail

sum  perfscore if perfscore != . & P_4thresholds_rator != . & P_4thresholds_ratee != . & yr == 11, detail

sum  perfscore if perfscore != . & P_4thresholds_rator != . & P_4thresholds_ratee != . & yr == 12, detail

sum  perfscore if perfscore != . & P_4thresholds_rator != . & P_4thresholds_ratee != . & yr == 13, detail

sum  perfscore if perfscore != . & P_4thresholds_rator != . & P_4thresholds_ratee != . & yr == 14, detail
