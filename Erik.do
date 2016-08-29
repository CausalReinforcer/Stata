* Resolving Erik's Suggestion

use data_erik.dta, clear

drop if younger == "Missing" | younger == "Military"
drop if older == "Missing" | older == "Military"

bysort prov_id: gen prov_id_count = _N 
drop if prov_id_count == 1 // Now, it has 1820 * 2 = 3640 obs

foreach var of varlist younger older{
	gen `var'_n = .
	replace `var'_n = 0 if `var' == "None"
	replace `var'_n = 1 if `var' == "Purchased"
	replace `var'_n = 2 if `var' == "Prefectural"
	replace `var'_n = 3 if `var' == "Prov&Nat"
}

gen transition_n = .
replace transition_n = 0 if transition == "GGF_GF"
replace transition_n = 1 if transition == "GF_F"

egen prov_id_n = group(prov_id)

gen older_none = 0
replace older_none = 1 if older_n == 0

gen older_purchased = 0
replace older_purchased = 1 if older_n == 1

gen older_pref = 0
replace older_pref = 1 if older_n == 2

gen older_prov = 0
replace older_prov = 1 if older_n == 3

gen older_none_tran = older_none * transition_n
gen older_purchased_tran = older_purchased * transition_n
gen older_pref_tran = older_pref * transition_n
gen older_prov_tran = older_prov * transition_n

gen 



*ologit younger_n i.older_n#i.transition_n, vce(cluster prov_id)

eststo clear
eststo base:  ologit younger_n older_purchased older_pref older_prov  transition_n, vce(cluster prov_id)
eststo inter: ologit younger_n older_none_tran older_purchased older_purchased_tran older_pref older_pref_tran older_prov older_prov_tran, vce(cluster prov_id)
esttab base inter using county_level.tex, label replace booktabs title(Robustness) starlevels(* 0.10 ** 0.05 *** 0.01) se pr2



prvalue , x(transition_n=0) save
prvalue , x(transition_n=1) dif



ologit younger_n i.older_n#i.transition_n
predict p_none p_purchased p_pref p_prov


margins, at(transition_n=0) 
margins, predict(outcome(1)) at(transition_n=0) atmeans post
margins, predict(outcome(2)) at(transition_n=0) atmeans post
margins, predict(outcome(3)) at(transition_n=0) atmeans post

margins, predict(outcome(0)) at(transition_n=1) atmeans post
margins, predict(outcome(1)) at(transition_n=1) atmeans post
margins, predict(outcome(2)) at(transition_n=1) atmeans post
margins, predict(outcome(3)) at(transition_n=1) atmeans post


ologit younger_n older_n#transition_n, vce(cluster prov_id) 

ologit younger_n older_n#transition_n i.year_prov

mlogit younger_n i.older_n transition_n, vce(cluster prov_id) 
