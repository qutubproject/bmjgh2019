* Qutub Project
* Title: The use of standardized patients for health care quality research in low- and middle-income countries
* Journal: BMJ Global Health

* Created/edited by: Ada Kwan
* Created on: 4 April 2019
* Last edited on: 13 September 2019


// SETTING UP
set more off
cd "~/bmjgh2019/"


// Run .ado files in "~/bmjgh2019/ado/"
	* betterbar
	* betterbarci
	* chartable
	* iemargins

	
// FIGURE 1: Weighting
// What is average provider quality versus what is the quality an average patient receives?
use "data/patna_baseline.dta", clear
* cp_19 correct-med_k_any_6 c2 med_k_any_6 re_3 re_1 c2 correct

  expand 2, gen(weighted)
    lab def weighted 0 "Unweighted" 1 "Weighted"
    lab val weighted weighted

  clonevar weight = cp_19
  replace weight = 1 if weighted == 0
  gen c2 = max(correct - med_k_any_6 , 0)
    label var c2 "Correct, No Antibiotics"

  betterbar med_k_any_6 re_3 re_1 c2 correct  ///
    [pweight = weight] , over(weighted) ci xlab(0 "0%" 0.2 "20%"  0.4 "40%" 0.6 "60%" 0.8 "80%" 1 "100%") ///
	legend(on rows(2) order(1 "Average Provider Quality (Unweighted)" 2 "Average Quality Received by Patients (Weighted)") size(small)) graphregion(color(white))
	
	graph save "outputs/figure_1.gph", replace			
	graph export "outputs/figure_1.tif" , replace
	graph export "outputs/figure_1.png" , replace width(1000)	

	
// FIGURE 2: Caseload-quality
// Do care dimensions vary across number of patients in the waiting room?
use "data/daniels2017_nairobi", clear

  gen dow = dow(sp_date)
  keep if sp_cp11 <60

  egen check = rsum(sp?_h? sp?_h??)

  reg check c.sp_cp11##c.sp_cp11
    predict ord

  areg check c.sp_cp11##c.sp_cp11, a(facilitycode)
    predict fe

  tw (histogram sp_cp11, width(2) yaxis(2) fc(gs12) lc(white)) ///
     (lowess ord sp_cp11, lpattern(dash) lc(navy) lw(thick)) ///
     (lowess fe sp_cp11, lc(navy) lw(thick)) ///
  , xtit("Patients Waiting in Facility") ytit("History Questions Asked") ytit(, axis(2)) ytit(, axis(1)) graphregion(color(white)) ///
	yscale(range(0 6)) ylab(, angle(0) axis(2)) yscale(off alt axis(2)) yscale(alt) ylab(,angle(0) nogrid) ///
	legend(on order(2 "Unadjusted Relationship in Data" 3 "Adjusted for Facility Averages") size(small))
	
	graph save "outputs/figure_2.gph", replace			
	graph export "outputs/figure_2.tif" , replace
	graph export "outputs/figure_2.png" , replace width(1000)	


// FIGURE 3: Morning-evening
// Does quality vary across operating hours?
use "data/mumbai_baseline.dta", clear

	keep if qutub_evening != .

	expand 2, gen(false2)
		replace case = 5 if false2 == 1

	la var correct "Correct"
	la def case 1 "Case 1" 2 "Case 2" 3 "Case 3" 4 "Case 4" 5 "Pooled", replace
  
	ttest correct if case==1, by(qutub_evening)
	ttest correct if case==2, by(qutub_evening)
	ttest correct if case==3, by(qutub_evening)
	ttest correct if case==4, by(qutub_evening)
	ttest correct, by(qutub_evening)

	betterbarci correct , ///
		over(qutub_evening) by(case) xlab(0 "0%" 0.2 "20%"  0.4 "40%" 0.6 "60%" 0.8 "80%" 1 "100%") ///
		legend(on rows(2) order(1 "Morning" 2 "Evening") size(small)) graphregion(color(white))
	
	graph save "outputs/figure_3.gph", replace			
	graph export "outputs/figure_3.tif" , replace
	graph export "outputs/figure_3.png" , replace width(1000)	


// FIGURE 4
// To what extent does provider knowledge differ from actual practice?
*** run in R: bmjgh/replication/rfiles/bmjgh_v1.Rnw using know_do_means.csv


// FIGURE 5: Empowered
// Can patient empowerment influence quality of care or other outcomes?
use "data/mumbai_midline.dta", clear

	replace re_1 = 0 if case == 1 & re_1 == .
	replace cp_12 = "11" if cp_12 == "SP11"
		replace cp_12 = "12" if cp_12 == "SP12"
		replace cp_12 = "14" if cp_12 == "SP14"
		replace cp_12 = "15" if cp_12 == "SP15"
		replace cp_12 = "21" if cp_12 == "SP21"
		replace cp_12 = "22" if cp_12 == "SP22"
		replace cp_12 = "31" if cp_12 == "SP31"
		replace cp_12 = "32" if cp_12 == "SP32"
		replace cp_12 = "41" if cp_12 == "SP41"
		replace cp_12 = "43" if cp_12 == "SP43"
	replace cp_12 = cp_12_1 if (cp_12=="" & cp_12_1 !="") | (cp_12_1=="43" & cp_12_2=="43")
	gen spid = cp_12
		la var spid "cp12. SP ID"

	* New variables
		gen ppia_wave0 = 0
			replace ppia_wave0 = 1 if facility_ppia_wave0 == 1 | provider_ppia_wave0 == 1 
		gen ppia_wave1 = 0
			replace ppia_wave1 = 1 if facility_ppia_wave1 == 1 | provider_ppia_wave1 == 1 // in program at time of interaction to give voucher
		gen ppia = 0
			replace ppia = 1 if ppia_wave0 == 1 | ppia_wave1 == 1

	* Results (eligibility, balance, results)
		* eligibility	
		keep if dr_5x != . // Must have been assigned tapped
			
			
	* "CONSORT"
		* eligibility
		ta case ppia
		
		* randomization
		ta case dr_5x if ppia == 1
		
		* received cxr
		ta case re_1 if dr_5x == 0 & ppia == 1
		ta case re_1 if dr_5x == 1 & ppia == 1
		
		* received voucher
		ta dr_5b re_1 if dr_5x == 0 & ppia == 1
		ta dr_5b re_1 if dr_5x == 1 & ppia == 1
		

	* deselect 1,2,3 depending on ppia provider status
		** 1. among all providers
		** 2. among ppia providers
			keep if ppia == 1 // & (cp_5_qual == 1 | cp_5_qual == 3)	
		** 3. among non-ppia providers
			*keep if ppia_wave1 == 0 // & (cp_5_qual == 1 | cp_5_qual == 3)	
		

	** REGARDLESS OF ORDERING CXR
	// Balance
			* no clustered SE
			forest reg                        ///
				correct re_1 re_3 re_4               ///
				med_any med_l_any_1 med_l_any_2 ///
				med_l_any_3 med_k_any_9         ///
				dr_1 dr_4 = dr_5x              ///
			, c(i.case i.sample i.cp_5_qual) bonferroni
			*graph save "outputs/fig_tapped_balance_nocluster.gph", replace			
			*graph export "outputs/fig_tapped_balance_nocluster.tif" , replace
			*graph export "outputs/fig_tapped_balance_nocluster.png" , replace width(1000)	

			* clustered SE
			forest reg                        ///
				correct re_1 re_3 re_4               ///
				med_any med_l_any_1 med_l_any_2 ///
				med_l_any_3 med_k_any_9         ///
				dr_1 dr_4 = dr_5x              ///
			, c(i.case i.sample i.cp_5_qual) cluster(spid) bonferroni
			
			*graph save "outputs/fig_tapped_balance_cluster.gph", replace			
			*graph export "outputs/fig_tapped_balance_cluster.tif" , replace
			*graph export "outputs/fig_tapped_balance_cluster.png" , replace width(1000)	

	** CONDITIONED ON ORDERING CXR	
	// Result
		keep if re_1 == 1

			
		* tapped2 conditioned means, using https://github.com/bbdaniels/iemargins
			separate dr_5b, by(case)
				la var dr_5b1 "CXR Voucher for Case 1"
				la var dr_5b2 "CXR Voucher for Case 2"
				la var dr_5b3 "CXR Voucher for Case 3"
				la var dr_5b4 "CXR Voucher for Case 4"

			* cluster
			iemargins dr_5b? dr_5b ///
			, ycom treatment(dr_5x) ///
				c(i.case i.sample i.cp_5_qual) cluster(spid) combine(rows(1)) ///
				graphoptions( ylab(0 "0%" 0.2 "20%"  0.4 "40%" 0.6 "60%" 0.8 "80%" 1 "100%"))

			graph save "outputs/figure_5.gph", replace			
			graph export "outputs/figure_5.tif" , replace
			graph export "outputs/figure_5.png" , replace width(1000)	
