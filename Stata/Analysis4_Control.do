version 18.0

capture which baplot
if _rc {
    di as error "Required Stata command not installed: baplot"
    di as error "Install it before running this workflow."
    exit 199
}

capture which checkvar
if _rc {
    di as error "Required Stata command not installed: checkvar"
    di as error "Install it before running this workflow."
    exit 199
}

capture which kappaetc
if _rc {
    di as error "Required Stata command not installed: kappaetc"
    di as error "Install it before running this workflow."
    exit 199
}

use "Stata/20240228-040.dta", clear

keep hhid pn n21 n22
merge 1:1 hhid pn using "R_objects/025_hrs16_cog.dta", nogen

preserve
clear
use "Stata/w051-preimputation.dta", clear
keep hhid pn Tgcp Th1rmsetotal
tempfile f1
save `f1'
restore

merge 1:1 hhid pn using `f1', nogen
contents

corr tf Tgcp
local Nis = `r(N)'
local rhois : di %3.2f `r(rho)'
local noteis "N = `Nis', pre-imputation data. Correlation = `rhois'"

twoway scatter tf Tgcp, ///
    aspect(1) ///
    ylab(-30 0 50 100 130) ///
    xlab(-30 0 50 100 130) ///
    msize(tiny) ///
    msymbol(o) ///
    mcolor(black*.70%40) ///
    ytitle("HRS/Core GCP (EAP)") ///
    xtitle("HRS/HCAP GCP (EAP)") ///
    note("`noteis'")
graph export "Figures/Stata_Ad_Hoc_fig1.png", width(2000) replace
graph close

baplot tf Tgcp
graph export "Figures/Stata_Ad_Hoc_fig2.png", width(2000) replace
graph close

generate CogImp = n21 * 2 + (n22 == 1)
checkvar CogImp n21 n22
label define CogImp 0 "No domains" 1 "1 domain" 2 "2+ domains"
label values CogImp CogImp

graph box Tgcp, over(CogImp) ytitle("HRS/HCAP GCP (EAP)")
graph export "Figures/Stata_Ad_Hoc_fig3.png", width(2000) replace

graph box tf, over(CogImp) ytitle("HRS/Core GCP (EAP)")
graph export "Figures/Stata_Ad_Hoc_fig4.png", width(2000) replace
graph close

tab CogImp if missing(tf) != 1

centile tf if missing(CogImp) != 1 & missing(tf) != 1, centile(15 35)
local p15 = r(c_1)
local p35 = r(c_2)

display "15th percentile: `p15'"
display "35th percentile: `p35'"

capture drop tfCogImp
generate tfCogImp = .
replace tfCogImp = 0 if tf >= `p35'
replace tfCogImp = 1 if tf < `p35' & tf > `p15'
replace tfCogImp = 2 if tf <= `p15'
replace tfCogImp = . if missing(tf)

tab CogImp tfCogImp
kap CogImp tfCogImp
kappaetc CogImp tfCogImp, wgt(quadratic)