cd /Users/rnj/DWork/GitHub/HRS_cog_classification/Stata/
use /Users/rnj/Library/CloudStorage/Dropbox/Work/HCAP23/POSTED/ANALYSIS/Integrated_Analysis_fork_EAP/w073_hcapdx.dta, clear

keep hhid pn n21 n22 
merge 1:1 hhid pn using ../R_objects/025_hrs16_cog.dta , nogen

* Pre-imputation cognitive scores 
preserve 
clear
use /Users/rnj/Library/CloudStorage/Dropbox/Work/HCAP23/POSTED/ANALYSIS/Integrated_Analysis_fork_EAP/w051-preimputation.dta
keep hhid pn Tgcp Th1rmsetotal 
tempfile f1
save `f1'
restore 

merge 1:1 hhid pn using `f1' , nogen
contents 

corr tf Tgcp
local Nis=`r(N)'
local rhois  : di %3.2f `r(rho)'
local noteis "N = `Nis', pre-imputation data. Correlation = `rhois'"
gr tw scatter tf Tgcp , aspect(1) ylab(-30 0 50 100 130) xlab(-30 0 50 100 130) msize(tiny) ms(o) mc(black*.70%40) ytitle(HRS/Core GCP (EAP)) xtitle(HRS/HCAP GCP (EAP)) note("`noteis'")
gr export fig1.png , width(2000) replace 
gr close 

baplot tf Tgcp
gr export fig2.png , width(2000) replace 
gr close 

gen CogImp = n21*2+(n22==1)
checkvar CogImp n21 n22 
la def CogImp 0 "No domains" 1 "1 domain" 2 "2+ domains" 
la val CogImp CogImp 

gr box Tgcp , over(CogImp) ytitle(HRS/HCAP GCP (EAP))
gr export fig3.png , width(2000) replace 
gr box tf , over(CogImp) ytitle(HRS/Core GCP (EAP))
gr export fig4.png , width(2000) replace 
gr close 

tab CogImp if missing(tf)~=1

centile tf if missing(CogImp)~=1 & missing(tf)~=1, centile(15 35)
local p15 = r(c_1) // 17th percentile
local p35 = r(c_2) // 38th percentile


// Display the macros
display "15th percentile: `p15'"
display "35th percentile: `p35'"

cap drop tfCogImp 
gen tfCogImp = .
replace tfCogImp = 0 if tf >= `p35' // At or above the 35th percentile
replace tfCogImp = 1 if tf < `p35' & tf > `p15' // Between the 15th and 35th percentiles
replace tfCogImp = 2 if tf <= `p15' // Equal to or below the 15th percentile
replace tfCogImp = . if missing(tf)
tab CogImp tfCogImp
kap CogImp tfCogImp
*ssc install kappaetc
kappaetc CogImp tfCogImp, wgt(quadratic)
