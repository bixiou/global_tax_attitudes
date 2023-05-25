
*** This do-file shows results of a wealth tax on the country of your choice ***

*** We strongly advise against using it on small countries, or countries that***
*** have few billionaires, as it is based on the Forbes correction.			 ***
*** It should therefore use the WID database AFTER the first correction    	 ***

*** The tax thresholds are in LOCAL 2021 CURRENCY, this differs from the     ***
*** simulator which was in 2021 USD MER for all regions						 ***


clear all

* R path
if "`c(os)'"=="MacOSX" | "`c(os)'"=="UNIX" {
    global Rpath "/usr/local/bin/R"
}
else {  // windows, change version number if necerssary
    global Rpath `"c:\r\R-3.5.1\bin\Rterm.exe"') 
}

global wid 			"~/Dropbox/W2ID/Latest_Updated_WID/wid-data"
global main 		"~/Dropbox/WID_WealthForbes"
global data 		"$main/data"
global work 		"$data/work-data"

* Country of choice
global iso 			FR

global year		= 	2021
global evasion 	=   15
global deprec	=	15

* Choice of tax rate is line 287

* Future improvements: leave choice of initial brackets here in global, loop on it in rsource. Choose tax rates in global

*------------------------------------------------------------------------------*


use $wid if inlist(widcode, "ahweal992j", "anninc992i", "npopul992i", "inyixx999i", "xlcusx999i") & iso == "$iso" & year == $year & p == "p0p100", clear
reshape wide value, i(iso year) j(widcode) string 
renvars value*, predrop(5)

drop p currency
ren ahweal992j avghweal

tempfile aggregates 
save `aggregates', replace

use $wid if inlist(widcode, "ahweal992j", "thweal992j") & year == $year & iso == "$iso", clear


merge m:1 iso year using `aggregates', nogen keep(matched)

reshape wide value, i(iso year p) j(widcode) string 
renvars value*, predrop(5)

generate long p_min = round(1000*real(regexs(1))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")
generate long p_max = round(1000*real(regexs(2))) if regexm(p, "^p([0-9\.]+)p([0-9\.]+)$")

generate n = round(p_max - p_min, 1)

keep if inlist(n, 1, 10, 100, 1000)
drop if n == 1000 & p_min >= 99000
drop if n == 100  & p_min >= 99900
drop if n == 10   & p_min >= 99990
drop p p_max currency
rename p_min p


foreach t in 1e5 2e5 5e5 1e6 2e6 5e6 1e7 2e7 5e7 1e8 2e8 5e8 1e9 2e9 5e9 8e6 1e10 2e10 5e10 1e11{
	gen t`t' = `t'
}
* Get billion USD threshold in LCU to compare to Forbes
gen t1e9US = 1e9*xlcusx999i

order iso year p n 
sort iso year p

drop if ahweal992j == . 

bys iso year (p): gen test = ah <= ah[_n-1] & _n!=1
drop if test 
drop test 
bys iso year (p): gen test = ah <= ah[_n-1] & _n!=1
drop if test 
drop test


save "$work/sample_$iso.dta", replace


*------------------------------------------------------------------------------*

* Use Gpinter R package to estimate finer brackets than provided in the world inequality database.

rsource, terminator(END_OF_R) rpath("$Rpath") roptions(`" --vanilla --args $iso "')

rm(list = ls())

library(pacman)
p_load(magrittr)
p_load(dplyr)
p_load(readr)
p_load(haven)
p_load(tidyr)
p_load(gpinter)
p_load(purrr)
p_load(stringr)
p_load(ggplot2)
p_load(glue)
p_load(progress)
p_load(zoo)
p_load(ggrepel)
p_load(countrycode)
options(dplyr.summarise.inform = FALSE)

iso <-commandArgs(trailingOnly=TRUE)

setwd("~/Dropbox/WID_WealthForbes/data/work-data")
data <- read_dta(paste("sample_",iso[[1]],".dta",sep=""))

i<-1
gperc <- c(
  seq(0, 99000, 1000), seq(99100, 99900, 100),
  seq(99910, 99990, 10), seq(99991, 99999, 1)
)

countrybrackets <- data[!is.na(data$ahweal992j),] %>% group_by(year) %>% group_split() %>% map_dfr(~ {
	
	dist <- shares_fit(
	  average = .x$avghweal[1],
	  bracketavg = .x$ahweal992j,
	  p = .x$p/1e5,
	  fast = TRUE
	)
    
    return(as.data.frame(
      generate_tabulation(dist, gperc/1e5)) 
      %>% mutate(year = .x$year[1],
                 avghweal = .x$avghweal[1],
                 average = dist$average[1],
                 f1e5 = 1 - fitted_cdf(dist, .x$t1e5[1]),
                 w1e5 = top_average(dist, 1 - f1e5),
                 f2e5 = 1 - fitted_cdf(dist, .x$t2e5[1]),
                 w2e5 = top_average(dist, 1 - f2e5),
                 f5e5 = 1 - fitted_cdf(dist, .x$t5e5[1]),
                 w5e5 = top_average(dist, 1 - f5e5),
                 f1e6 = 1 - fitted_cdf(dist, .x$t1e6[1]),
                 w1e6 = top_average(dist, 1 - f1e6),
                 f2e6 = 1 - fitted_cdf(dist, .x$t2e6[1]),
                 w2e6 = top_average(dist, 1 - f2e6),
                 f5e6 = 1 - fitted_cdf(dist, .x$t5e6[1]),
                 w5e6 = top_average(dist, 1 - f5e6),
                 f1e7 = 1 - fitted_cdf(dist, .x$t1e7[1]),
                 w1e7 = top_average(dist, 1 - f1e7),
                 f2e7 = 1 - fitted_cdf(dist, .x$t2e7[1]),
                 w2e7 = top_average(dist, 1 - f2e7),
                 f5e7 = 1 - fitted_cdf(dist, .x$t5e7[1]),
                 w5e7 = top_average(dist, 1 - f5e7),
                 f1e8 = 1 - fitted_cdf(dist, .x$t1e8[1]),
                 w1e8 = top_average(dist, 1 - f1e8),
                 f2e8 = 1 - fitted_cdf(dist, .x$t2e8[1]),
                 w2e8 = top_average(dist, 1 - f2e8),
                 f5e8 = 1 - fitted_cdf(dist, .x$t5e8[1]),
                 w5e8 = top_average(dist, 1 - f5e8),
                 f1e9 = 1 - fitted_cdf(dist, .x$t1e9[1]),
                 w1e9 = top_average(dist, 1 - f1e9),
                 f2e9 = 1 - fitted_cdf(dist, .x$t2e9[1]),
                 w2e9 = top_average(dist, 1 - f2e9),
                 f5e9 = 1 - fitted_cdf(dist, .x$t5e9[1]),
                 w5e9 = top_average(dist, 1 - f5e9),
				 f8e6 = 1 - fitted_cdf(dist, .x$t8e6[1]),
                 w8e6 = top_average(dist, 1 - f8e6),
                 f1e10 = 1 - fitted_cdf(dist, .x$t1e10[1]),
                 w1e10 = top_average(dist, 1 - f1e10),
                 f2e10 = 1 - fitted_cdf(dist, .x$t2e10[1]),
                 w2e10 = top_average(dist, 1 - f2e10),
                 f5e10 = 1 - fitted_cdf(dist, .x$t5e10[1]),
                 w5e10 = top_average(dist, 1 - f5e10),
                 f1e11 = 1 - fitted_cdf(dist, .x$t1e11[1]),
                 w1e11 = top_average(dist, 1 - f1e11),
                 f1e9US = 1 - fitted_cdf(dist, .x$t1e9US[1]),
                 w1e9US = top_average(dist, 1 - f1e9US))
      %>% select(year,f1e5,f2e5,f5e5,f1e6,f2e6,f5e6,f1e7,f2e7,f5e7,f1e8,f2e8,f5e8,f1e9,f2e9,f5e9,f8e6,f1e10,f2e10,f5e10,f1e11,f1e9US,w1e5,w2e5,w5e5,w1e6,w2e6,w5e6,w1e7,w2e7,w5e7,w1e8,w2e8,w5e8,w1e9,w2e9,w5e9,w8e6,w1e10,w2e10,w5e10,w1e11,w1e9US)
    ) %>% distinct()
  })
countrybrackets$iso <- iso
countrybrackets <- distinct(countrybrackets)

write_dta(countrybrackets, paste("sample2_",iso[[1]],".dta",sep=""))

END_OF_R


*------------------------------------------------------------------------------*


* Import aggregates from WID
use if inlist(widcode, "ahweal992j", "anninc992i", "npopul992i", "inyixx999i", "xlcusx999i") & year == $year & iso == "$iso" & p == "p0p100" using $wid, clear
reshape wide value, i(iso year) j(widcode) string 
renvars value*, predrop(5)

drop p currency

gen mnninc999i = anninc992i*npopul992i
gen mhweal999i = ahweal992j*npopul992i


* Get thresholds from gpinter and Forbes wealth
gen p = 0
merge 1:1 iso year   using "$work/sample2_$iso.dta", nogen keep(matched)
merge 1:1 iso year p using "$work/wealth-distributions-matched-forbes.dta", keep(master matched) nogen keepusing(worth nb)
drop p

recode worth nb (mis=0)
ren (worth nb) (w_forbes n_forbes)
replace w_forbes = w_forbes*1e6*xlcusx999i


* Make wealth group 
foreach t in 1e5 2e5 5e5 1e6 2e6 5e6 1e7 2e7 5e7 1e8 2e8 5e8 1e9 2e9 5e9 8e6 1e10 2e10 5e10 1e11 1e9US{
	replace w`t' = f`t'*npopul992i*w`t'
	gen n`t'     = f`t'*npopul992i
	
	clonevar n`t'i = n`t' // to keep track of initial values
	clonevar w`t'i = w`t'
}

* Generate Forbes ratio
gen ratio_n = .
gen ratio_w = .
 
replace ratio_n = n_forbes/n1e9US
replace ratio_w = min(w_forbes/w1e9US,w1e7/w1e9)


foreach c in n w{
	
	replace `c'1e9US = `c'1e9US*ratio_`c'
	
* Adjust numbers above 1 billion USD (this is the actual correction)

	foreach t in 1e7 2e7 5e7 1e8 2e8 5e8 1e9 2e9 5e9 8e6 1e10 2e10 5e10 1e11{
		replace `c'`t' = `c'`t'*ratio_`c'/(log(`t'/(1e9*xlcusx999i))*0.35+1) if "`c'"=="w" & `t'/xlcusx999i>0.999*1e9 
		
*the log factor is here to ensure that the applied ratio is lower the higher the bracket, power law

		replace `c'`t' = `c'`t'*ratio_`c' if "`c'"=="n" & `t'/xlcusx999i>0.999*1e9 
		replace `c'`t' = 0 if ratio_`c' == 0
	}
}

foreach t in 1e5 2e5 5e5 1e6 2e6 5e6 1e7 2e7 5e7 1e8 2e8 5e8 1e9 2e9 5e9 8e6 1e10 2e10 5e10{
	replace n`t' = round(n`t')
	replace w`t' = 0 if n`t'==0
}

replace n1e11 = 0 if n1e11<.5
replace w1e11 = 0 if n1e11<.5
replace n1e11 = ceil(n1e11)
replace n1e11 = round(w1e11/1e9/110) if iso == "US"

foreach c in n w{
	* Ensure there are no increasing top wealth or numbers of individuals 
	replace `c'5e8 = `c'5e8 + 1.1*(`c'1e9-`c'5e8) if `c'1e9-`c'5e8>0
	replace `c'2e8 = `c'2e8 + 1.1*(`c'5e8-`c'2e8) if `c'5e8-`c'2e8>0
	replace `c'1e8 = `c'1e8 + 1.1*(`c'2e8-`c'1e8) if `c'2e8-`c'1e8>0
	replace `c'5e7 = `c'5e7 + 1.1*(`c'1e8-`c'5e7) if `c'1e8-`c'5e7>0
	replace `c'2e7 = `c'2e7 + 1.1*(`c'5e7-`c'2e7) if `c'5e7-`c'2e7>0
	replace `c'1e7 = `c'1e7 + 1.1*(`c'2e7-`c'1e7) if `c'2e7-`c'1e7>0
	replace `c'5e6 = `c'5e6 + 1.1*(`c'1e7-`c'5e6) if `c'1e7-`c'5e6>0
	replace `c'2e6 = `c'2e6 + 1.1*(`c'5e6-`c'2e6) if `c'5e6-`c'2e6>0
	replace `c'1e6 = `c'1e6 + 1.1*(`c'2e6-`c'1e6) if `c'2e6-`c'1e6>0
	replace `c'5e5 = `c'5e5 + 1.1*(`c'1e6-`c'5e5) if `c'1e6-`c'5e5>0
	replace `c'2e5 = `c'2e5 + 1.1*(`c'5e5-`c'2e5) if `c'5e5-`c'2e5>0
// 	replace `c'1e5 = `c'1e5 + 1.1*(`c'2e5-`c'1e5) if `c'2e5-`c'1e5>0
}


* Test if distribution impacted outside of the 99.999% percentile
foreach t in 2e6 5e6 1e7 2e7 5e7 1e8 2e8 5e8 1e9{
	cap drop testf testw 
	quietly gen testf = f`t'>1e-5			   //group outside of perc-127
	quietly gen testw = abs(w`t'/w`t'i-1)>.01  //wealth has changed more than 1%
	quietly gen test`t' = testf*testw
	di "`t'"
	tab iso year if test`t' 
}
drop test* n1*i n2*i n5*i n8*i w*i f* ratio* ahweal992j 

drop n1e9US w1e9US

* tax rates
local r1e5		0
local r2e5		0
local r5e5		0
local r1e6		0
local r2e6		0
local r5e6		0
local r8e6		1
local r1e7		1
local r2e7		1
local r5e7		2
local r1e8		2
local r2e8		2
local r5e8		2
local r1e9		3
local r2e9		3
local r5e9		3
local r1e10		3
local r2e10		3
local r5e10		3
local r1e11		3

foreach t in 1e5 2e5 5e5 1e6 2e6 5e6 1e7 2e7 5e7 1e8 2e8 5e8 1e9 2e9 5e9 8e6 1e10 2e10 5e10 1e11{
	gen rate`t' = `r`t''
}

reshape long f w n rate, i(iso year) j(threshold_ 1e5 2e5 5e5 1e6 2e6 5e6 1e7 2e7 5e7 1e8 2e8 5e8 1e9 2e9 5e9 8e6 1e10 2e10 5e10 1e11)
destring(threshold_), gen(threshold)
sort iso year threshold

bys iso year (threshold): gen wb = w-w[_n+1] if _n!=_N
replace wb = w if wb == .
bys iso year (threshold): gen nb = n-n[_n+1] if _n!=_N
replace nb = n if nb == .

keep iso year threshold mnninc999i mhweal999i w wb n nb w_forbes n_forbes rate

collapse (min) threshold (sum) nb wb, by(iso year mnninc mhweal rate)

* Compute tax revenues
gen revenue = (wb-nb*threshold)*(rate/100)
local N = _N-1
forvalues i = 1/`N'{
	gen lag`i' = nb*(threshold[_n-`i'+1]-threshold[_n-`i'])*(rate[_n-`i']/100)
	recode lag`i' (mis=0)
	replace revenue = revenue + lag`i'
}
drop lag*

replace thresho = . if _n == 1
replace rate    = . if _n == 1
foreach v in nb wb revenue{
	replace `v' = 0 if _n == 1
	local N = _N-1
	forvalues i = 1/`N'{
		replace `v' =  `v'+`v'[_n+`i'] if _n == 1
	}
}

gen 	effrate = revenue*(1-$evasion /100)/wb
replace revenue = revenue*(1-$evasion /100)*(1-$deprec /100)

gen 	pct_inc = revenue/mnninc999i
gen 	pct_wea = revenue/mhweal999i
replace revenue = revenue/1e9
replace rate 	= rate/100
replace wb 		= round(wb/1e9)

tostring(threshold), replace
gen group = ""
replace group = substr(threshold,1,1)+"00k" if strlen(threshold)==6
replace group = substr(threshold,1,1)+"m"   if strlen(threshold)==7
replace group = substr(threshold,1,1)+"0m"  if strlen(threshold)==8
replace group = substr(threshold,1,1)+"00m" if strlen(threshold)==9
replace group = substr(threshold,1,1)+"b"   if strlen(threshold)==10
replace group = substr(threshold,1,1)+"0b"  if strlen(threshold)==11
replace group = substr(threshold,1,1)+"00b" if strlen(threshold)==12

replace group = "All above " + group[_n+1] if _n==1
replace group = group + "-" + group[_n+1] if _n!=_N & _n!=1
replace group = "+" + group if _n==_N


keep  iso year group nb wb rate effrate revenue pct_inc pct_wea
order iso year group nb wb rate effrate revenue pct_inc pct_wea
label var iso 	"Country code"
label var year 	"Year"
label var group "Wealth group"
label var nb 	"N. individuals"
label var wb 	"Bracket wealth (€ bn)"
label var rate 	"Marginal tax rate"
label var effr 	"Eff. tax rate"
label var rev 	"Revenue (€ bn)"
label var pct_i	"Revenue (% Nat. Inc.)"
label var pct_w "Revenue (% HH Wealth)"

* Export 
save "$work/tax_$iso.dta", replace
export excel using "$work/tax_$iso.xlsx", sheet("Sheet1")  sheetmod first(varl)
putexcel set "$work/tax_$iso.xlsx", modify sheet("Sheet1")
putexcel (F2:G5), nformat(percent)
putexcel (I2:J5), nformat(percent)
