clear
//clear matrix
set mem 80m
set more off
set matsize 800
capture log close

local citylist "d_Chengdu d_Chongqing d_Haerbin d_Hefei"

cd "E:\MyPapers\happiness\TWINSDATA"
//cd "I:\MyPapers\happiness\TWINSDATA"
use all_twins.dta, clear

cd "E:\MyPapers\happiness\TWINSDATA\result\Nov2017Edu"
//cd "I:\MyPapers\happiness\TWINSDATA\result\Feb2017"

log using happiness_4scale_table.txt, replace


//Draw Figure 1
//egen bins=cut(income_num),at(0(100)30000)
//replace bins=bins+25
drop if income==0

bysort edu: egen happiness_4scale_mean=mean(happiness_4scale)
twoway (scatter happiness_4scale_mean edu) (lfit happiness_4scale edu)(qfit happiness_4scale edu), ///
ytitle(Mean happiness_4scale) xtitle(Education) saving(Meanhappiness_4scale_Edu.tif, replace) legend(label(1 "Mean happiness") label(2 "Linear fit") label(3 "Quadratic fit")) 

drop happiness_4scale_mean


//Table 1: income bracket and happiness matrix
tab f15_3
tab f15_3  edu

gen tenure_squared = tenure*tenure

//Table 2: summary

sum happiness_4scale income_num age agesquared male birth_rate VeryEarly_disease_d edu married health work_over_40h


quiet reg happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d `citylist',robust cluster(twinpair)
outreg2 using Table3_edu_happiness4scale.doc,nolabel replace coefastr se

quiet reg happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income   `citylist',robust cluster(twinpair)
outreg2 using Table3_edu_happiness4scale.doc,nolabel append coefastr se

quiet reg happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income married health   `citylist',robust cluster(twinpair)
outreg2 using Table3_edu_happiness4scale.doc,nolabel append coefastr se

quiet reg happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income married health  work_over_40h `citylist',robust cluster(twinpair)
outreg2 using Table3_edu_happiness4scale.doc,nolabel append coefastr se


//order probit
quiet oprobit happiness_4scale edu age agesquared male `citylist',robust cluster(twinpair)
outreg2 using TableA1_edu_oprobit_happiness4scale.doc,  nolabel replace coefastr se

quiet oprobit happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d `citylist',robust cluster(twinpair)
outreg2 using TableA1_edu_oprobit_happiness4scale.doc,  nolabel append coefastr se

quiet oprobit happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income `citylist',robust cluster(twinpair)
outreg2 using TableA1_edu_oprobit_happiness4scale.doc,  nolabel append coefastr se

quiet oprobit happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income married `citylist',robust cluster(twinpair)
outreg2 using TableA1_edu_oprobit_happiness4scale.doc,  nolabel append coefastr se

quiet oprobit happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income married health  `citylist',robust cluster(twinpair)
outreg2 using TableA1_edu_oprobit_happiness4scale.doc,  nolabel append coefastr se

quiet oprobit happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income married health work_over_40h   `citylist',robust cluster(twinpair)
outreg2 using TableA1_edu_oprobit_happiness4scale.doc,  nolabel append coefastr se

**** MZ Twins ****
cd "E:\MyPapers\happiness\TWINSDATA"

use twins_MZ_IV_DIF.dta, clear

sum Dincome IVDincome sDincome sIVDincome
quiet tab Dincome // check the distribution of difference in log(income)
ttest edu=IVedu
ttest Dhappiness_4scale=0
ttest Dedu=0
ttest IVDedu=0
ttest sDedu=0
ttest sIVDedu=0
ttest Dedu=0
ttest Dmarried=0
ttest Dhealth=0 
ttest Dwork_over_40h=0


ttest sDedu==sIVDedu

//corr income IVincome
//corr income IVincome

pwcorr Dedu IVDedu
pwcorr sDedu sIVDedu

quiet reshape wide
tab happiness_4scale1 happiness_4scale2

drop if income1==0 | income2==0

//corr income IVincome
tab happiness_4scale1 happiness_4scale2

tab edu1 edu2
corr edu1 edu2

//tab edu1 edu2

quiet reshape long


gen Abs_Dedu  = abs(Dedu)
sum Abs_Dedu //average diff in edu 1.1
tab Abs_Dedu // 20% of pairs with a diff in edu >2

pwcorr edu IVedu, sig star(.05)
pwcorr edu IVedu if twcode==1,sig star(.05)
pwcorr edu IVedu if twcode==2,sig star(.05)

pwcorr Dedu  IVDedu , sig star(.05)
pwcorr sDedu  sIVDedu , sig star(.05)

ttest Dedu=0
ttest IVDedu =0
ttest sDedu =0
ttest sIVDedu =0

cd "E:\MyPapers\happiness\TWINSDATA\result\Nov2017Edu"

//Table2 :continued (column 2)

**** OLS *****
quiet reg happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d `citylist',robust cluster(twinpair)
outreg2 using Table3_edu_happiness4scale.doc,nolabel append coefastr se

quiet reg happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income   `citylist',robust cluster(twinpair)
outreg2 using Table3_edu_happiness4scale.doc,nolabel append coefastr se

quiet reg happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income married health  `citylist',robust cluster(twinpair)
outreg2 using Table3_edu_happiness4scale.doc,nolabel append coefastr se

quiet reg happiness_4scale edu age agesquared male birth_rate VeryEarly_disease_d income married health work_over_40h  `citylist',robust cluster(twinpair)
outreg2 using Table3_edu_happiness4scale.doc,nolabel append coefastr se

est store OLS_MZtwins

***** IV: NOT reported in paper****
quiet ivreg happiness_4scale (edu=IVedu) age agesquared male birth_rate VeryEarly_disease_d `citylist',robust cluster(twinpair)
outreg2 using TableIV_edu_happiness4scale.doc,  nolabel replace coefastr se


quiet ivreg happiness_4scale (edu=IVedu) age agesquared male birth_rate VeryEarly_disease_d income   `citylist',robust cluster(twinpair)
outreg2 using TableIV_edu_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg happiness_4scale (edu=IVedu) age agesquared male birth_rate VeryEarly_disease_d income married health  `citylist',robust cluster(twinpair)
outreg2 using TableIV_edu_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg happiness_4scale (edu=IVedu) age agesquared male birth_rate VeryEarly_disease_d income married health  work_over_40h `citylist' ,robust cluster(twinpair)
outreg2 using TableIV_edu_happiness4scale.doc,  nolabel append coefastr se


****  FE ****

collapse Dhappiness_4scale Dincome Dincome_highschoolabove Dincome_collegeabove sDincome IVDincome sIVDincome Dwage Dedu AbsDedu Dedu_highschoolabove Dedu_collegeabove Dmarried Dunemployed Dhealth Dwork_over_40h Dbirth_rate DVeryEarly_disease_d Dincome_spouse Dwage_spouse Dedu_spouse ///
         Ahappiness_4scale Aincome Awage Amarried Ahealth Awork_over_40h Aedu Aunemployed Abirth_rate AVeryEarly_disease_d Aincome_spouse Awage_spouse Aedu_spouse ///
         Dtenure Atenure, by(twinpair) 

//quiet reg Dhappiness_4scale Dincome,noc robust
//outreg2 using Table2_happiness4scale.doc,  nolabel append coefastr se
//iis twinpair
//tis twcode
//xtreg happiness_4scale income birth_rate VeryEarly_disease_d edu tenure, fe robust

//Use First Diff
quiet reg Dhappiness_4scale Dedu Dbirth_rate DVeryEarly_disease_d,noc robust
outreg2 using Table3_edu_happiness4scale.doc,  nolabel append coefastr se

quiet reg Dhappiness_4scale Dedu Dbirth_rate DVeryEarly_disease_d Dincome, noc robust
outreg2 using Table3_edu_happiness4scale.doc,  nolabel append coefastr se

quiet reg Dhappiness_4scale Dedu Dbirth_rate DVeryEarly_disease_d Dincome  Dmarried Dhealth, noc robust
outreg2 using Table3_edu_happiness4scale.doc,  nolabel append coefastr se

quiet reg Dhappiness_4scale Dedu Dbirth_rate DVeryEarly_disease_d Dincome  Dmarried Dhealth Dwork_over_40h ,noc robust
outreg2 using Table3_edu_happiness4scale.doc,  nolabel append coefastr se

est store FE_MZtwins

//suest OLS_MZtwins FE_MZtwins
//test [OLS_MZtwins_mean]income=[FE_MZtwins_mean]Dincome


//IVFE 
//cd "I:\happiness\TWINSDATA"
cd "E:\MyPapers\happiness\TWINSDATA"

use twins_MZ_IV_DIF.dta, clear
quiet reshape wide

drop if income1==0 | income2==0
//drop if IVincome1 ==0 | IVincome2==0

quiet reshape long

pwcorr income IVincome
pwcorr income IVincome if twcode==1
pwcorr income IVincome if twcode==2

pwcorr Dincome IVDincome, sig star(.05)
pwcorr sDincome sIVDincome, sig star(.05)

ttest Dincome=0
ttest IVDincome=0
ttest sDincome=0
ttest sIVDincome=0


collapse Dhappiness_4scale Dincome Dincome_highschoolabove Dincome_collegeabove sDincome IVDincome sIVDincome Dwage Dedu IVDedu sDedu sIVDedu Dedu_highschoolabove Dedu_collegeabove Dmarried Dunemployed Dhealth Dwork_over_40h Dbirth_rate DVeryEarly_disease_d Dincome_spouse Dwage_spouse Dedu_spouse ///
         Ahappiness_4scale Aincome Awage Amarried Ahealth Awork_over_40h Aedu Aunemployed Abirth_rate AVeryEarly_disease_d Aincome_spouse Awage_spouse Aedu_spouse ///
         Dtenure Atenure, by(twinpair) 

cd "E:\MyPapers\happiness\TWINSDATA\result\Nov2017Edu"
****  Table 6: IVFE *****
****  IVFE-1 ****
quiet ivreg Dhappiness_4scale (Dedu=IVDedu) Dbirth_rate DVeryEarly_disease_d,noc robust
outreg2 using Table6IVFE_Edu_happiness4scale.doc,  nolabel replace coefastr se

quiet ivreg Dhappiness_4scale (Dedu=IVDedu) Dbirth_rate DVeryEarly_disease_d Dincome,noc robust
outreg2 using Table6IVFE_Edu_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg Dhappiness_4scale (Dedu=IVDedu) Dbirth_rate DVeryEarly_disease_d Dincome Dmarried Dhealth, noc robust
outreg2 using Table6IVFE_Edu_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg Dhappiness_4scale (Dedu=IVDedu) Dbirth_rate DVeryEarly_disease_d Dincome Dmarried Dhealth Dwork_over_40h, noc robust
//quiet ivreg2 Dhappiness_4scale (Dincome=IVDincome) Dedu Dmarried Dhealth Dwork_over_40h, noc robust
outreg2 using Table6IVFE_Edu_happiness4scale.doc,  nolabel append coefastr se

est store IVFE1_MZtwins

****  IVFE-2 ****
quiet ivreg Dhappiness_4scale (sDedu=sIVDedu) Dbirth_rate DVeryEarly_disease_d,noc robust
outreg2 using Table6IVFE_Edu_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg Dhappiness_4scale (sDedu=sIVDedu) Dbirth_rate DVeryEarly_disease_d Dincome,noc robust
outreg2 using Table6IVFE_Edu_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg Dhappiness_4scale (sDedu=sIVDedu) Dbirth_rate DVeryEarly_disease_d Dincome Dmarried Dhealth, noc robust
outreg2 using Table6IVFE_Edu_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg Dhappiness_4scale (sDedu=sIVDedu) Dbirth_rate DVeryEarly_disease_d Dincome Dmarried Dhealth Dwork_over_40h, noc robust
//quiet ivreg2 Dhappiness_4scale (Dincome=IVDincome) Dedu Dmarried Dhealth Dwork_over_40h, noc robust
outreg2 using Table6IVFE_Edu_happiness4scale.doc,  nolabel append coefastr se

//codes below are optional

**** Table 4: Correlation between family and within-family
collapse Dhappiness_4scale Dincome Dwage Dedu Dmarried Dunemployed Dhealth Dbirth_rate DVeryEarly_disease_d Dincome_spouse Dwage_spouse Dedu_spouse Dwork_over_40h ///
         Ahappiness_4scale Aincome Awage Amarried Ahealth Aedu Aunemployed Abirth_rate AVeryEarly_disease_d Aincome_spouse Awage_spouse Aedu_spouse Awork_over_40h ///
         Dtenure Atenure, by(twinpair) 
pwcorr Dincome Dwage Dedu Dmarried Dunemployed Dhealth Dbirth_rate DVeryEarly_disease_d Dincome_spouse Dwage_spouse Dedu_spouse Dwork_over_40h, sig star(.05)

pwcorr Aincome Awage Aedu Amarried Aunemployed Ahealth Abirth_rate AVeryEarly_disease_d Aincome_spouse Awage_spouse Aedu_spouse Awork_over_40h,sig star(.05)

pwcorr Dedu Dmarried Dunemployed Dhealth Dbirth_rate DVeryEarly_disease_d Dincome_spouse Dwage_spouse Dedu_spouse Dtenure , sig star(.05)

pwcorr Aedu Amarried Aunemployed Ahealth Abirth_rate AVeryEarly_disease_d Aincome_spouse Awage_spouse Aedu_spouse Atenure,sig star(.05)

//robustness check for married couples: luck on the marriage market; not reported in paper
//cd "I:\happiness\TWINSDATA"
cd "E:\MyPapers\happiness\TWINSDATA"

use twins_MZ_IV_DIF.dta, clear

//cd "I:\happiness\TWINSDATA\result\Feb2016"
cd "E:\MyPapers\happiness\TWINSDATA\result\Feb2017"

quiet reshape wide

drop if edu_spouse1==. | edu_spouse2==.
drop if income_spouse1==. | income_spouse2==.
//drop if income1==0 | income2==0
//drop if income_spouse1==0 | income_spouse2==0

quiet reshape long

quiet reg happiness_4scale income age agesquared male birth_rate VeryEarly_disease_d edu health work_over_40h  `citylist' edu_spouse,robust cluster(twinpair)
outreg2 using Table_Married_SpouseEduIncome_happiness4scale.doc,nolabel replace coefastr se

quiet reg happiness_4scale income age agesquared male birth_rate VeryEarly_disease_d edu health work_over_40h  `citylist' edu_spouse income_spouse ,robust cluster(twinpair)
outreg2 using Table_Married_SpouseEduIncome_happiness4scale.doc,nolabel append coefastr se


collapse Dhappiness_4scale Dincome sDincome IVDincome sIVDincome Diff_d_income_zero IVD_d_income_zero sD_d_income_zero    sIVD_d_income_zero Dincome_highschoolabove Dincome_collegeabove Dwage ///
         Dedu Dedu_highschoolabove Dedu_collegeabove Dedu_spouse Dmarried Dunemployed Dhealth Dwork_over_40h Dbirth_rate DVeryEarly_disease_d Dincome_spouse Dwage_spouse  ///
         Ahappiness_4scale Aincome Awage Amarried Ahealth Awork_over_40h Aedu Aedu_spouse Aunemployed Abirth_rate Aearly_disease_d Aincome_spouse Awage_spouse ///
         Dtenure Atenure, by(twinpair) 

quiet reg Dhappiness_4scale Dincome Dbirth_rate DVeryEarly_disease_d Dedu Dhealth Dwork_over_40h Dedu_spouse, noc robust
outreg2 using Table_Married_SpouseEduIncome_happiness4scale.doc,  nolabel append coefastr se

quiet reg Dhappiness_4scale Dincome Dbirth_rate DVeryEarly_disease_d Dedu Dhealth Dwork_over_40h Dedu_spouse Dincome_spouse, noc robust
outreg2 using Table_Married_SpouseEduIncome_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg Dhappiness_4scale (Dincome =IVDincome) Dbirth_rate DVeryEarly_disease_d Dedu Dhealth Dwork_over_40h Dedu_spouse, noc robust
outreg2 using Table_Married_SpouseEduIncome_happiness4scale.doc,  nolabel append coefastr se

quiet ivreg Dhappiness_4scale (Dincome =IVDincome) Dbirth_rate DVeryEarly_disease_d Dedu Dhealth Dwork_over_40h Dedu_spouse Dincome_spouse, noc robust
outreg2 using Table_Married_SpouseEduIncome_happiness4scale.doc,  nolabel append coefastr se



// Table 7: Robustness: other measures of education: education level


cd "E:\MyPapers\happiness\TWINSDATA"

use twins_mz_clean.dta, clear
//cd "H:\MyPapers\happiness\TWINSDATA\result"
cd "E:\MyPapers\happiness\TWINSDATA\result\Nov2017Edu"


*** FD ***





exit, clear
log close
