clear
clear matrix
set mem 80m
set matsize 800
capture log close

//cd "H:\MyPapers\Happiness\TWINSDATA"
cd "C:\Users\Aiqing-Jiang\Desktop\HappinessCODES\TWINSDATA"

use twins2a.dta, clear

log using SWB_datagen.txt, replace

sort c7a
merge c7a using industry_wagegrowth.dta

***** rename industry_wage_growth_past9y industry_wage_growth_past8y
replace industry_wage_growth_past8y=industry_wage_growth_past8y*9/8


***** keep the pairs only have two siblings *****
gen double twinpair = citycode*1000+hhcode
egen number=count(twinpair), by(twinpair)
drop if number!=2
drop number


***** Randomize the twin code (1,2) *****
gen random_12 = uniform()
bysort twinpair: egen switch= sum(random_12)
replace switch = (switch>1.0)
gen twcode_backup = twcode
replace twcode=3-twcode if switch==1


** another way to randomize
/*
gen first_as_first=switch
replace twcode=1 if (first_as_first==1 & twcode_backup==1)
replace twcode=2 if (first_as_first==1 & twcode_backup==2)
replace twcode=2 if (first_as_first==0 & twcode_backup==1)
replace twcode=1 if (first_as_first==0 & twcode_backup==2)
*/

tab twcode
sum switch


***********************Birth Order**********************
tab e1
gen BirthOrder_ElderDummy = e1==1


***********************CITY DUMMIES*********************
tab city
gen d_Chengdu =  (city=="Chengdu") & (city~="")
gen d_Chongqing = (city=="Chongqing") & (city~="")
gen d_Haerbin = (city=="Haerbin") & (city~="")
gen d_Hefei = (city=="Hefei") & (city~="")
gen d_Wuhan =  (city=="Wuhan") & (city~="")


****** Birth Rate and Early Life Health ******
gen birth_rate = e4_2
gen height     = f6
gen height_spouse = f9


gen early_disease=0
forvalues i=1(1)8 {
replace early_disease= early_disease+(f11_`i'==1 & f12_`i'<=18)
}
forvalues j=1(1)4{
replace early_disease= early_disease+(f11_9_`j'==1 & f12_9_`j'<=18)
}
gen early_disease_d = early_disease>0


gen VeryEarly_disease=0
forvalues i=1(1)8 {
replace VeryEarly_disease= VeryEarly_disease+(f11_`i'==1 & f12_`i'<=3)
}
forvalues j=1(1)4{
replace VeryEarly_disease= VeryEarly_disease+(f11_9_`j'==1 & f12_9_`j'<=3)
}
gen VeryEarly_disease_d = VeryEarly_disease>0

*******************EDUCATION PROCESSING METHOD BY ZHEYU************
**self-reported education level
**replace a3_1=0 if a3_1==.
**replace a3_2=0 if a3_2==.
**replace a3_3=0 if a3_3==.
**replace a3_4=0 if a3_4==.
**replace a3_5=0 if a3_5==.
**replace a3_6=0 if a3_6==.
**replace a3_7=0 if a3_7==.
**replace a3_8=0 if a3_8==.
**gen edu=a3_1+a3_2+a3_3+a3_4+a3_5+a3_6+a3_7+a3_8
********************************************************************

*******************EDUCATION*****************************
gen year_primary = a3_1
replace year_primary =  0 if (a3_1==. & a1_1~=1 & a4_1~=1)
replace year_primary =  6 if (a3_1==. & a4_1==1)

gen year_junior = a3_2
replace year_junior =  0 if (a3_2==. & a1_2~=1 & a4_2~=1)
replace year_junior =  3 if (a3_2==. & a4_2==1)

gen year_senior = a3_3
replace year_senior  =  0 if (a3_3==. & a1_3~=1 & a4_3~=1)
replace year_senior  =  3 if (a3_3==. & a4_3==1)

gen year_vaca = a3_4
replace year_vaca  =  0 if (a3_4==. & a1_4~=1 & a4_4~=1)
replace year_vaca  =  3 if (a3_4==. & a4_4==1)
 
gen year_college = a3_5
replace year_college  =  0 if (a3_5==. & a1_5~=1 & a4_5~=1)
replace year_college  =  3 if (a3_5==. & a4_5==1)


gen year_bachelor = a3_6
replace year_bachelor  =  0 if (a3_6==. & a1_6~=1 & a4_6~=1)
replace year_bachelor  =  4 if (a3_6==. & a4_6==1)

gen year_master = a3_7
replace year_master   =  0 if (a3_7==. & a1_7~=1 & a4_7~=1)
replace year_master   =  3 if (a3_7==. & a4_7==1)

gen year_doctor = a3_8
replace year_doctor   =  0 if (a3_8==. & a1_8~=1 & a4_8~=1)
replace year_doctor   =  3 if (a3_8==. & a4_8==1)

gen edu =   year_primary+ year_junior +year_senior+year_vaca+ year_college+ year_bachelor+ year_master +year_doctor
gen edusquared = edu*edu

//dummies

gen edu_primary = a4_1==1 & a4_2~=1 & a4_3~=1 & a4_4~=1 & a4_5~=1 & a4_6~=1 & a4_7~=1 & a4_8~=1
gen edu_junior  = a4_2==1 & a4_3~=1 & a4_4~=1 & a4_5~=1 & a4_6~=1 & a4_7~=1 & a4_8~=1
gen edu_senior  = a4_3==1 & a4_4~=1 & a4_5~=1 & a4_6~=1 & a4_7~=1 & a4_8~=1
gen edu_techhigh     = a4_4==1 & a4_5~=1 & a4_6~=1 & a4_7~=1 & a4_8~=1
gen edu_college = a4_5==1 & a4_6~=1 & a4_7~=1 & a4_8~=1
gen edu_univ    = a4_6==1 & a4_7~=1 & a4_8~=1
gen edu_master  = a4_7==1 & a4_8~=1
gen edu_doctoral= a4_8==1

gen edu_techhighcollege = edu_techhigh | edu_college
gen edu_univabove = edu_univ | edu_master |edu_doctoral // too few master & doctorals

gen edu_highschoolabove = a4_3==1 | a4_4==1 | a4_5==1 | a4_6==1 | a4_7==1 | a4_8==1
gen edu_collegeabove = a4_5==1 | a4_6==1 | a4_7==1 | a4_8==1

sum edu_primary edu_junior edu_senior edu_techhigh  edu_college edu_univ edu_master edu_doctoral edu_highschoolabove edu_collegeabove
 
**************************educational level*************************

gen junior      = (a1_2==1&a1_3==2&a1_4==2&a1_5==2&a1_6==2&a1_7==2&a1_8==2)
gen high		= (a1_3==1&a1_4==2&a1_5==2&a1_6==2&a1_7==2&a1_8==2)
gen techhigh	= (a1_4==1&a1_5==2&a1_6==2&a1_7==2&a1_8==2)
gen college		= (a1_5==1&a1_6==2&a1_7==2&a1_8==2)
gen univ		= (a1_6==1|a1_7==1|a1_8==1)
gen tech=techhigh==1|college==1 //combine techhigh and college

gen edu_level   = edu
replace edu_level =1 if (a1_2==2&a1_3==2&a1_4==2&a1_5==2&a1_6==2&a1_7==2&a1_8==2)
replace edu_level =2 if (junior==1)
replace edu_level =3 if (high==1)
replace edu_level =4 if (tech==1)
replace edu_level =5 if (univ==1)
replace edu_level =1 if (edu_level==6)



**IV educational level
gen IVjunior	= (a14_2==1&a14_3==2&a14_4==2&a14_5==2&a14_6==2&a14_7==2&a14_8==2)
gen IVhigh		= (a14_3==1&a14_4==2&a14_5==2&a14_6==2&a14_7==2&a14_8==2)
gen IVtechhigh	= (a14_3==2&a14_4==1&a14_5==2&a14_6==2&a14_7==2&a14_8==2)
gen IVcollege	= (a14_5==1&a14_6==2&a14_7==2&a14_8==2)
gen IVuniv		= (a14_6==1|a14_7==1|a14_8==1)
gen IVtech=IVtechhigh==1|IVcollege==1


******************SIBLING'S EDUCATION***********************
gen 	  year_primary_sib = a16_1
replace year_primary_sib =  0 if (a16_1==. & a14_1~=1 & a17_1~=1)

gen	  year_junior_sib  = a16_2
replace year_junior_sib  =  0 if (a16_2==. & a14_2~=1 & a17_2~=1)

gen 	  year_senior_sib  = a16_3
replace year_senior_sib  =  0 if (a16_3==. & a14_3~=1 & a17_3~=1)

gen 	  year_vaca_sib    = a16_4
replace year_vaca_sib 	 =  0 if (a16_4==. & a14_4~=1 & a17_4~=1)
 
gen 	  year_college_sib = a16_5
replace year_college_sib =  0 if (a16_5==. & a14_5~=1 & a17_5~=1)

gen 	  year_bachelor_sib = a16_6
replace year_bachelor_sib =  0 if (a16_6==. & a14_6~=1 & a17_6~=1)

gen 	  year_master_sib = a16_7
replace year_master_sib =  0 if (a16_7==. & a14_7~=1 & a17_7~=1)

gen 	  year_doctor_sib = a16_8
replace year_doctor_sib =  0 if (a16_8==. & a14_8~=1 & a17_8~=1)

gen edu_sib =   year_primary_sib+ year_junior_sib +year_senior_sib+year_vaca_sib+ year_college_sib+ year_bachelor_sib+ year_master_sib +year_doctor_sib
gen edusquared_sib = edu_sib*edu_sib


//dummies
gen edu_primary_sib = a17_1==1 & a17_2~=1 & a17_3~=1 & a17_4~=1 & a17_5~=1 & a17_6~=1 & a17_7~=1 & a17_8~=1
gen edu_junior_sib  = a17_2==1 & a17_3~=1 & a17_4~=1 & a17_5~=1 & a17_6~=1 & a17_7~=1 & a17_8~=1
gen edu_senior_sib  = a17_3==1 & a17_4~=1 & a17_5~=1 & a17_6~=1 & a17_7~=1 & a17_8~=1
gen edu_techhigh_sib     = a17_4==1 & a17_5~=1 & a17_6~=1 & a17_7~=1 & a17_8~=1
gen edu_college_sib = a17_5==1 & a17_6~=1 & a17_7~=1 & a17_8~=1
gen edu_univ_sib    = a17_6==1 & a17_7~=1 & a17_8~=1
gen edu_master_sib  = a17_7==1 & a17_8~=1
gen edu_doctoral_sib= a17_8==1
gen edu_univabove_sib = edu_univ_sib | edu_master_sib | edu_doctoral_sib // too few master & doctorals



***** create a IV, and replace IVedu1 as edu_sib2 later **********
gen IVedu    = edu 
gen sIVedu   = edu_sib
gen Aedu_own     = edu

 
*******************SPOUSE'S EDUCATION************************
gen edu_spouse_level = 6 if (b4==1)
replace edu_spouse_level = 9  if (b4==2)
replace edu_spouse_level = 12  if (b4==3 | b4==4)
replace edu_spouse_level = 15  if (b4==5)
replace edu_spouse_level = 16  if (b4==6)
replace edu_spouse_level = 19  if (b4==7)
*gen edu_spouse = b7
gen edu_spouse = edu_spouse_level
gen edusquared_spouse = edu_spouse*edu_spouse
gen edu_cou_dif = edu - edu_spouse

gen CCP = e10==1 if e10~=.


*****************BASIC *********************************
gen tenure = c3
gen age = (2002-e4_1_1)/10
gen agesquared = age*age
gen male = (e8==1) 
replace male = . if(e8==.)
gen married = (b1==2)
replace married = . if  (b1==.)
gen divorced = (b1==3)  if  (b1~=.)
gen widowed = (b1==4)  if  (b1~=.)
gen nchild	= b8_2
gen nchild_died = b8_1-b8_2


*******************INCOME*******************************
gen population = e13

gen income = ln(c12a+1)
gen income_num = c12a

replace income=0 if c12a==. & c4a~=1   // a looser defintion to have more observations
replace income_num=0 if c12a==. & c4a~=1   // a looser defintion to have more observations

xtile Incrank_Chengdu= income if d_Chengdu==1, nquantiles(5) //Create variable containing quantile categories

xtile Incrank_Chongqing= income if d_Chongqing==1, nquantiles(5)

xtile Incrank_Haerbin= income if d_Haerbin==1, nquantiles(5)

xtile Incrank_Hefei= income if d_Hefei==1, nquantiles(5)

xtile Incrank_Wuhan= income if d_Wuhan==1, nquantiles(5)

gen Incrank=1 if income~=.
replace Incrank=2 if Incrank_Chengdu==2|Incrank_Chongqing==2|Incrank_Haerbin==2|Incrank_Hefei==2|Incrank_Wuhan==2
replace Incrank=3 if Incrank_Chengdu==3|Incrank_Chongqing==3|Incrank_Haerbin==3|Incrank_Hefei==3|Incrank_Wuhan==3
replace Incrank=4 if Incrank_Chengdu==4|Incrank_Chongqing==4|Incrank_Haerbin==4|Incrank_Hefei==4|Incrank_Wuhan==4
replace Incrank=5 if Incrank_Chengdu==5|Incrank_Chongqing==5|Incrank_Haerbin==5|Incrank_Hefei==5|Incrank_Wuhan==5

gen income_squared = income*income
gen d_income_zero = income==0 if income~=.
gen d_income_below100 = income<log(100) if income~=.

//income braket
gen income_0to200     = income_num<200
gen income_200to400   = income_num>=200 & income_num<400
gen income_400to800   = income_num>=400 & income_num<800
gen income_800to1600  = income_num>=800 & income_num<1600
gen income_1600above  = income_num>=1600 & income_num~=.

gen income_bracket = 1 
replace income_bracket = 2 if income_200to400==1
replace income_bracket = 3 if income_400to800==1
replace income_bracket = 4 if income_800to1600==1
replace income_bracket = 5 if income_1600above==1

gen income_highschoolabove = income*edu_highschoolabove
gen income_collegeabove = income*edu_collegeabove

gen income_spouse = ln(c12b+1)
gen rawwage = c121a
//gen wage = ln(c121a+1)
//only use those with positive earning
gen wage_withzero = ln(c121a+1)
gen wage = ln(c121a)
gen d_zerowage = wage_withzero==0 if wage_withzero~=.

gen wage_spouse = ln(c121b+1)
gen inc_cou_dif = income-income_spouse
gen wage_cou_dif = wage-wage_spouse
gen income_bothspouse = ln(c12a+c12b+1)
gen income_aver	 = ln((c12a+c12b)/population+1)

// Family income of last year (9 scales)
gen familyinc = e18
gen unemployed = (c4a==4) if (c4a~=.)
gen working = (c4a==1) if c4a~=.
gen not_in_lf = (c4a==2 | c4a==3 | c4a==5) if c4a~=.

gen workday = c9a
replace workday=0 if working==0 & workday==.

gen workhour_legal = c10a
replace workhour_legal=0 if working==0 & workhour_legal==.

gen workhour = c11a
replace workhour=0 if working==0 & workhour==.
gen workhour_sq = workhour*workhour

gen work_zero = workhour==0
gen work_pt  = workhour<=20 & workhour~=0 
gen work_ot  = workhour>workhour_legal if workhour~=. & workhour_legal~=.
gen work_over_fifty  = workhour>50 if workhour~=. 
gen work_over_40h    = workhour>40 if workhour~=.

gen workhour_month = (workday/7)*workhour
replace workhour_month = workhour_month/100
gen workhour_month_sq = workhour_month*workhour_month

gen wage_rate_num = c121a/workhour_month
gen wage_rate     = ln(wage_rate_num)


** 82 percentile **
***** Industry Mean Income from outside data (NBS) ******

gen log_industry_wage_2001 = log(industry_wage_2001)


***** Median/Mean Income within Industries from the data*****

gen c7a_recode = c7a
replace c7a_recode=0 if c7a_recode==.

bysort c7a: egen indu_med_income=median(c12a)
replace indu_med_income = log(indu_med_income)

bysort c7a_recode: egen indu_mean_income=mean(c12a)
egen cell_pop=count(c7a_recode), by(c7a_recode)
tab cell_pop
replace indu_mean_income=(indu_mean_income*cell_pop-c12a)/(cell_pop-1)
drop cell_pop
replace indu_mean_income = log(indu_mean_income)

bysort c7a c6a: egen indu_occu_med_income=median(c12a)
replace indu_occu_med_income = log(indu_occu_med_income)

bysort c7a c6a: egen indu_occu_mean_income=mean(c12a)
replace indu_occu_mean_income = log(indu_occu_mean_income)

bysort city c7a c6a: egen indu_occu_city_med_income=median(c12a)
replace indu_occu_city_med_income = log(indu_occu_city_med_income)

bysort city c7a c6a: egen indu_occu_city_mean_income=mean(c12a)
replace indu_occu_city_mean_income = log(indu_occu_city_mean_income)

//generate age group
gen age_group=1 if age~=.
replace age_group=2 if age>2.5 & age<=3.5
replace age_group=3 if age>3.5 & age<=4.5
replace age_group=4 if age>4.5 & age~=.

bysort c7a age_group: egen indu_age_med_income=median(c12a)
replace indu_age_med_income=log(indu_age_med_income)

bysort c7a_recode age_group: egen indu_age_mean_income=mean(c12a)
gen indu_age_group=c7a_recode*10+age_group
egen cell_pop=count(indu_age_group), by(indu_age_group)
quiet tab cell_pop
replace indu_age_mean_income=(indu_age_mean_income*cell_pop-c12a)/(cell_pop-1)
drop cell_pop
replace indu_age_mean_income= log(indu_age_mean_income)

bysort c7a c6a age_group: egen indu_occu_age_med_income=median(c12a)
replace indu_occu_age_med_income = log(indu_occu_age_med_income)

bysort c7a c6a age_group: egen indu_occu_age_mean_income=mean(c12a)
replace indu_occu_age_mean_income = log(indu_occu_age_mean_income)


bysort city c7a c6a age_group: egen indu_occu_city_age_med_income=median(c12a)
replace indu_occu_city_age_med_income = log(indu_occu_city_age_med_income)

bysort city c7a c6a age_group: egen indu_occu_city_age_mean_income=mean(c12a)
replace indu_occu_city_age_mean_income = log(indu_occu_city_age_mean_income)

gen nochangofjob_past5y= c161b<=1996 | c161b==.


*******************INCOME IV****************************
//Sibling's income reported by you
gen 		income_sib = ln(d43a/12+1)
replace 	income_sib = ln(d43b/12+1) if d32b==1
replace 	income_sib = ln(d43c/12+1) if d32c==1
replace 	income_sib = ln(d43d/12+1) if d32d==1
replace 	income_sib = ln(d43e/12+1) if d32e==1
replace 	income_sib = ln(d43f/12+1) if d32f==1
replace 	income_sib = ln(d43g/12+1) if d32g==1
replace 	income_sib = ln(d43h/12+1) if d32h==1

gen income_sib_selfreported = income_sib // sibling's self-reported income, to be replaced
gen d_income_zero_sib_selfreported = income_sib_selfreported==0 if income_sib_selfreported ~=.


gen 		IVincome   = income_sib
gen         income_squared_sib = income_sib * income_sib
gen         IVincome_squared = income_squared_sib

gen         d_income_zero_sib = income_sib==0 if income_sib~=.
gen         IV_d_income_zero = d_income_zero_sib 

gen         d_income_below100_sib = income_sib<log(100) if income_sib~=.
gen         IV_d_income_below100 = d_income_below100_sib  



xtile IVIncrank_Chengdu= IVincome if d_Chengdu==1, nquantiles(5) //Create variable containing quantile categories

xtile IVIncrank_Chongqing= IVincome if d_Chongqing==1, nquantiles(5)

xtile IVIncrank_Haerbin= IVincome if d_Haerbin==1, nquantiles(5)

xtile IVIncrank_Hefei= IVincome if d_Hefei==1, nquantiles(5)

xtile IVIncrank_Wuhan= IVincome if d_Wuhan==1, nquantiles(5)

gen IVIncrank=1 if income~=.
replace IVIncrank=2 if IVIncrank_Chengdu==2|IVIncrank_Chongqing==2|IVIncrank_Haerbin==2|IVIncrank_Hefei==2|IVIncrank_Wuhan==2
replace IVIncrank=3 if IVIncrank_Chengdu==3|IVIncrank_Chongqing==3|IVIncrank_Haerbin==3|IVIncrank_Hefei==3|IVIncrank_Wuhan==3
replace IVIncrank=4 if IVIncrank_Chengdu==4|IVIncrank_Chongqing==4|IVIncrank_Haerbin==4|IVIncrank_Hefei==4|IVIncrank_Wuhan==4
replace IVIncrank=5 if IVIncrank_Chengdu==5|IVIncrank_Chongqing==5|IVIncrank_Haerbin==5|IVIncrank_Hefei==5|IVIncrank_Wuhan==5

/*
gen IVincome_0to200     = income_num<200
gen IVincome_200to400   = income_num>=200 & income_num<400
gen IVincome_400to800   = income_num>=400 & income_num<800
gen IVincome_800to1600  = income_num>=800 & income_num<1600
gen IVincome_1600above  = income_num>=1600 & income_num~=.
*/
/*
tab f15_3 income_0to100 
tab f15_3 income_100to200 
tab f15_3 income_100to200 
*/
/*
gen income_bracket = 1 
replace income_bracket = 2 if income_200to400==1
replace income_bracket = 3 if income_400to800==1
replace income_bracket = 4 if income_800to1600==1
replace income_bracket = 5 if income_1600above==1
*/

****************** Transfer between Twins***************
gen transfer_out 		= d44a
replace transfer_out 	= d44b if d32b==1
replace transfer_out 	= d44c if d32c==1
replace transfer_out 	= d44d if d32d==1
replace transfer_out 	= d44e if d32e==1
replace transfer_out 	= d44f if d32f==1
replace transfer_out 	= d44g if d32g==1
replace transfer_out 	= d44h if d32h==1

gen transfer_in 		= d45a
replace transfer_in 	= d45b if d32b==1
replace transfer_in 	= d45c if d32c==1
replace transfer_in 	= d45d if d32d==1
replace transfer_in 	= d45e if d32e==1
replace transfer_in 	= d45f if d32f==1
replace transfer_in 	= d45g if d32g==1
replace transfer_in 	= d45h if d32h==1

gen transfer_d = (transfer_out~=. & transfer_out~=0)|(transfer_in~=. & transfer_in~=0)

gen transfer_net = transfer_in-transfer_out

// sibling number, including self
gen sibling_num = 1
foreach i in a b c d e f g h {
replace sibling_num = sibling_num + (d32`i'~=.)
}



****************** Income of first job *****************
gen yearfirstjob = c1
gen income_first = ln(c2+1)
**adjust the income with inflation
replace income_first = ln(c2*3.227714163+1) if yearfirstjob==1979
replace income_first = ln(c2*3.11470454+1) if yearfirstjob==1980
replace income_first = ln(c2*3.021877712+1) if yearfirstjob==1981
replace income_first = ln(c2*2.920909512+1) if yearfirstjob==1982
replace income_first = ln(c2*2.914224196+1) if yearfirstjob==1983
replace income_first = ln(c2*2.831535288+1) if yearfirstjob==1984
replace income_first = ln(c2*2.681229271+1) if yearfirstjob==1985
replace income_first = ln(c2*2.532696543+1) if yearfirstjob==1986
replace income_first = ln(c2*2.429815231+1) if yearfirstjob==1987
replace income_first = ln(c2*2.166701001+1) if yearfirstjob==1988
replace income_first = ln(c2*2.013516505+1) if yearfirstjob==1989
replace income_first = ln(c2*1.879175895+1) if yearfirstjob==1990
replace income_first = ln(c2*1.766445066+1) if yearfirstjob==1991
replace income_first = ln(c2*1.660376738+1) if yearfirstjob==1992
replace income_first = ln(c2*1.412610384+1) if yearfirstjob==1993
replace income_first = ln(c2*1.175980051+1) if yearfirstjob==1994
replace income_first = ln(c2*1.037001129+1) if yearfirstjob==1995
replace income_first = ln(c2*0.973101647+1) if yearfirstjob==1996
replace income_first = ln(c2*0.965979756+1) if yearfirstjob==1997
replace income_first = ln(c2*0.9872016740+1) if yearfirstjob==1998
replace income_first = ln(c2*1.010489192+1) if yearfirstjob==1999
replace income_first = ln(c2*1.0097+1)      if yearfirstjob==2000
replace income_first = ln(c2*0.997924491+1) if yearfirstjob==2001


************************ House ************************
gen house_area = e14
gen house_own  = (e16==1 | e16==2 |e16==3 |e16==4) if e16~=.


************************ Trvel ************************
gen travel = (g1a~=1 | g1b~=1) if (g1a~=. | g1b~=.)


************************HEALTH PROXIES******************
gen health = 6-f5
gen health_spouse = 6-f8

gen ill = (f11_1==1 | f11_2==1 | f11_3==1 | f11_4==1 | f11_5==1 | f11_6==1 | f11_7==1 | f11_8==1 | f11_9_1==1 |f11_9_2==1  | f11_9_3==1  | f11_9_4==1 )
gen ill_spouse = (f13_1==1 | f13_2==1 | f13_3==1 | f13_4==1 | f13_5==1 | f13_6==1 | f13_7==1 | f13_8==1 | f13_9_1==1 |f13_9_2==1  | f13_9_3==1  | f13_9_4==1  )

gen disfunction =  ( f11_9_1==1 |f11_9_2==1  | f11_9_3==1  | f11_9_4==1 )
gen disfunction_spouse =  ( f13_9_1==1 |f13_9_2==1  | f13_9_3==1  | f13_9_4==1 )


**********************LIFE HABIT***********************
gen smoke = f2a if (f2a~=6)
gen drink 	= f3a if (f3a~=9)
gen sports = f4a if (f4a~=6)


**********************MOOD****************************
//use 4 scales:
gen happiness_4scale = 5-f15_3

// use 3 scales: 1,2 as new 1, 3 as new 2, 4 as new 3.
gen happiness = 4-f15_3
replace happiness=1 if happiness==0

gen happiness_3scale = happiness
/*
replace happiness=0 if happiness<=2
replace happiness=1 if happiness==3
*/

//binary value: 4 vs 321 or 43 vs 21
gen happiness_4_321 = f15_3==1 if f15_3~=.
gen happiness_43_21 = f15_3==1 | f15_3==2 if f15_3~=.


gen sad_4scale	     = 5-f15_1
gen scared_4scale	 = 5-f15_2
gen angry_4scale	 = 5-f15_4
gen hateful_4scale	 = 5-f15_5


// translate to 3 scales:
recode sad     (1 2 =1) (3=2) (4=3), generate (sad_3scale)
recode scared  (1 2 =1) (3=2) (4=3), generate (scared_3scale)
recode angry   (1 2 =1) (3=2) (4=3), generate (angry_3scale)
recode hateful (1 2 =1) (3=2) (4=3), generate (hateful_3scale)



****************** F16 MOOD Hide *********************
gen sad_hide_4scale	     = 5-f16_1
gen scared_hide_4scale	 = 5-f16_2
gen happiness_hide_4scale =5-f16_3
gen angry_hide_4scale	 = 5-f16_4
gen hateful_hide_4scale	 = 5-f16_5



****************** F17 MOOD Control *********************
gen sad_control_4scale	     = 5-f17_1
gen scared_control_4scale	 = 5-f17_2
gen happiness_control_4scale = 5-f17_3
gen angry_control_4scale	 = 5-f17_4
gen hateful_control_4scale	 = 5-f17_5


******************  F18 Life Short(7 scale) ******************
gen life_short	= 8-f18

******************  F19 Future Decided(7 scale) ***************
gen future_decided           = 8-f19


*********************** Keep Twins both with complete information ************************
gen birth_year = e4_1_1
gen birth_month = e4_1_2

//gen d_haircolor = (e2==1) if (e2~=.)
//gen d_look  =  (e3==1) if (e3~=.)
//another defination to have more observations: code as not the same look/haircolor if not answered
gen d_haircolor = (e2==1) 
gen d_look  =  (e3==1) 


drop if (happiness==.)
/*
drop if (sad==.)
drop if (scared==.)
drop if (angry==.)
drop if (hateful==.)
*/
drop if (sad_4scale==.)
drop if (scared_4scale==.)
drop if (angry_4scale==.)
drop if (hateful_4scale==.)


drop if income==.
drop if (age==.)
drop if (male==.)
drop if (edu==.)
drop if CCP==.
drop if (married==.)
drop if (divorced==.)
drop if (widowed==.)
drop if health==. 

//drop if workhour_month==.
drop if birth_rate==.
//drop if early_disease_d==.
drop if VeryEarly_disease_d==. 
drop if work_over_40h==.
//drop if not_in_lf==.
//drop if unemployed==.

drop if BirthOrder_ElderDummy==.
drop if height==.



save all_twins.dta, replace

********************************************************
use all_twins.dta, clear

keep  happiness happiness_4scale sad_4scale scared_4scale angry_4scale hateful_4scale happiness_3scale sad_3scale scared_3scale angry_3scale hateful_3scale BirthOrder_ElderDummy happiness_4_321  happiness_43_21 birth_rate early_disease_d VeryEarly_disease_d income income_squared income_squared_sib income_num d_income_zero d_income_zero_sib IV_d_income_zero  d_income_below100 d_income_below100_sib IV_d_income_below100  income_bracket Incrank IVIncrank income_highschoolabove income_collegeabove edu_highschoolabove edu_collegeabove edu_univabove income_sib income_sib_selfreported d_income_zero_sib_selfreported  IVincome IVincome_squared income_spouse ///
      indu_med_income indu_mean_income indu_occu_med_income indu_occu_mean_income indu_occu_age_med_income indu_occu_age_mean_income ///
      indu_age_med_income indu_age_mean_income industry_wage_growth_past5y industry_wage_growth_past7y industry_wage_growth_past8y  industry_wage_1993 industry_wage_growth_past9y  industry_wage_1996 income_bothspouse income_aver  familyinc income_first yearfirstjob house_area house_own /// 
      rawwage wage d_zerowage wage_rate wage_spouse  unemployed not_in_lf working work_zero work_pt workday workhour workhour_sq workhour_month workhour_month_sq work_ot work_over_fifty work_over_40h age  agesquared tenure /// 
      edu  edusquared edu_spouse edu_sib IVedu sIVedu Aedu_own high techhigh college univ tech male  c5a c6a c7a /// 
      married divorced widowed smoke drink sports  health health_spouse  disfunction transfer_out transfer_in transfer_net /// 
      twcode twinpair  sibling_num d_haircolor d_look birth_year birth_month  d_Chengdu d_Chongqing d_Haerbin d_Hefei d_Wuhan ///
	  edu_primary  edu_junior edu_senior  edu_techhighcollege edu_univabove edu_techhigh edu_college edu_univ edu_master edu_doctoral indu_age_group industry_wage_2001 log_industry_wage_2001 height height_spouse CCP ///
	  edu_level travel  ///
	  sad_hide_4scale  scared_hide_4scale  happiness_hide_4scale  angry_hide_4scale  hateful_hide_4scale ///
	  sad_control_4scale  scared_control_4scale  happiness_control_4scale  angry_control_4scale  hateful_control_4scale ///
	  life_short future_decided 
	  
quiet reshape wide happiness happiness_4scale sad_4scale scared_4scale angry_4scale hateful_4scale happiness_3scale sad_3scale scared_3scale angry_3scale hateful_3scale BirthOrder_ElderDummy happiness_4_321  happiness_43_21 birth_rate early_disease_d VeryEarly_disease_d income income_squared income_squared_sib income_num d_income_zero d_income_zero_sib IV_d_income_zero  d_income_below100 d_income_below100_sib IV_d_income_below100  income_bracket Incrank IVIncrank income_highschoolabove income_collegeabove edu_highschoolabove edu_collegeabove edu_univabove income_sib income_sib_selfreported d_income_zero_sib_selfreported  IVincome IVincome_squared income_spouse ///
      indu_med_income indu_mean_income indu_occu_med_income indu_occu_mean_income indu_occu_age_med_income indu_occu_age_mean_income ///
      indu_age_med_income indu_age_mean_income industry_wage_growth_past5y industry_wage_growth_past7y industry_wage_growth_past8y  industry_wage_1993 industry_wage_growth_past9y  industry_wage_1996 income_bothspouse income_aver  familyinc income_first yearfirstjob house_area house_own /// 
      rawwage wage d_zerowage wage_rate wage_spouse  unemployed not_in_lf working work_zero work_pt workday workhour workhour_sq workhour_month workhour_month_sq work_ot work_over_fifty work_over_40h age  agesquared tenure /// 
      edu edusquared edu_spouse edu_sib IVedu sIVedu Aedu_own high techhigh college univ tech male  c5a c6a c7a /// 
      married divorced widowed smoke drink sports  health health_spouse  disfunction transfer_out transfer_in transfer_net /// 
      sibling_num d_haircolor d_look birth_year birth_month  d_Chengdu d_Chongqing d_Haerbin d_Hefei d_Wuhan ///
	  edu_primary  edu_junior edu_senior edu_techhighcollege edu_univabove edu_techhigh edu_college edu_univ edu_master edu_doctoral indu_age_group industry_wage_2001 log_industry_wage_2001 height height_spouse CCP ///
	  edu_level travel  ///
	  sad_hide_4scale  scared_hide_4scale  happiness_hide_4scale  angry_hide_4scale  hateful_hide_4scale ///
	  sad_control_4scale  scared_control_4scale  happiness_control_4scale  angry_control_4scale  hateful_control_4scale ///
	  life_short future_decided, i(twinpair) j(twcode)

replace  IVedu1 = edu_sib2
replace  IVedu2 = edu_sib1

//gen dz=(male1~= male2 | d_haircolor1~=1 | d_look1~=1 | d_haircolor2~=1 | d_look2~=1)

gen mz=(male1== male2 & d_haircolor1==1 & d_look1==1 & d_haircolor2==1 & d_look2==1)
gen dz= (mz~=1)

//drop if BirthOrder_ElderDummy1==BirthOrder_ElderDummy2


drop if (age1==.)|(age2==.)
drop if (male1==.)|(male2==.)


drop if (happiness_4scale1==.)|(happiness_4scale2==.)
drop if (sad_4scale1==.)|(sad_4scale2==.)
drop if (scared_4scale1==.)|(scared_4scale2==.)
drop if (angry_4scale1==.)|(angry_4scale2==.)
drop if (hateful_4scale1==.)|(hateful_4scale2==.)

drop if (happiness_hide_4scale1==.)|(happiness_hide_4scale2==.)
drop if (sad_hide_4scale1==.)|(sad_hide_4scale2==.)
drop if (scared_hide_4scale1==.)|(scared_hide_4scale2==.)
drop if (angry_hide_4scale1==.)|(angry_hide_4scale2==.)
drop if (hateful_hide_4scale1==.)|(hateful_hide_4scale2==.)

drop if (happiness_control_4scale1==.)|(happiness_control_4scale2==.)
drop if (sad_control_4scale1==.)|(sad_control_4scale2==.)
drop if (scared_control_4scale1==.)|(scared_control_4scale2==.)
drop if (angry_control_4scale1==.)|(angry_control_4scale2==.)
drop if (hateful_control_4scale1==.)|(hateful_control_4scale2==.)

drop if (life_short1==.)|(life_short2==.)
drop if (future_decided1==.)|(future_decided2==.)


//drop if  (male1~= male2 |  birth_year1~= birth_year2 | birth_month1~= birth_month2)
//drop if (birth_year1==. | birth_year2==.)
//drop if  (birth_year1~= birth_year2)

//drop if  (male1==. |  birth_year1==. | birth_month1==.)


drop if (BirthOrder_ElderDummy1==. )|( BirthOrder_ElderDummy2==.)

drop if (income1==. )| (income2==.)
drop if (married1==.)| (married2==.)
//drop if (divorced1==.)| (divorced2==.)
//drop if (widowed1==.)| (widowed2==.)

drop if (edu1==.)|(edu2==.)
drop if (edu_level1==.)|(edu_level2==.)
//drop if (CCP1==. )|(CCP2==.)
drop if (health1==.)|(health2==.)


//drop if (unemployed1==.)|(unemployed2==.)
//drop if workhour_month1==. | workhour_month2==.
//drop if not_in_lf1==. | not_in_lf2==.

drop if (work_over_40h1==.) | (work_over_40h2==.)
drop if (birth_rate1==.) | (birth_rate2==.)
drop if (height1==.) | (height2==.)


//drop if (early_disease_d1==.)  | (early_disease_d2==.)
drop if (VeryEarly_disease_d1==.)  | (VeryEarly_disease_d2==.) 

drop if (house_own1==.) | (house_own2==.)
drop if (travel1==.) | (travel2==.)
drop if (sports1==.) | (sports2==.)
drop if (drink1==.) | (drink2==.)
drop if (smoke1==.) | (smoke2==.)






gen  Dhappiness 	= happiness1-happiness2
gen  Dhappiness_4_321  = happiness_4_3211-happiness_4_3212
gen  Dhappiness_43_21  = happiness_43_211-happiness_43_212

/*
gen  Dsad 	    = sad1-sad2
gen  Dscared 	= scared1-scared2
gen  Dangry 	= angry1-angry2
gen  Dhateful	= hateful1-hateful2
*/

gen  Dhappiness_4scale = happiness_4scale1-happiness_4scale2
gen  Dsad_4scale 	    = sad_4scale1-sad_4scale2
gen  Dscared_4scale 	= scared_4scale1-scared_4scale2
gen  Dangry_4scale   	= angry_4scale1-angry_4scale2
gen  Dhateful_4scale	= hateful_4scale1-hateful_4scale2

gen  Dhappiness_hide_4scale = happiness_hide_4scale1-happiness_hide_4scale2
gen  Dsad_hide_4scale 	    = sad_hide_4scale1-sad_hide_4scale2
gen  Dscared_hide_4scale 	= scared_hide_4scale1-scared_hide_4scale2
gen  Dangry_hide_4scale   	= angry_hide_4scale1-angry_hide_4scale2
gen  Dhateful_hide_4scale	= hateful_hide_4scale1-hateful_hide_4scale2

gen  Dhappiness_control_4scale 	= happiness_control_4scale1-happiness_control_4scale2
gen  Dsad_control_4scale 	    = sad_control_4scale1-sad_control_4scale2
gen  Dscared_control_4scale 	= scared_control_4scale1-scared_control_4scale2
gen  Dangry_control_4scale   	= angry_control_4scale1-angry_control_4scale2
gen  Dhateful_control_4scale	= hateful_control_4scale1-hateful_control_4scale2

gen  Dlife_short        = life_short1 - life_short2
gen  Dfuture_decided    = future_decided1 - future_decided2

gen  Dhappiness_3scale 	= happiness_3scale1-happiness_3scale2
gen  Dsad_3scale 	    = sad_3scale1-sad_3scale2
gen  Dscared_3scale 	= scared_3scale1-scared_3scale2
gen  Dangry_3scale   	= angry_3scale1-angry_3scale2
gen  Dhateful_3scale	= hateful_3scale1-hateful_3scale2


gen  Dincome      = income1-income2
gen  Dincome_squared = income_squared1-income_squared2

replace  income_sib_selfreported1 = income2 
replace  income_sib_selfreported2 = income1

replace d_income_zero_sib_selfreported1= income_sib_selfreported1==0 if income_sib_selfreported1~=.
replace d_income_zero_sib_selfreported2= income_sib_selfreported2==0 if income_sib_selfreported2~=.

gen  Dincome_num  = exp(income1)-exp(income2)
gen  Dincome_num_year = Dincome_num*12 

gen  Dincome_bracket = income_bracket1-income_bracket2
gen  DIncrank        = Incrank1-Incrank2 

gen  Diff_d_income_zero = d_income_zero1-d_income_zero2
gen  Diff_d_income_below100 =  d_income_below1001-d_income_below1002

gen  Dincome_highschoolabove  = income_highschoolabove1-income_highschoolabove2
gen  Dincome_collegeabove  = income_collegeabove1-income_collegeabove2


gen  DBirthOrder_ElderDummy = BirthOrder_ElderDummy1 - BirthOrder_ElderDummy2
gen  Dmale = male1-male2    //for DZ twins only

gen  Dmarried 	= married1-married2
gen  Ddivorced    = divorced1-divorced2
gen  Dwidowed     = widowed1-widowed2

gen  Dhealth      = health1-health2
gen  Dedu         = edu1-edu2
gen  Dedu_level   = edu_level1-edu_level2
gen  AbsDedu      = Dedu
replace AbsDedu   = -Dedu if Dedu<0

gen  DCCP         = CCP1-CCP2

gen  Dedu_highschoolabove = edu_highschoolabove1-edu_highschoolabove2
gen  Dedu_collegeabove    = edu_collegeabove1-edu_collegeabove2

gen  Dedu_junior = edu_junior1-edu_junior2
gen  Dedu_senior = edu_senior1-edu_senior2
gen  Dedu_techhighcollege = edu_techhighcollege1 - edu_techhighcollege2
gen  Dedu_univabove = edu_univabove1 - edu_univabove1

gen  Dedu_techhigh = edu_techhigh1 - edu_techhigh2
gen  Dedu_college = edu_college1-edu_college2
gen  Dedu_univ = edu_univ1-edu_univ2
gen  Dedu_master = edu_master1-edu_master2
gen  Dedu_doctoral = edu_doctoral1-edu_doctoral2
gen  Dedu_univabove= edu_univabove1-edu_univabove2


gen  Dunemployed  = unemployed1-unemployed2
gen  Dnot_in_lf   = not_in_lf1-not_in_lf2
gen  Dbirth_rate  = birth_rate1-birth_rate2
gen  Dearly_disease_d = early_disease_d1-early_disease_d2
gen  DVeryEarly_disease_d =  VeryEarly_disease_d1-VeryEarly_disease_d2

gen  Dheight      = height1 - height2

gen  Dworkday     = workday1-workday2
gen  Dworkhour    = workhour1-workhour2
gen  Dworkhour_sq = workhour_sq1-workhour_sq2
gen  Dworkhour_month = workhour_month1-workhour_month2
gen  Dworkhour_month_sq =  workhour_month_sq1-workhour_month_sq2
gen  Dwork_ot     = work_ot1-work_ot2
gen  Dwork_over_fifty     = work_over_fifty1-work_over_fifty2
gen  Dwork_over_40h = work_over_40h1-work_over_40h2
gen  Dwork_zero   = work_zero1-work_zero2
gen  Dwork_pt     = work_pt1-work_pt2
gen  Dtenure      = tenure1-tenure2

gen  Dwage        = wage1-wage2
gen  D_d_zerowage = d_zerowage1-d_zerowage2

gen  Dwage_rate   = wage_rate1-wage_rate2
gen  Dhouse_own   = house_own1-house_own2
gen  Dincome_bothspouse = income_bothspouse1-income_bothspouse2
gen  Dfamilyinc = familyinc1-familyinc2
gen  Dincome_first = income_first1-income_first2

gen  Dincome_spouse = income_spouse1-income_spouse2
gen  Dwage_spouse	  = wage_spouse1-wage_spouse2
gen  Dedu_spouse    = edu_spouse1-edu_spouse2

gen  Dindu_med_income 			= indu_med_income1-indu_med_income2
gen  Dindu_mean_income			= indu_mean_income1-indu_mean_income2 
gen  Dindu_occu_med_income		= indu_occu_med_income1-indu_occu_med_income2
gen  Dindu_occu_mean_income		= indu_occu_mean_income1-indu_occu_mean_income2	
gen  Dindu_occu_age_med_income	= indu_occu_age_med_income1-indu_occu_age_med_income2
gen  Dindu_occu_age_mean_income	= indu_occu_age_mean_income1-indu_occu_age_mean_income2
gen  Dindu_age_med_income		= indu_age_med_income1-indu_age_med_income2
gen  Dindu_age_mean_income		= indu_age_mean_income1-indu_age_mean_income2
gen  Dindustry_wage_growth_past5y   = industry_wage_growth_past5y1-industry_wage_growth_past5y2
gen  Dindustry_wage_growth_past9y  = industry_wage_growth_past9y1-industry_wage_growth_past9y2

gen  Dindustry_wage_2001        = industry_wage_20011-industry_wage_20012
gen  Dlog_industry_wage_2001    = log_industry_wage_20011-log_industry_wage_20012

gen  Dtravel  = travel1- travel2
gen  Ddrink   = drink1-drink2
gen  Dsports  = sports1- sports2
gen  Dsmoke   = smoke1-smoke2

//maximum income
gen Max_income = income1
replace Max_income = income2 if income2>income1


*** For comparison of correlations ***
gen Ahappiness_4scale= 0.5*(happiness_4scale1+happiness_4scale2)
gen Ahappiness = 0.5*(happiness1+happiness2)
gen Ahappiness_4_321 = 0.5*(happiness_4_3211+happiness_4_3212)
gen Ahappiness_43_21 = 0.5*(happiness_43_211+happiness_43_212)

/*
gen  Asad 	    = 0.5*(sad1+sad2)
gen  Ascared 	= 0.5*(scared1+scared2)
gen  Aangry 	= 0.5*(angry1+angry2)
gen  Ahateful	= 0.5*(hateful1+hateful2)
*/

gen  Asad_4scale 	    = 0.5*(sad_4scale1+sad_4scale2)
gen  Ascared_4scale 	= 0.5*(scared_4scale1+scared_4scale2)
gen  Aangry_4scale   	= 0.5*(angry_4scale1+angry_4scale2)
gen  Ahateful_4scale	= 0.5*(hateful_4scale1+hateful_4scale2)

gen  Ahappiness_3scale 	= 0.5*(happiness_3scale1+happiness_3scale2)
gen  Asad_3scale 	    = 0.5*(sad_3scale1+sad_3scale2)
gen  Ascared_3scale 	= 0.5*(scared_3scale1+scared_3scale2)
gen  Aangry_3scale   	= 0.5*(angry_3scale1+angry_3scale2)
gen  Ahateful_3scale	= 0.5*(hateful_3scale1+hateful_3scale2)



gen Aincome = 0.5*(income1+income2)
gen Ad_income_zero = 0.5*(d_income_zero1+d_income_zero2)
gen Ad_income_below100 = 0.5*(d_income_below1001+d_income_below1002)
gen Aincome_squared = 0.5*(income_squared1+income_squared2  )

gen Awage    = 0.5*(wage1+wage2)
gen Amarried = 0.5*(married1+married2)
gen Adivorced    = 0.5*(divorced1+divorced2)
gen Awidowed     = 0.5*(widowed1+widowed2)


gen ABirthOrder_ElderDummy = 0.5*(BirthOrder_ElderDummy1+BirthOrder_ElderDummy2)

gen Ahealth  = 0.5*(health1+health2)
gen Aedu     = 0.5*(edu1+edu2)
gen ACCP     = 0.5*(CCP1+CCP2)

gen Aunemployed  = 0.5*(unemployed1+unemployed2)
gen Awork_over_40h = 0.5*(work_over_40h1+work_over_40h2)
gen Abirth_rate  = 0.5*(birth_rate1+birth_rate2)
gen Aearly_disease_d = 0.5*(early_disease_d1+early_disease_d2) 
gen AVeryEarly_disease_d = 0.5*(VeryEarly_disease_d1+VeryEarly_disease_d2)
gen Aheight      = 0.5*(height1+height2)

gen Aincome_spouse = 0.5*(income_spouse1+income_spouse2)
gen Awage_spouse	  = 0.5*(wage_spouse1+wage_spouse2)
gen Aedu_spouse     = 0.5*(edu_spouse1+edu_spouse2)
gen Atenure         = 0.5*(tenure1+tenure2)
gen Dtransfer_out    = log(transfer_out1+1)-log(transfer_out2+1)
gen Dtransfer_in     = log(transfer_in1+1)-log(transfer_in2+1)
gen transfer_netout_mean = 0.5*(Dtransfer_out-Dtransfer_in)
gen transfer_netout_mean_num = 0.5*((transfer_out1-transfer_in1)+(transfer_in2-transfer_out2))

save twins_DIF.dta, replace


quiet reshape long
save twins_clean.dta, replace


drop if dz==1
save twins_mz_DIF.dta, replace

quiet reshape long
save twins_mz_clean.dta, replace


use twins_DIF.dta, clear
keep if dz==1
save twins_dz_DIF.dta, replace

quiet reshape long
save twins_dz_clean.dta, replace


*** use income variable and IV ***
use twins_DIF.dta, clear

drop if (income1==.)|(income2==.)
//drop if IVincome1==.
//drop if IVincome2==.

* IVFE-1
gen  IVDincome    = income_sib2-income_sib1
gen  IVDincome_squared = income_squared_sib2-income_squared_sib1

gen  IVDincome_num = exp(income_sib2)-exp(income_sib1)

gen  IVD_d_income_zero = d_income_zero_sib2-d_income_zero_sib1 

gen  IVD_d_income_below100 = d_income_below100_sib2-d_income_below100_sib1

gen  IVDIncrank   = IVIncrank2-IVIncrank1

gen  IVDedu       = edu_sib2-edu_sib1

* IVFE-2
gen  sDincome     = income1-income_sib1
gen  sDincome_squared = income_squared1-income_squared_sib1

gen  sIVDincome   = income_sib2-income2
gen  sIVDincome_squared = income_squared_sib2-income_squared2


gen  sD_d_income_zero   = d_income_zero1-d_income_zero_sib1
gen  sIVD_d_income_zero = d_income_zero_sib2-d_income_zero2

gen  sD_d_income_below100   = d_income_below1001-d_income_below100_sib1
gen  sIVD_d_income_below100 = d_income_below100_sib2-d_income_below1002

gen  sDIncrank    = Incrank1-IVIncrank1
gen  sIVDIncrank  = Incrank2-IVIncrank2 

//gen  sDedu        = edu1-edu_sib1
//gen  sIVDedu      = edu_sib2-edu2

//correct IV for income
replace  IVincome1    = income_sib2
replace  IVincome2    = income_sib1

replace  IVincome_squared1    = income_squared_sib2
replace  IVincome_squared2     = income_squared_sib1

replace IV_d_income_zero1 =    d_income_zero_sib2
replace IV_d_income_zero2 =    d_income_zero_sib1


replace IV_d_income_below1001 =    d_income_below100_sib2
replace IV_d_income_below1002 =    d_income_below100_sib1

gen IVIncrank_temp  = IVIncrank1
replace  IVIncrank1   = IVIncrank2
replace  IVIncrank2   = IVIncrank_temp

//IV for Edu
gen  DIVedu       = IVedu1-IVedu2

gen  sDedu        = edu1-edu_sib1
gen  sIVDedu      = edu_sib2-edu2 

replace  Aedu_own1        = (edu1+edu_sib2)/2
replace  Aedu_own2        = (edu2+edu_sib1)/2

quiet reshape long
save twins_IV_DIF.dta, replace

drop if dz==1
save twins_MZ_IV_DIF.dta, replace

use twins_IV_DIF.dta, clear
keep if dz==1
save twins_DZ_IV_DIF.dta, replace

