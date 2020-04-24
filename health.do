clear

cd "D:\Rati\Blog\Blog 21"

use CB2019_Georgia_response_30Jan2020.dta

******************* DV   *******************

///// TRUHLTH -- How much do you trust or distrust healthcare system?
gen health_trust = TRUHLTH  
recode health_trust (-9 / -1 = .) (1/2 = 1) (3=2) (4/5 = 3)

*************************************** IV   ****************************************

//// demographic vars: sett, age, gender, education, havejob, minority, internet, 
//// wealth index : ownearship of houshold items (utility)

////// religion related questions: RELSERV TRURELI


/// =================================================================================
/// recoding demographic variables 
/// =================================================================================

/// STRATUM
gen sett = STRATUM

/// RESPAGE
gen age = RESPAGE

/// gender
/// recoding Female from 2 to 0
gen gender = RESPSEX
recode gender (2=0) /// female = 0 

//// RESPEDU  => education 
/*  1 = secondary or lower 2 = secodanry technical 3 = higher */
gen education = RESPEDU
recode education (1/4 = 1) (5 = 2) (6/8 = 3) (-9 / -1 = .)

//// EMPLSIT => havejob 
/* 1 = empl 0 = no */
gen havejob = EMPLSIT
recode havejob (5/6 = 1) (1/4 = 0) (7/8 = 0) (-9 / -1 = . )

///  ETHNIC -- Ethnicity of the respondent  => minority
/* 0 = Georgian   1 = Non-Georgian   */
gen minority = ETHNIC
recode minority (4 / 7 = 1)  (3 =0) (2=1) (1=1) (-9 / -1 = .)

///// Internet exposure FRQINTR => internet
/* 1 = Every day 2 = Less often 3 = Never	 */
gen internet = FRQINTR
recode internet (1=1) (2/4 =2) (5/6 = 3) (-9 / -1 = .)


//// Wealth Index => utility
foreach var of varlist OWNCOTV OWNDIGC OWNWASH OWNFRDG OWNAIRC OWNCARS OWNCELL OWNCOMP {
gen `var'r = `var' 
}

foreach var of varlist OWNCOTVr OWNDIGCr OWNWASHr OWNFRDGr OWNAIRCr OWNCARSr OWNCELLr OWNCOMPr {
recode `var' (-9 / -1 = .)
}

gen utility = (OWNCOTVr + OWNDIGCr + OWNWASHr + OWNFRDGr + OWNAIRCr + OWNCARSr + OWNCELLr + OWNCOMPr)


////// religions questions
//// RELSERV

gen rel_serv = RELSERV 

recode rel_serv (-9 / -1 =.) (1 /4= 1) (5=2) (6/7=3) 

///// TRURELI -- How much do you trust or distrust religious institutions?
gen reli_trust = TRURELI  
recode reli_trust (-9 / -1 = .) (1/2 = 1) (3=2) (4/5 = 3)

/// TRUARMY -- How much do you trust or distrust army?
gen army_trust = TRUARMY  
recode army_trust (-9 / -1 = .) (1/2 = 1) (3=2) (4/5 = 3)

/// TRUPOLI -- How much do you trust or distrust police?
gen poli_trust = TRUPOLI  
recode poli_trust (-9 / -1 = .) (1/2 = 1) (3=2) (4/5 = 3)

/// TRUBANK -- How much do you trust or distrust banks?
gen bank_trust = TRUBANK  
recode bank_trust (-9 / -1 = .) (1/2 = 1) (3=2) (4/5 = 3)

/// TRUMEDI -- How much do you trust or distrust media?
gen media_trust = TRUMEDI  
recode media_trust (-9 / -1 = .) (1/2 = 1) (3=2) (4/5 = 3)



//// Weighting


svyset PSU [pweight=INDWT], strata(SUBSTRATUM) fpc(NPSUSS) singleunit(certainty) || ID, fpc(NHHPSU) || _n, fpc(NADHH)

stop
/// ============================================================================================================================
/// model 1: health_trust IV: i.sett age gender i.education havejob  minority internet utility
/// ============================================================================================================================


svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility 
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

svy: ologit health_trust  i.sett age gender i.education havejob  minority internet utility 
margins, at(sett=(1 2 3 ))
marginsplot


svy: ologit health_trust  i.sett age gender i.education havejob  minority internet utility 
margins, at(gender=(0 1 ))
marginsplot

svy: ologit health_trust  i.sett age gender i.education havejob  minority internet utility 
margins, at(minority=(0 1 ))
marginsplot

svy: ologit health_trust  i.sett age gender i.education havejob  minority internet utility 
margins, at(internet=(1 2 3))
marginsplot

/// settlement, internet and minority interactions
svy: ologit health_trust  i.sett#i.internet age gender i.education havejob minority utility 

svy: ologit health_trust  i.sett#minority age gender i.education havejob i.internet utility 

svy: ologit health_trust  i.sett i.internet#minority age gender i.education havejob utility 


/// ============================================================================================================================
/// model 2: health_trust IV: i.sett age gender i.education havejob  minority internet utility + religous attandance 
/// ============================================================================================================================

svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility  i.rel_serv
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility  i.rel_serv
margins, at(rel_serv=(1 2 3))
marginsplot



/// ============================================================================================================================
/// model 3: health_trust IV: i.sett age gender i.education havejob  minority internet utility + trureli
/// ============================================================================================================================

svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility  i.reli_trust
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility  i.reli_trust
margins, at(reli_trust=(1 2 3))
marginsplot


//====================================================================================================================
/// other institutions
//====================================================================================================================

svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility  i.army_trust
margins, dydx(*) post
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility  i.poli_trust

svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility  i.bank_trust

svy: ologit health_trust  i.sett age gender i.education havejob  minority i.internet utility  i.media_trust

//====================================================================================================================
/// trusting religious institutions
//====================================================================================================================

svy: ologit reli_trust  i.sett age gender i.education havejob  minority i.internet utility 


