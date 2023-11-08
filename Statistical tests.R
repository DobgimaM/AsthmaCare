# Statistical tests for analysis
# Age data

AgeW <- respi %>% filter(ethnicity_cat == 1) %>% select(age1)
AgeSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(age)
AgeBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(age)
AgeNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(age)

t_age1 <- t.test(AgeW,AgeSEA)
t_age2 <- t.test(AgeW,AgeBlk)
t_age3 <- t.test(AgeW,AgeNEA)

p_age1 <- round(t_age1$p.value,3)
p_age2 <- round(t_age2$p.value,3)
p_age3 <- round(t_age3$p.value,3)

#-------------------------------------------------------------------------------

FemW <- respi %>% filter(ethnicity_cat == 1) %>% select(female)
FemSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(female) #%>% rename(female1 = female)
FemBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(female)
FemNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(female)


t_Fem1 <- chisq.test(xtabs(FemW,FemSEA))
t_Fem2 <- chisq.test(xtabs(FemW,FemBlk))
t_Fem3 <- chisq.test(xtabs(FemW,FemNEA))

p_Fem1 <- round(t_Fem1$p.value,3)
p_Fem2 <- round(t_Fem2$p.value,3)
p_Fem3 <- round(t_Fem3$p.value,3)

#-------------------------------------------------------------------------------

NvsmkW <- respi %>% filter(ethnicity_cat == 1) %>% select(never_smoker)
NvsmkSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(never_smoker)
NvsmkBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(never_smoker)
NvsmkNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(never_smoker)

t_Nvsmk1 <- chisq.test(xtabs(NvsmkW,NvsmkSEA))
t_Nvsmk2 <- chisq.test(xtabs(NvsmkW,NvsmkBlk))
t_Nvsmk3 <- t.test(NvsmkW,NvsmkNEA)        # Should be chisq test with a larger sample size

p_Nvsmk1 <- round(t_Nvsmk1$p.value,3)
p_Nvsmk2 <- round(t_Nvsmk2$p.value,3)
p_Nvsmk3 <- round(t_Nvsmk3$p.value,3)

#-------------------------------------------------------------------------------

IMDqW <- respi %>% filter(ethnicity_cat == 1) %>% select(imd_quintile)
IMDqSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(imd_quintile)
IMDqBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(imd_quintile)
IMDqNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(imd_quintile)


t_IMDq1 <- chisq.test(xtabs(IMDqW,IMDqSEA))
t_IMDq2 <- chisq.test(xtabs(IMDqW,IMDqBlk))
t_IMDq3 <- t.test(IMDqW,IMDqNEA)        # Should be chisq test with a larger sample size

p_IMDq1 <- round(t_IMDq1$p.value,3)
p_IMDq2 <- round(t_IMDq2$p.value,3)
p_IMDq3 <- round(t_IMDq3$p.value,3)

#-------------------------------------------------------------------------------

IncW <- respi %>% filter(ethnicity_cat == 1) %>% select(income)
IncSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(income)
IncBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(income)
IncNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(income)

t_Inc1 <- chisq.test(xtabs(IncW,IncSEA))
t_Inc2 <- chisq.test(xtabs(IncW,IncBlk))
t_Inc3 <- chisq.test(xtabs(IncW,IncSEA))

p_Inc1 <- round(t_Inc1$p.value,3)
p_Inc2 <- round(t_Inc2$p.value,3)
p_Inc3 <- round(t_Inc3$p.value,3)

#p_Inc3 <- NA # Not enough values in the current dataset

#-------------------------------------------------------------------------------

UniEdW <- respi %>% filter(ethnicity_cat == 1) %>% select(highest_qual_cat)
UniEdSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(highest_qual_cat)
UniEdBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(highest_qual_cat)
UniEdNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(highest_qual_cat)

t_UniEd1 <- chisq.test(xtabs(UniEdW,UniEdSEA))
t_UniEd2 <- chisq.test(xtabs(UniEdW,UniEdBlk))
t_UniEd3 <- t.test(UniEdW,UniEdNEA)       # Should be chisq test with a larger sample size

p_UniEd1 <- round(t_UniEd1$p.value,3)
p_UniEd2 <- round(t_UniEd2$p.value,3)
p_UniEd3 <- round(t_UniEd3$p.value,3)

p_UniEd3 <- NA # Not enough values in the current dataset
#-------------------------------------------------------------------------------
chisq.test(xtabs(IMDqW,IMDqSEA))

EmReStW <- respi %>% filter(ethnicity_cat == 1) %>% select(employment_cat)
EmReStSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(employment_cat)
EmReStBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(employment_cat)
EmReStNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(employment_cat)

t_EmReSt1 <- chisq.test(xtabs(EmReStW,EmReStSEA))
t_EmReSt2 <- chisq.test(xtabs(EmReStW,EmReStBlk)) 
t_EmReSt3 <- chisq.test(xtabs(EmReStW,EmReStNEA)) 

p_EmReSt1 <- round(t_EmReSt1$p.value,3)
p_EmReSt2 <- round(t_EmReSt2$p.value,3)
p_EmReSt3 <- round(t_EmReSt3$p.value,3)

#-------------------------------------------------------------------------------


BMIW <- respi %>% filter(ethnicity_cat == 1) %>% select(bmi)
BMISEA <- respi %>% filter(ethnicity_cat == 2) %>% select(bmi)
BMIBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(bmi)
BMINEA <- respi %>% filter(ethnicity_cat == 4) %>% select(bmi)

t_BMI1 <- chisq.test(xtabs(BMIW,BMISEA))
t_BMI2 <- chisq.test(xtabs(BMIW,BMIBlk))
t_BMI3 <- chisq.test(xtabs(BMIW,BMINEA))

p_BMI1 <- round(t_BMI1$p.value,3)
p_BMI2 <- round(t_BMI2$p.value,3)
p_BMI3 <- round(t_BMI3$p.value,3)

#-------------------------------------------------------------------------------

BMI30W <- respi %>% filter(ethnicity_cat == 1) %>% select(bmi_cat)
BMI30SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(bmi_cat)
BMI30Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(bmi_cat)
BMI30NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(bmi_cat)

t_BMI301 <- chisq.test(xtabs(BMI30W,BMI30SEA))
t_BMI302 <- chisq.test(xtabs(BMI30W,BMI30Blk))
t_BMI303 <- t.test(BMI30W,BMI30NEA)

p_BMI301 <- round(t_BMI301$p.value,3)
p_BMI302 <- round(t_BMI302$p.value,3)
p_BMI303 <- round(t_BMI303$p.value,3)

#-------------------------------------------------------------------------------

ExMW <- respi %>% filter(ethnicity_cat == 1) %>% select(pa_overall_exmet)
ExMSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(pa_overall_exmet)
ExMBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(pa_overall_exmet)
ExMNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(pa_overall_exmet)

t_ExM1 <- chisq.test(xtabs(ExMW,ExMSEA))
t_ExM2 <- chisq.test(xtabs(ExMW,ExMBlk))
t_ExM3 <- chisq.test(xtabs(ExMW,ExMNEA))

p_ExM1 <- round(t_ExM1$p.value,3)
p_ExM2 <- round(t_ExM2$p.value,3)
p_ExM3 <- round(t_ExM3$p.value,3)

p_ExM3 <- NA

#-------------------------------------------------------------------------------

apFinW <- respi %>% filter(ethnicity_cat == 1) %>% select(air_pol_fine)
apFinSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(air_pol_fine)
apFinBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(air_pol_fine)
apFinNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(air_pol_fine)

t_apFin1 <- t.test(apFinW,apFinSEA)
t_apFin2 <- t.test(apFinW,apFinBlk)
t_apFin3 <- t.test(apFinW,apFinNEA)

p_apFin1 <- round(t_apFin1$p.value,3)
p_apFin2 <- round(t_apFin2$p.value,3)
p_apFin3 <- round(t_apFin3$p.value,3)

p_apFin3 <- NA

#-------------------------------------------------------------------------------

apCrsW <- respi %>% filter(ethnicity_cat == 1) %>% select(air_pol_coarse)
apCrsSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(air_pol_coarse)
apCrsBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(air_pol_coarse)
apCrsNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(air_pol_coarse)

t_apCrs1 <- t.test(apCrsW,apCrsSEA)
t_apCrs2 <- t.test(apCrsW,apCrsBlk)
t_apCrs3 <- t.test(apCrsW,apCrsNEA)

p_apCrs1 <- round(t_apCrs1$p.value,3)
p_apCrs2 <- round(t_apCrs2$p.value,3)
p_apCrs3 <- round(t_apCrs3$p.value,3)

p_apCrs3 <- NA

#-------------------------------------------------------------------------------

NO2W <- respi %>% filter(ethnicity_cat == 1) %>% select(air_pol_nit_dioxide)
NO2SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(air_pol_nit_dioxide)
NO2Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(air_pol_nit_dioxide)
NO2NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(air_pol_nit_dioxide)

t_NO21 <- t.test(NO2W,NO2SEA)
t_NO22 <- t.test(NO2W,NO2Blk)
t_NO23 <- t.test(NO2W,NO2NEA)

p_NO21 <- round(t_NO21$p.value,3)
p_NO22 <- round(t_NO22$p.value,3)
p_NO23 <- round(t_NO23$p.value,3)

#-------------------------------------------------------------------------------

NOxW <- respi %>% filter(ethnicity_cat == 1) %>% select(air_pol_nit_oxide)
NOxSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(air_pol_nit_oxide)
NOxBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(air_pol_nit_oxide)
NOxNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(air_pol_nit_oxide)

t_NOx1 <- t.test(NOxW,NOxSEA)
t_NOx2 <- t.test(NOxW,NOxBlk)
t_NOx3 <- t.test(NOxW,NOxNEA)

p_NOx1 <- round(t_NOx1$p.value,3)
p_NOx2 <- round(t_NOx2$p.value,3)
p_NOx3 <- round(t_NOx3$p.value,3)

#-------------------------------------------------------------------------------

HypW <- respi %>% filter(ethnicity_cat == 1) %>% select(c_hypertension)
HypSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(c_hypertension)
HypBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(c_hypertension)
HypNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(c_hypertension)

t_Hyp1 <- chisq.test(xtabs(HypW,HypSEA))
t_Hyp2 <- chisq.test(xtabs(HypW,HypBlk))
t_Hyp3 <- chisq.test(xtabs(HypW,HypNEA))

p_Hyp1 <- round(t_Hyp1$p.value,3)
p_Hyp2 <- round(t_Hyp2$p.value,3)
p_Hyp3 <- round(t_Hyp3$p.value,3)

#-------------------------------------------------------------------------------

PainCW <- respi %>% filter(ethnicity_cat == 1) %>% select(c_pain)
PainCSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(c_pain)
PainCBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(c_pain)
PainCNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(c_pain)

t_PainC1 <- chisq.test(xtabs(PainCW,PainCSEA))
t_PainC2 <- chisq.test(xtabs(PainCW,PainCBlk))
t_PainC3 <- chisq.test(xtabs(PainCW,PainCNEA))

p_PainC1 <- round(t_PainC1$p.value,3)
p_PainC2 <- round(t_PainC2$p.value,3)
p_PainC3 <- round(t_PainC3$p.value,3)

p_PainC3 <- NA
#-------------------------------------------------------------------------------

DysW <- respi %>% filter(ethnicity_cat == 1) %>% select(c_dyspepsia)
DysSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(c_dyspepsia)
DysBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(c_dyspepsia)
DysNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(c_dyspepsia)

t_Dys1 <- chisq.test(xtabs(DysW,DysSEA))
t_Dys2 <- chisq.test(xtabs(DysW,DysBlk))
t_Dys3 <- chisq.test(xtabs(DysW,DysNEA))

p_Dys1 <- round(t_Dys1$p.value,3)
p_Dys2 <- round(t_Dys2$p.value,3)
p_Dys3 <- round(t_Dys3$p.value,3)

p_Dys1 <- NA
p_Dys2 <- NA
p_Dys3 <- NA


#-------------------------------------------------------------------------------

DepW <- respi %>% filter(ethnicity_cat == 1) %>% select(c_depression)
DepSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(c_depression)
DepBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(c_depression)
DepNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(c_depression)

t_Dep1 <- chisq.test(xtabs(DepW,DepSEA))
t_Dep2 <- chisq.test(xtabs(DepW,DepBlk))
t_Dep3 <- chisq.test(xtabs(DepW,DepNEA))

p_Dep1 <- round(t_Dep1$p.value,3)
p_Dep2 <- round(t_Dep2$p.value,3)
p_Dep3 <- round(t_Dep3$p.value,3)

p_Dep1 <- NA
p_Dep3 <- NA

#-------------------------------------------------------------------------------

EczW <- respi %>% filter(ethnicity_cat == 1) %>% select(c_eczema)
EczSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(c_eczema)
EczBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(c_eczema)
EczNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(c_eczema)

t_Ecz1 <- chisq.test(xtabs(EczW,EczSEA))
t_Ecz2 <- chisq.test(xtabs(EczW,EczBlk))
t_Ecz3 <- chisq.test(xtabs(EczW,EczNEA))

p_Ecz1 <- round(t_Ecz1$p.value,3)
p_Ecz2 <- round(t_Ecz2$p.value,3)
p_Ecz3 <- round(t_Ecz3$p.value,3)

p_Ecz1 <- NA
p_Ecz2 <- NA
p_Ecz3 <- NA

#-------------------------------------------------------------------------------
#Non-asthma disease

Nasth1W <- respi %>% filter(ethnicity_cat == 1) %>% select(c_count)
Nasth1SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(c_count)
Nasth1Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(c_count)
Nasth1NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(c_count)

t_Nasth11 <- chisq.test(xtabs(Nasth1W,Nasth1SEA))
t_Nasth12 <- chisq.test(xtabs(Nasth1W,Nasth1Blk))
t_Nasth13 <- chisq.test(xtabs(Nasth1W,Nasth1NEA))

p_Nasth11 <- round(t_Nasth11$p.value,3)
p_Nasth12 <- round(t_Nasth12$p.value,3)
p_Nasth13 <- round(t_Nasth13$p.value,3)

#-------------------------------------------------------------------------------

Asth_ageW <- respi %>% filter(ethnicity_cat == 1) %>% select(asthma_onset)
Asth_ageSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(asthma_onset)
Asth_ageBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(asthma_onset)
Asth_ageNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(asthma_onset)

t_Asth_age1 <- t.test(Asth_ageW,Asth_ageSEA)
t_Asth_age2 <- t.test(Asth_ageW,Asth_ageBlk)
t_Asth_age3 <- t.test(Asth_ageW,Asth_ageNEA)

p_Asth_age1 <- round(t_Asth_age1$p.value,3)
p_Asth_age2 <- round(t_Asth_age2$p.value,3)
p_Asth_age3 <- round(t_Asth_age3$p.value,3)

p_Asth_age1 <- NA
p_Asth_age3 <- NA

#-------------------------------------------------------------------------------

SABAW <- respi %>% filter(ethnicity_cat == 1) %>% select(m_saba)
SABASEA <- respi %>% filter(ethnicity_cat == 2) %>% select(m_saba)
SABABlk <- respi %>% filter(ethnicity_cat == 3) %>% select(m_saba)
SABANEA <- respi %>% filter(ethnicity_cat == 4) %>% select(m_saba)

t_SABA1 <- chisq.test(xtabs(SABAW,SABASEA))
t_SABA2 <- chisq.test(xtabs(SABAW,SABABlk))
t_SABA3 <- chisq.test(xtabs(SABAW,SABANEA))

p_SABA1 <- round(t_SABA1$p.value,3)
p_SABA2 <- round(t_SABA2$p.value,3)
p_SABA3 <- round(t_SABA3$p.value,3)

p_SABA1 <- NA
p_SABA3 <- NA

#-------------------------------------------------------------------------------

ICSW <- respi %>% filter(ethnicity_cat == 1) %>% select(m_ics)
ICSSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(m_ics)
ICSBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(m_ics)
ICSNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(m_ics)

t_ICS1 <- chisq.test(xtabs(ICSW,ICSSEA))
t_ICS2 <- chisq.test(xtabs(ICSW,ICSBlk))
t_ICS3 <- chisq.test(xtabs(ICSW,ICSNEA))

p_ICS1 <- round(t_ICS1$p.value,3)
p_ICS2 <- round(t_ICS2$p.value,3)
p_ICS3 <- round(t_ICS3$p.value,3)

p_ICS1 <- NA
p_ICS2 <- NA
p_ICS3 <- NA

#-------------------------------------------------------------------------------

OCSW <- respi %>% filter(ethnicity_cat == 1) %>% select(m_ocs)
OCSSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(m_ocs)
OCSBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(m_ocs)
OCSNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(m_ocs)

t_OCS1 <- chisq.test(xtabs(OCSW,OCSSEA))
t_OCS2 <- chisq.test(xtabs(OCSW,OCSBlk))
t_OCS3 <- chisq.test(xtabs(OCSW,OCSNEA))

p_OCS1 <- round(t_OCS1$p.value,3)
p_OCS2 <- round(t_OCS2$p.value,3)
p_OCS3 <- round(t_OCS3$p.value,3)

p_OCS1 <- NA
p_OCS2 <- NA
p_OCS3 <- NA


#-------------------------------------------------------------------------------

pkflwW <- respi %>% filter(ethnicity_cat == 1) %>% select(peak_flow)
pkflwSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(peak_flow)
pkflwBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(peak_flow)
pkflwNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(peak_flow)

t_pkflw1 <- t.test(pkflwW,pkflwSEA)
t_pkflw2 <- t.test(pkflwW,pkflwBlk)
t_pkflw3 <- t.test(pkflwW,pkflwNEA)

p_pkflw1 <- round(t_pkflw1$p.value,3)
p_pkflw2 <- round(t_pkflw2$p.value,3)
p_pkflw3 <- round(t_pkflw3$p.value,3)

#-------------------------------------------------------------------------------

FEV1W <- respi %>% filter(ethnicity_cat == 1) %>% select(fev1)
FEV1SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(fev1)
FEV1Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(fev1)
FEV1NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(fev1)

t_FEV11 <- t.test(FEV1W,FEV1SEA)
t_FEV12 <- t.test(FEV1W,FEV1Blk)
t_FEV13 <- t.test(FEV1W,FEV1NEA)

p_FEV11 <- round(t_FEV11$p.value,3)
p_FEV12 <- round(t_FEV12$p.value,3)
p_FEV13 <- round(t_FEV13$p.value,3)

#-------------------------------------------------------------------------------

FEVllnW <- respi %>% filter(ethnicity_cat == 1) %>% select(lln_fev1)
FEVllnSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(lln_fev1)
FEVllnBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(lln_fev1)
FEVllnNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(lln_fev1)

t_FEVlln1 <- t.test(FEVllnW,FEVllnSEA)
t_FEVlln2 <- t.test(FEVllnW,FEVllnBlk)
t_FEVlln3 <- t.test(FEVllnW,FEVllnNEA)

p_FEVlln1 <- round(t_FEVlln1$p.value,3)
p_FEVlln2 <- round(t_FEVlln2$p.value,3)
p_FEVlln3 <- round(t_FEVlln3$p.value,3)

#-------------------------------------------------------------------------------

FVCW <- respi %>% filter(ethnicity_cat == 1) %>% select(fvc)
FVCSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(fvc)
FVCBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(fvc)
FVCNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(fvc)

t_FVC1 <- t.test(FVCW,FVCSEA)
t_FVC2 <- t.test(FVCW,FVCBlk)
t_FVC3 <- t.test(FVCW,FVCNEA)

p_FVC1 <- round(t_FVC1$p.value,3)
p_FVC2 <- round(t_FVC2$p.value,3)
p_FVC3 <- round(t_FVC3$p.value,3)

#-------------------------------------------------------------------------------

ffratioW <- respi %>% filter(ethnicity_cat == 1) %>% select(fev1_fvc_ratio)
ffratioSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(fev1_fvc_ratio)
ffratioBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(fev1_fvc_ratio)
ffratioNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(fev1_fvc_ratio)

t_ffratio1 <- t.test(ffratioW,ffratioSEA)
t_ffratio2 <- t.test(ffratioW,ffratioBlk)
t_ffratio3 <- t.test(ffratioW,ffratioNEA)

p_ffratio1 <- round(t_ffratio1$p.value,3)
p_ffratio2 <- round(t_ffratio2$p.value,3)
p_ffratio3 <- round(t_ffratio3$p.value,3)

#-------------------------------------------------------------------------------

EosW <- respi %>% filter(ethnicity_cat == 1) %>% select(blood_eos)
EosSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(blood_eos)
EosBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(blood_eos)
EosNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(blood_eos)

t_Eos1 <- wilcox.test(EosW$blood_eos,EosSEA$blood_eos)
t_Eos2 <- wilcox.test(EosW$blood_eos,EosBlk$blood_eos)
t_Eos3 <- wilcox.test(EosW$blood_eos,EosNEA$blood_eos)

p_Eos1 <- round(t_Eos1$p.value,3)
p_Eos2 <- round(t_Eos2$p.value,3)
p_Eos3 <- round(t_Eos3$p.value,3)


#-------------------------------------------------------------------------------

Eos300W <- respi %>% filter(ethnicity_cat == 1) %>% select(blood_eos_cat)
Eos300SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(blood_eos_cat)
Eos300Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(blood_eos_cat)
Eos300NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(blood_eos_cat)


t_Eos3001 <- chisq.test(xtabs(Eos300W,Eos300SEA))
t_Eos3002 <- chisq.test(xtabs(Eos300W,Eos300Blk))
t_Eos3003 <- t.test(Eos300W,Eos300NEA)

p_Eos3001 <- round(t_Eos3001$p.value,3)
p_Eos3002 <- round(t_Eos3002$p.value,3)
p_Eos3003 <- round(t_Eos3003$p.value,3)

p_Eos3003 <-NA

#-------------------------------------------------------------------------------

AstHosW <- respi %>% filter(ethnicity_cat == 1) %>% select(asthma_admission)
AstHosSEA <- respi %>% filter(ethnicity_cat == 2) %>% select(asthma_admission)
AstHosBlk <- respi %>% filter(ethnicity_cat == 3) %>% select(asthma_admission)
AstHosNEA <- respi %>% filter(ethnicity_cat == 4) %>% select(asthma_admission)

t_AstHos1 <- t.test(AstHosW,AstHosSEA)
t_AstHos2 <- t.test(AstHosW,AstHosBlk)
t_AstHos3 <- t.test(AstHosW,AstHosNEA)

p_AstHos1 <- round(t_AstHos1$p.value,3)
p_AstHos2 <- round(t_AstHos2$p.value,3)
p_AstHos3 <- round(t_AstHos3$p.value,3)

noms <-c('Age', 'Female', 'Nvsmk', 'IMD1', 'Income31k', 'Uni_edu', 'EmReSt', 
         'BMI', 'BMI30', 'ExMET', 'ap_Fine', 'Coarse', 'NO2', 'NOx', 'Hyp', 
         'Pain', 'Dys', 'Dep', 'PsoEcz', 'AgeAsth', 'SABA', 'ICS', 'OCS', 
         'Pkflow', 'FEV1', '<LLN', 'FVC' , 'FEV1/FVC', 'Eos', 'Eos300', 'Asthhos')

file.edit('clean_table.R')
