library(haven)
library(foreign)
library(tidyverse)

#-------------------------------------------------------------------------------

# setwd("C:/Users/Dob/Desktop/HDRUK Internship/Research/Data")
list.files()

respi <- read_dta("dob.dta")


vars <- data.frame("name" = names(respi),
                   "label" = sapply(respi, function(x) attr(x, "label"))  %>% as.character(),
                   "labelled" = sapply(respi, is.labelled) )

resp <- as_tibble(respi)

attach(respi)

names(resp)
dim(resp)
str(resp)

resp$predicted_ave_fev1 %>% summary()


length(!duplicated(resp$patient_id))
# N = 514

summary(resp$age)
# Aged 40.28 - 70.20, mean = 57.3

table(resp$gender)
# 0 = 286 1 = 228

table(resp$asthma)
# 0 = 107 1 = 13
table(resp$ethnicity)

table(resp$country_birth)
table(resp$asthma_admission)
#table(resp$asthma_onset)
table(resp$health_rating)
table(resp$health_rating_cat)
table(resp$imd_decile)
table(resp$asthma_admission)
table(resp$income)
table(resp$income_cat)
table(resp$age_education)
table(resp$employment_cat)
table(resp$employment_0)
table(resp$employment_1)
table(resp$ethnicity_dicot)

respi$ethnicity <- factor(respi$ethnicity)

respi <- respi %>% mutate(Nonasthma = ifelse(c_count > 0,1,0 ))

t1 <- respi %>% group_by(ethnicity_cat) %>% 
  summarise(no = n(), Age_yrs = mean(age), sd_age = sd(age),
            tot_fem = sum(female),
            nv_smk = sum(never_smoker),
            income_31k = sum(income_cat, na.rm = T),
            bmi = mean(bmi, na.rm = T), sd_bmi = sd(bmi, na.rm = TRUE),
            ex_met = mean(pa_overall_exmet, na.rm = T),
            ap_fin = mean(air_pol_fine, na.rm = T), sd_fin = sd(air_pol_fine, na.rm = TRUE), 
            ap_coarse = mean(air_pol_coarse,  na.rm = T),sd_coarse = sd(air_pol_coarse, na.rm = TRUE), 
            NO2 = mean(air_pol_nit_dioxide, na.rm = T), sd_NO2 = sd(air_pol_nit_dioxide, na.rm = TRUE),
            NOx = mean(air_pol_nit_oxide, na.rm = T), sd_NOx = sd(air_pol_nit_oxide, na.rm = TRUE),
            hypertension = sum(c_hypertension), painful_cond = sum(c_pain), dyspep = sum(c_dyspepsia),
            depres = sum(c_depression),ecz = sum(c_eczema),
            saba = sum(m_saba), ics = sum(m_ics),
            asth_onage = mean(asthma_onset, na.rm =T), asth_sd = sd(asthma_onset, na.rm = T),
            ocs = sum(m_ocs)) #%>% view()


t2 <- respi %>% group_by(ethnicity_cat) %>% 
  summarise(pk_flow = mean(peak_flow, na.rm = T), sd_pkflw = sd(peak_flow, na.rm = TRUE),
            FEV1 = mean(fev1, na.rm = T), sd_fev1 = sd(fev1, na.rm = TRUE),
            FVC = mean(fvc, na.rm = T), sd_fvc = sd(fvc, na.rm = T),
            lln_fev1 = mean(lln_fev1, na.rm = T), sd_lln_fev1 = sd(lln_fev1, na.rm = TRUE),
            fev1fvc_ratio = mean(fev1_fvc_ratio, na.rm = T),
            Eos_cnt = mean(blood_eos, na.rm = T), Eos_sd = sd(blood_eos, na.rm = T),
            Eos_300ul = mean(blood_eos_cat, na.rm = T), Eos_300_sd = sd(blood_eos_cat, na.rm = T),
            Asthma_hos = sum(asthma_admission),
            NonAsthma = sum(Nonasthma)) 

t3 <- left_join(t1,t2, by ='ethnicity_cat')            
t4 <- as.data.frame(t(t3)) %>% round(1) %>% slice(-1) %>% select(-c(5,6))
names(t4) <- c( "White", "SE Asian", "Black", "NE Asian")
#view(t4)

respi %>% group_by(ethnicity_cat) %>% summarise(asth_age = mean(asthma_onset, na.rm = T))
respi %>% group_by(ethnicity_cat) %>% summarise(asth_sd = sd(asthma_onset, na.rm = T))

respi %>% group_by(ethnicity_cat) %>% filter(imd_quintile ==1) %>% summarise(IMD = sum( na.rm = T))
#-------------------------------------------------------------------------------

# Females
fem_W <- respi %>% filter(ethnicity_cat == 1) %>% select(female) %>% sum() #269
fem_SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(female) %>% sum() #1
fem_Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(female) %>% sum() #9
fem_NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(female) %>% sum() #1
tot_W <- respi %>% filter(ethnicity_cat == 1) %>%  nrow() #286
tot_SEA <- respi %>% filter(ethnicity_cat == 2) %>%  nrow()
tot_Blk <- respi %>% filter(ethnicity_cat == 3) %>%  nrow()
tot_NEA <- respi %>% filter(ethnicity_cat == 4) %>%  nrow()



round(fem_W/tot_W,2)        
round(fem_SEA/tot_SEA,2)        
round(fem_Blk/tot_Blk,2)        
round(fem_NEA/tot_NEA,2)        

# prop.table(c(fem_W,fem_Blk,fem_SEA,fem_NEA)) %>% round(2)
sum(fem_W,fem_Blk,fem_SEA,fem_NEA)

# Smokers
Smk_W <- respi %>% filter(ethnicity_cat == 1) %>% select(never_smoker) %>% sum() #269
Smk_SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(never_smoker) %>% sum() #1
Smk_Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(never_smoker) %>% sum() #9
Smk_NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(never_smoker) %>% sum() #1
Smk_tot <- respi %>% select(never_smoker) %>% sum() #262

round(Smk_W/tot_W,2)        #0.92
round(Smk_SEA/tot_SEA,2)        #0.03
round(Smk_Blk/tot_Blk,2)        #0.03
round(Smk_NEA/tot_NEA,2)        #0.01

#-------------------------------------------------------------------------------
# IMD
# IMD least deprived
IMD_W <- respi %>% filter(imd_quintile ==0) %>% filter(ethnicity_cat == 1) %>% select(imd_quintile) %>% nrow() #269
IMD_SEA <- respi %>% filter(imd_quintile ==0) %>% filter(ethnicity_cat == 2) %>% select(imd_quintile)  %>% nrow() #1
IMD_Blk <- respi %>% filter(imd_quintile ==0) %>% filter(ethnicity_cat == 3) %>% select(imd_quintile) %>% nrow() #9
IMD_NEA <- respi %>% filter(imd_quintile ==0) %>% filter(ethnicity_cat == 4) %>% select(imd_quintile)  %>% nrow() #1
IMD_tot <- respi %>% filter(imd_quintile ==0) %>%  select(imd_quintile) %>% nrow() #262

c(IMD_W,IMD_SEA,IMD_Blk,IMD_NEA) 

round(IMD_W/tot_W,2)        #0.97
round(IMD_SEA/tot_SEA,2)        #0.02
round(IMD_Blk/tot_Blk,2)        #0.01
round(IMD_NEA/tot_NEA,2)        #0.00
tot_W

# IMD 2
IMD2_W <- respi %>% filter(imd_quintile ==1) %>% filter(ethnicity_cat == 1) %>% select(imd_quintile) %>% nrow() #269
IMD2_SEA <- respi %>% filter(imd_quintile ==1) %>% filter(ethnicity_cat == 2) %>% select(imd_quintile)  %>% nrow() #1
IMD2_Blk <- respi %>% filter(imd_quintile ==1) %>% filter(ethnicity_cat == 3) %>% select(imd_quintile) %>% nrow() #9
IMD2_NEA <- respi %>% filter(imd_quintile ==1) %>% filter(ethnicity_cat == 4) %>% select(imd_quintile)  %>% nrow() #1
IMD2_tot <- respi %>% filter(imd_quintile ==1) %>%  select(imd_quintile) %>% nrow() #262

c(IMD2_W,IMD2_SEA,IMD2_Blk,IMD2_NEA) 

round(IMD_W/tot_W,2)        #0.97
round(IMD_SEA/tot_SEA,2)        #0.02
round(IMD_Blk/tot_Blk,2)        #0.01
round(IMD_NEA/tot_NEA,2)        #0.00

# IMD 3
IMD3_W <- respi %>% filter(imd_quintile ==2) %>% filter(ethnicity_cat == 1) %>% select(imd_quintile) %>% nrow() #269
IMD3_SEA <- respi %>% filter(imd_quintile ==2) %>% filter(ethnicity_cat == 2) %>% select(imd_quintile)  %>% nrow() #1
IMD3_Blk <- respi %>% filter(imd_quintile ==2) %>% filter(ethnicity_cat == 3) %>% select(imd_quintile) %>% nrow() #9
IMD3_NEA <- respi %>% filter(imd_quintile ==2) %>% filter(ethnicity_cat == 4) %>% select(imd_quintile)  %>% nrow() #1
IMD3_tot <- respi %>% filter(imd_quintile ==2) %>%  select(imd_quintile) %>% nrow() #262

c(IMD3_W,IMD3_SEA,IMD3_Blk,IMD3_NEA) 

round(IMD_W/tot_W,2)        #0.97
round(IMD_SEA/tot_SEA,2)        #0.02
round(IMD_Blk/tot_Blk,2)        #0.01
round(IMD_NEA/tot_NEA,2)        #0.00

# IMD 4
IMD4_W <- respi %>% filter(imd_quintile ==3) %>% filter(ethnicity_cat == 1) %>% select(imd_quintile) %>% nrow() #269
IMD4_SEA <- respi %>% filter(imd_quintile ==3) %>% filter(ethnicity_cat == 2) %>% select(imd_quintile)  %>% nrow() #1
IMD4_Blk <- respi %>% filter(imd_quintile ==3) %>% filter(ethnicity_cat == 3) %>% select(imd_quintile) %>% nrow() #9
IMD4_NEA <- respi %>% filter(imd_quintile ==3) %>% filter(ethnicity_cat == 4) %>% select(imd_quintile)  %>% nrow() #1
IMD4_tot <- respi %>% filter(imd_quintile ==3) %>%  select(imd_quintile) %>% nrow() #262

c(IMD4_W,IMD4_SEA,IMD4_Blk,IMD4_NEA) 

round(IMD_W/tot_W,2)        #0.97
round(IMD_SEA/tot_SEA,2)        #0.02
round(IMD_Blk/tot_Blk,2)        #0.01
round(IMD_NEA/tot_NEA,2)        #0.00

# IMD 5
IMD5_W <- respi %>% filter(imd_quintile ==4) %>% filter(ethnicity_cat == 1) %>% select(imd_quintile) %>% nrow() #269
IMD5_SEA <- respi %>% filter(imd_quintile ==4) %>% filter(ethnicity_cat == 2) %>% select(imd_quintile)  %>% nrow() #1
IMD5_Blk <- respi %>% filter(imd_quintile ==4) %>% filter(ethnicity_cat == 3) %>% select(imd_quintile) %>% nrow() #9
IMD5_NEA <- respi %>% filter(imd_quintile ==4) %>% filter(ethnicity_cat == 4) %>% select(imd_quintile)  %>% nrow() #1
IMD5_tot <- respi %>% filter(imd_quintile ==4) %>%  select(imd_quintile) %>% nrow() #262

c(IMD5_W,IMD5_SEA,IMD5_Blk,IMD5_NEA) 

round(IMD5_W/tot_W,2)        #0.97
round(IMD5_SEA/tot_SEA,2)        #0.02
round(IMD5_Blk/tot_Blk,2)        #0.01
round(IMD5_NEA/tot_NEA,2)        #0.00
IMD_tot

#-------------------------------------------------------------------------------
# Income >= 31k
Inc_W <- respi %>% filter(income_cat ==1) %>% filter(ethnicity_cat == 1) %>% nrow() #269
Inc_SEA <- respi %>% filter(income_cat ==1) %>% filter(ethnicity_cat == 2) %>% nrow()
Inc_Blk <- respi %>% filter(income_cat ==1) %>% filter(ethnicity_cat == 3) %>% nrow()
Inc_NEA <- respi %>% filter(income_cat ==1) %>% filter(ethnicity_cat == 4) %>% nrow()
Inc_tot <- respi %>% filter(income_cat ==1) %>%  nrow()

c(Inc_W,Inc_SEA,Inc_Blk,Inc_NEA) 

round(Inc_W/tot_W,2)        #0.97
round(Inc_SEA/tot_SEA,2)        #0.02
round(Inc_Blk/tot_Blk,2)        #0.01
round(Inc_NEA/tot_NEA,2)        #0.00

#-------------------------------------------------------------------------------
# University educated
Uni_W <- respi %>% filter(highest_qual_cat ==1) %>% filter(ethnicity_cat == 1) %>% nrow() #269
Uni_SEA <- respi %>% filter(highest_qual_cat ==1) %>% filter(ethnicity_cat == 2) %>% nrow()
Uni_Blk <- respi %>% filter(highest_qual_cat ==1) %>% filter(ethnicity_cat == 3) %>% nrow()
Uni_NEA <- respi %>% filter(highest_qual_cat ==1) %>% filter(ethnicity_cat == 4) %>% nrow()
Uni_tot <- respi %>% filter(highest_qual_cat ==1) %>%  nrow()

c(Uni_W,Uni_SEA,Uni_Blk,Uni_NEA) 

round(Uni_W/tot_W,2)        #0.97
round(Uni_SEA/tot_SEA,2)        #0.02
round(Uni_Blk/tot_Blk,2)        #0.01
round(Uni_NEA/tot_NEA,2)        #0.00

#-------------------------------------------------------------------------------
# Employed/retired/student
Emp_W <- respi %>% filter(employment_cat ==1) %>% filter(ethnicity_cat == 1) %>% nrow() #269
Emp_SEA <- respi %>% filter(employment_cat ==1) %>% filter(ethnicity_cat == 2) %>% nrow()
Emp_Blk <- respi %>% filter(employment_cat ==1) %>% filter(ethnicity_cat == 3) %>% nrow()
Emp_NEA <- respi %>% filter(employment_cat ==1) %>% filter(ethnicity_cat == 4) %>% nrow()
Emp_tot <- respi %>% filter(employment_cat ==1) %>%  nrow()

c(Emp_W,Emp_SEA,Emp_Blk,Emp_NEA) 

round(Emp_W/tot_W,2)    
round(Emp_SEA/tot_SEA,2)
round(Emp_Blk/tot_Blk,2)
round(Emp_NEA/tot_NEA,2)

#-------------------------------------------------------------------------------

# BMI
bmi_sd <- respi %>% group_by(ethnicity_cat) %>% 
  summarise(bmi = sd(bmi, na.rm = T)) %>% 
  slice(-c(5,6)) %>%  round(2)


respi %>% group_by(ethnicity_cat) %>% 
  filter(bmi_cat == 2) %>% 
  select(bmi_cat) %>%  table()

bmisd_W <- bmi_sd[1,2] %>% round(2)
bmisd_SEA <- bmi_sd[2,2] %>% round(2)
bmisd_Blk <- bmi_sd[3,2] %>% round(2)
bmisd_NEA <- bmi_sd[4,2] %>% round(2)

#-------------------------------------------------------------------------------
# llnfev
llnfev_sd <- respi %>% group_by(ethnicity_cat) %>% 
  summarise(bmi = sd(lln_fev1, na.rm = T)) %>% 
  round(2) %>% slice(-c(5,6))

#-------------------------------------------------------------------------------

BMI30_W <- respi %>% filter(bmi_cat ==2) %>% filter(ethnicity_cat == 1) %>% nrow() #269
BMI30_SEA <- respi %>% filter(bmi_cat ==2) %>% filter(ethnicity_cat == 2) %>% nrow()
BMI30_Blk <- respi %>% filter(bmi_cat ==2) %>% filter(ethnicity_cat == 3) %>% nrow()
BMI30_NEA <- respi %>% filter(bmi_cat ==2) %>% filter(ethnicity_cat == 4) %>% nrow()

c(BMI30_W,BMI30_SEA,BMI30_Blk,BMI30_NEA)

round(BMI30_W/tot_W,2)    
round(BMI30_SEA/tot_SEA,2)
round(BMI30_Blk/tot_Blk,2)
round(BMI30_NEA/tot_NEA,2)

#-------------------------------------------------------------------------------

# Excess MET
Ex_MET <- respi %>% group_by(ethnicity_cat) %>% 
  summarise(met_median = median(pa_overall_exmet, na.rm = T),
            met_IQR = IQR(pa_overall_exmet, na.rm = T)) %>% 
  round(2) %>% slice(-c(5,6))


#-------------------------------------------------------------------------------
#    Comorbidities
# Hypertension
Hyp_W <- respi %>% filter(c_hypertension ==1) %>% filter(ethnicity_cat == 1) %>% select(c_hypertension) %>% nrow() 
Hyp_SEA <- respi %>% filter(c_hypertension ==1) %>% filter(ethnicity_cat == 2) %>% select(c_hypertension)  %>% nrow()
Hyp_Blk <- respi %>% filter(c_hypertension ==1) %>% filter(ethnicity_cat == 3) %>% select(c_hypertension) %>% nrow() 
Hyp_NEA <- respi %>% filter(c_hypertension ==1) %>% filter(ethnicity_cat == 4) %>% select(c_hypertension)  %>% nrow()

c(Hyp_W,Hyp_SEA,Hyp_Blk,Hyp_NEA) 

round(Hyp_W/tot_W,2)        
round(Hyp_SEA/tot_SEA,2)      
round(Hyp_Blk/tot_Blk,2)        
round(Hyp_NEA/tot_NEA,2)        

# Painful conditions
Pain_W <- respi %>% filter(c_pain ==1) %>% filter(ethnicity_cat == 1) %>% select(c_pain) %>% nrow() 
Pain_SEA <- respi %>% filter(c_pain ==1) %>% filter(ethnicity_cat == 2) %>% select(c_pain)  %>% nrow() 
Pain_Blk <- respi %>% filter(c_pain ==1) %>% filter(ethnicity_cat == 3) %>% select(c_pain) %>% nrow() 
Pain_NEA <- respi %>% filter(c_pain ==1) %>% filter(ethnicity_cat == 4) %>% select(c_pain)  %>% nrow()

c(Pain_W,Pain_SEA,Pain_Blk,Pain_NEA) 

round(Pain_W/tot_W,2)        
round(Pain_SEA/tot_SEA,2)      
round(Pain_Blk/tot_Blk,2)        
round(Pain_NEA/tot_NEA,2)        

# Dyspepsia
Dys_W <- respi %>% filter(c_dyspepsia ==1) %>% filter(ethnicity_cat == 1) %>% select(c_dyspepsia) %>% nrow() 
Dys_SEA <- respi %>% filter(c_dyspepsia ==1) %>% filter(ethnicity_cat == 2) %>% select(c_dyspepsia)  %>% nrow() 
Dys_Blk <- respi %>% filter(c_dyspepsia ==1) %>% filter(ethnicity_cat == 3) %>% select(c_dyspepsia) %>% nrow() 
Dys_NEA <- respi %>% filter(c_dyspepsia ==1) %>% filter(ethnicity_cat == 4) %>% select(c_dyspepsia)  %>% nrow()

c(Dys_W,Dys_SEA,Dys_Blk,Dys_NEA) 

round(Dys_W/tot_W,2)        
round(Dys_SEA/tot_SEA,2)      
round(Dys_Blk/tot_Blk,2)        
round(Dys_NEA/tot_NEA,2)        

# Depression
Dep_W <- respi %>% filter(c_depression ==1) %>% filter(ethnicity_cat == 1) %>% select(c_depression) %>% nrow() 
Dep_SEA <- respi %>% filter(c_depression ==1) %>% filter(ethnicity_cat == 2) %>% select(c_depression)  %>% nrow() 
Dep_Blk <- respi %>% filter(c_depression ==1) %>% filter(ethnicity_cat == 3) %>% select(c_depression) %>% nrow() 
Dep_NEA <- respi %>% filter(c_depression ==1) %>% filter(ethnicity_cat == 4) %>% select(c_depression)  %>% nrow()

c(Dep_W,Dep_SEA,Dep_Blk,Dep_NEA) 

round(Dep_W/tot_W,2)        
round(Dep_SEA/tot_SEA,2)      
round(Dep_Blk/tot_Blk,2)        
round(Dep_NEA/tot_NEA,2)        

# Psoriasis/eczema
Ecz_W <- respi %>% filter(c_eczema ==1) %>% filter(ethnicity_cat == 1) %>% select(c_eczema) %>% nrow() 
Ecz_SEA <- respi %>% filter(c_eczema ==1) %>% filter(ethnicity_cat == 2) %>% select(c_eczema)  %>% nrow() 
Ecz_Blk <- respi %>% filter(c_eczema ==1) %>% filter(ethnicity_cat == 3) %>% select(c_eczema) %>% nrow() 
Ecz_NEA <- respi %>% filter(c_eczema ==1) %>% filter(ethnicity_cat == 4) %>% select(c_eczema)  %>% nrow()

c(Ecz_W,Ecz_SEA,Ecz_Blk,Ecz_NEA) 

round(Ecz_W/tot_W,2)        
round(Ecz_SEA/tot_SEA,2)      
round(Ecz_Blk/tot_Blk,2)        
round(Ecz_NEA/tot_NEA,2)        

#-------------------------------------------------------------------------------

# Excess MET
asth_age <- respi %>% group_by(ethnicity_cat) %>% 
  summarise(avg = mean(asthma_onset, na.rm = T),
            stdv = sd(asthma_onset, na.rm = T)) %>% 
  round(2) %>% slice(-c(5,6))

#-------------------------------------------------------------------------------
# Asthma medication
# SABA
SABA_W <- respi %>% filter(m_saba ==1) %>% filter(ethnicity_cat == 1) %>% select(m_saba) %>% nrow() 
SABA_SEA <- respi %>% filter(m_saba ==1) %>% filter(ethnicity_cat == 2) %>% select(m_saba)  %>% nrow() 
SABA_Blk <- respi %>% filter(m_saba ==1) %>% filter(ethnicity_cat == 3) %>% select(m_saba) %>% nrow() 
SABA_NEA <- respi %>% filter(m_saba ==1) %>% filter(ethnicity_cat == 4) %>% select(m_saba)  %>% nrow()

c(SABA_W,SABA_SEA,SABA_Blk,SABA_NEA) 

round(SABA_W/tot_W,2)        
round(SABA_SEA/tot_SEA,2)      
round(SABA_Blk/tot_Blk,2)        
round(SABA_NEA/tot_NEA,2)        

#-------------------------------------------------------------------------------
# ICS
ICS_W <- respi %>% filter(m_ics ==1) %>% filter(ethnicity_cat == 1) %>% select(m_ics) %>% nrow() 
ICS_SEA <- respi %>% filter(m_ics ==1) %>% filter(ethnicity_cat == 2) %>% select(m_ics)  %>% nrow() 
ICS_Blk <- respi %>% filter(m_ics ==1) %>% filter(ethnicity_cat == 3) %>% select(m_ics) %>% nrow() 
ICS_NEA <- respi %>% filter(m_ics ==1) %>% filter(ethnicity_cat == 4) %>% select(m_ics)  %>% nrow()

c(ICS_W,ICS_SEA,ICS_Blk,ICS_NEA) 

round(ICS_W/tot_W,2)        
round(ICS_SEA/tot_SEA,2)      
round(ICS_Blk/tot_Blk,2)        
round(ICS_NEA/tot_NEA,2)        

#-------------------------------------------------------------------------------
# OCS
OCS_W <- respi %>% filter(m_ocs ==1) %>% filter(ethnicity_cat == 1) %>% select(m_ocs) %>% nrow() 
OCS_SEA <- respi %>% filter(m_ocs ==1) %>% filter(ethnicity_cat == 2) %>% select(m_ocs)  %>% nrow() 
OCS_Blk <- respi %>% filter(m_ocs ==1) %>% filter(ethnicity_cat == 3) %>% select(m_ocs) %>% nrow() 
OCS_NEA <- respi %>% filter(m_ocs ==1) %>% filter(ethnicity_cat == 4) %>% select(m_ocs)  %>% nrow()

c(OCS_W,OCS_SEA,OCS_Blk,OCS_NEA) 

round(OCS_W/tot_W,2)        
round(OCS_SEA/tot_SEA,2)      
round(OCS_Blk/tot_Blk,2)        
round(OCS_NEA/tot_NEA,2)        

#-------------------------------------------------------------------------------
# FEV1/FVC ratio
FFratio_W <- respi %>% filter(ethnicity_cat == 1) %>% select(fev1_fvc_ratio) %>% sum(na.rm = T) %>% round(2)
FFratio_SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(fev1_fvc_ratio)  %>% sum(na.rm = T) %>% round(2)
FFratio_Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(fev1_fvc_ratio) %>% sum(na.rm = T) %>% round(2)
FFratio_NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(fev1_fvc_ratio)  %>% sum(na.rm = T) %>% round(2)

c(FFratio_W,FFratio_SEA,FFratio_Blk,FFratio_NEA) 

round(FFratio_W/tot_W,2)        
round(FFratio_SEA/tot_SEA,2)      
round(FFratio_Blk/tot_Blk,2)        
round(FFratio_NEA/tot_NEA,2)        

#-------------------------------------------------------------------------------
# Asthma hospitalization
Asthma_W <- respi %>% filter(asthma_admission ==1) %>% filter(ethnicity_cat == 1) %>% select(asthma_admission) %>% nrow() 
Asthma_SEA <- respi %>% filter(asthma_admission ==1) %>% filter(ethnicity_cat == 2) %>% select(asthma_admission)  %>% nrow() 
Asthma_Blk <- respi %>% filter(asthma_admission ==1) %>% filter(ethnicity_cat == 3) %>% select(asthma_admission) %>% nrow() 
Asthma_NEA <- respi %>% filter(asthma_admission ==1) %>% filter(ethnicity_cat == 4) %>% select(asthma_admission)  %>% nrow()

c(Asthma_W,Asthma_SEA,Asthma_Blk,Asthma_NEA) 

round(Asthma_W/tot_W,2)        
round(Asthma_SEA/tot_SEA,2)      
round(Asthma_Blk/tot_Blk,2)        
round(Asthma_NEA/tot_NEA,2)        


#-------------------------------------------------------------------------------

# Non Asthma Asthma_W <- respi %>% filter(asthma_admission ==1) %>% filter(ethnicity_cat == 1) %>% select(asthma_admission) %>% nrow() 
NonAst_W <- respi %>% filter(ethnicity_cat == 1) %>% select(Nonasthma) %>% sum()
NonAst_SEA <- respi %>% filter(ethnicity_cat == 2) %>% select(Nonasthma)  %>% sum() 
NonAst_Blk <- respi %>% filter(ethnicity_cat == 3) %>% select(Nonasthma) %>% sum() 
NonAst_NEA <- respi %>% filter(ethnicity_cat == 4) %>% select(Nonasthma)  %>% sum()

c(NonAst_W,NonAst_SEA,NonAst_Blk,NonAst_NEA) 

round(Asthma_W/tot_W,2)        
round(Asthma_SEA/tot_SEA,2)      
round(Asthma_Blk/tot_Blk,2)        
round(Asthma_NEA/tot_NEA,2)        


#-------------------------------------------------------------------------------


df <- data.frame(Variable = c('female','Never_smoke','IMD','IMD2','IMD3',
                              'IMD4', 'IMD5','Income_31k','Uni_edu','EmReSt', 'BMI30',
                              'Hyp','Pain', 'Dys', 'Dep',
                              'Ecz', 'SABA', 'ICS', 'OCS', 'FFratio', 'Asthma','NonAst'),
                 White = c(fem_W, Smk_W,IMD_W, IMD2_W, IMD3_W, IMD4_W, IMD5_W, Inc_W,
                           Uni_W, Emp_W, BMI30_W, Hyp_W, Pain_W, Dys_W, Dep_W, 
                           Ecz_W, SABA_W, ICS_W, OCS_W, FFratio_W, Asthma_W, NonAst_W), 
                 SE_Asia = c(fem_SEA, Smk_SEA,
                             IMD_SEA, IMD2_SEA, IMD3_SEA, IMD4_SEA, IMD5_SEA, Inc_SEA, 
                             Uni_SEA, Emp_SEA, BMI30_SEA, Hyp_SEA, Pain_SEA, 
                             Dys_SEA, Dep_SEA, Ecz_SEA, SABA_SEA, ICS_SEA, 
                             OCS_SEA, FFratio_SEA, Asthma_SEA, NonAst_SEA),
                 Black = c(fem_Blk, Smk_Blk,IMD_Blk, IMD2_Blk, IMD3_Blk, 
                           IMD4_Blk, IMD5_Blk, Inc_Blk, Uni_Blk, Emp_Blk, BMI30_Blk, 
                           Hyp_Blk, Pain_Blk, Dys_Blk, Dep_Blk, Ecz_Blk, 
                           SABA_Blk, ICS_Blk, OCS_Blk, FFratio_Blk, Asthma_Blk, NonAst_Blk),
                 NE_Asia = c(fem_NEA, Smk_NEA, IMD_NEA, IMD2_NEA, IMD3_NEA, 
                             IMD4_NEA, IMD5_NEA, Inc_NEA, Uni_NEA, Emp_NEA, BMI30_NEA, 
                             Hyp_NEA, Pain_NEA, Dys_NEA, Dep_NEA, Ecz_NEA, 
                             SABA_NEA, ICS_NEA, OCS_NEA, FFratio_NEA, Asthma_NEA, NonAst_NEA)
                 
) %>%  mutate_at(c('White','SE_Asia','Black', "NE_Asia"),as.numeric)
str(df)

df1 <- df %>% mutate(White = round(White/tot_W,2), SE_Asia = round(SE_Asia/tot_SEA,2), 
                     Black = round(Black/tot_Blk,2), 
                     NE_Asia = round(NE_Asia/tot_NEA,2)) %>% 
  select_if(is.numeric) %>% round(2) %>% 
  mutate_at(vars(White:NE_Asia), .funs = funs(.*100))

df_perc <- df %>% select(Variable) %>% cbind(df1)

df2 <-rbind(      IMD = c(IMD_W,IMD_SEA,IMD_Blk, IMD_NEA),
                  IMD2 = c(IMD2_W,IMD2_SEA,IMD2_Blk, IMD2_NEA),
                  IMD3 = c(IMD3_W,IMD3_SEA,IMD3_Blk, IMD3_NEA),
                  IMD4 = c(IMD4_W,IMD4_SEA,IMD4_Blk, IMD4_NEA),
                  IMD5 = c(IMD5_W,IMD5_SEA,IMD5_Blk, IMD5_NEA),
                  Uni_edu = c(Uni_W,Uni_SEA,Uni_Blk,Uni_NEA),
                  EmpRtSt = c(Emp_W, Emp_SEA, Emp_Blk, Emp_NEA),
                  BMI30 = c(BMI30_W, BMI30_SEA, BMI30_Blk, BMI30_NEA),
                  Bmisd = bmi_sd$bmi,
                  Ex_METmed = Ex_MET$met_median,
                  Ex_METiqr = Ex_MET$met_IQR,
                  llnfev1_sd = llnfev_sd$bmi,
                  age_asth = asth_age$avg,
                  age_asth_sd = asth_age$stdv
)
colnames(df2) <- c("White","SE Asian", "Black", "NE Asian")
df2 <- round(df2,1)

file.edit('Statistical tests.R')
