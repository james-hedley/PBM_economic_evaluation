# Project: Statistical analyses of ANZDATA data for use as inputs into the economic evaluation
#          of increased use of primary brain malignancy donors for kidney transplantation
# Created by: James Hedley
# Date created: 29th July 2021
# Last updated: 22nd June 2022


# Load libraries
library('tidyverse')
library('stringr')
library('haven')
library('readxl')
library('labelled')
library('janitor')
library('lubridate')
library('flexsurv')
library('popEpi')
library('nnet')
library('survminer')
library('readxl')


# Set directories
usyd <- '//shared.sydney.edu.au/research-data/'
anzdataloc <- paste0(usyd,'PRJ-MODUS/3 Data Management/ANZDATA useable data')

paper3loc <- 'C:/Users/james.hedley/My Drive/Education/USYD - PhD/Paper 3 PBT kidney donors economic evaluation/'
inputsloc <- paste0(paper3loc,'Model inputs/')
modelsloc <- paste0(paper3loc,'Model inputs/R models/')
dashboardloc <- paste0(paper3loc,'Dashboard/')


# Load data ----
## ANZDATA datasets
CancerInDonor <- read_dta(file.path(anzdataloc,'42962_AnzdataCancerInDonor.dta'))
CancerNonSkinTumours <- read_dta(file.path(anzdataloc,'42962_AnzdataCancerNonSkinTumours.dta'))
CancerSkinTumours <- read_dta(file.path(anzdataloc,'42962_AnzdataCancerSkinTumours.dta'))
#CentreTransfers <- read_dta(file.path(anzdataloc,'42962_AnzdataCentreTransfers.dta'))
Comorbidities <- read_dta(file.path(anzdataloc,'42962_AnzdataComorbidities.dta'))
#CourseOfTreatments <- read_dta(file.path(anzdataloc,'42962_AnzdataCourseOfTreatments.dta'))
#Haemodialysis <- read_dta(file.path(anzdataloc,'42962_AnzdataHaemodialysis.dta'))
#OtherComorbidities <- bind_cols(read_dta(file.path(anzdataloc,'42962_AnzdataOtherComorbidities.dta'),col_select=c(1:3)),
#                                read.csv(file.path(anzdataloc,'42962_AnzdataOtherComorbidities.csv')) %>% dplyr::select(4:5)) # String variables too large and cause errors when trying to open using read_dta()
Patients <-  bind_cols(read_dta(file.path(anzdataloc,'42962_AnzdataPatients.dta'),col_select=c(1:19)),
                       read.csv(file.path(anzdataloc,'42962_AnzdataPatients.csv')) %>% dplyr::select(20), # String variables too large and cause errors when trying to open using read_dta()
                       read_dta(file.path(anzdataloc,'42962_AnzdataPatients.dta'),col_select=c(21:24)))
Transplants <- read_dta(file.path(anzdataloc,'42962_AnzdataTransplants.dta'))
#TransplantSerumCreatinine <- read_dta(file.path(anzdataloc,'42962_AnzdataTransplantSerumCreatinine.dta'))
BloodGroup <- read_dta(file.path(anzdataloc,'42962_BloodGroup.dta'))
DonorDetails <- read_dta(file.path(anzdataloc,'42962_DonorDetails.dta'))
OMStatusHistory <- read_dta(file.path(anzdataloc,'42962_OMStatusHistory.dta'))


## Australian Life tables
periods <- c('2012_2014', '2013_2015', '2014_2016', '2015_2017', '2016_2018', '2017_2019')

for (period in periods) {
  ## Open lifetables data
  extension <- ifelse(period == '2017_2019', '.xlsx', '.xls')
  
  assign(paste0('lifetables_', period),
         read_excel(path=file.path(inputsloc, paste0('ABS Life tables ', period, extension)),
                    sheet='Table_1.9',
                    range='A8:I108',
                    col_names=c('age',
                                'males_lx','males_qx','males_Lx','males_ex',
                                'females_lx','females_qx','females_Lx','females_ex')))
  
  ## Reshape to long format (one row per age per sex)
  assign(paste0('lifetables_long_', period),
         eval(parse(text = paste0('lifetables_', period)), envir = .GlobalEnv) %>%
           uncount(2) %>%
           group_by(age) %>%
           mutate(female = seq_len(n()) - 1,
                  lx = ifelse(female == 1, females_lx, males_lx),
                  Lx = ifelse(female == 1, females_Lx, males_Lx),
                  ex = ifelse(female == 1, females_ex, males_ex),
                  qx = ifelse(female == 1, females_qx, males_qx)) %>%
           ungroup() %>%
           distinct(age, female, lx, Lx, ex, qx))
}


## AIHW cancer incidence by age and sex
aihw_cancerincidence <- read_excel(path = file.path(inputsloc, 'AIHW Cancer incidence.xlsx'),
                   sheet = 'Table S1a.1',
                   skip = 6,
                   col_names = c('datatype', 'cancersite', 'year', 'sex', 
                                 'agegroup', 'count', 'agespecificrate', 
                                 'agestandardisedrate_aus', 
                                 'agestandardisedrate_who', 
                                 'agestandardisedrate_segi', '_', 'icd10codes'))


## AIHW cancer mortality by age and sex
aihw_cancermortality <- read_excel(path = file.path(inputsloc, 'AIHW Cancer mortality.xlsx'),
                                   sheet = 'Table S2a.1',
                                   skip = 6,
                                   col_names = c('datatype', 'cancersite', 'year', 'sex',
                                                 'agegroup', 'count', 'agespecificrate', 
                                                 'agestandardisedrate_aus',
                                                 'agestandardisedrate_who',
                                                 'agestandardisedrate_segi', 
                                                 '_', 'icd10codes'))


## AIHW cancer relative survival by age and sex
aihw_cancersurvival <- read_excel(path = file.path(inputsloc, 'AIHW Cancer relative survival.xlsx'),
                                  sheet = 'Table S3a.2',
                                  skip = 6,
                                  col_names = c('survivaltype', 'cancersite', 'period', 'sex', 
                                                'yearsafterdiagnosis', 'agegroup', 'survival', 
                                                'survival_lower95', 'survival_upper95', 
                                                '_', 'icd10codes'))



## ANZDATA Annual Report 2021 - chapter 3 Figure 3.2.1 and Figure 3.2.2 Data 
## Data extracted using Web Plot Digitizer
anzdata_mortality_dialysis <- read_excel(path = file.path(inputsloc, 'ANZDATA 2021 Mortality data.xlsx'),
                                         sheet = 'Dialysis',
                                         range = 'B7:H13',
                                         col_names = c('age', 'dialysis_female', 'genpop_female', 'irr_female',
                                                       'dialysis_male', 'genpop_male', 'irr_male'))

anzdata_mortality_transplant <- read_excel(path = file.path(inputsloc, 'ANZDATA 2021 Mortality data.xlsx'),
                                           sheet = 'Transplant',
                                           range = 'B7:H13',
                                           col_names = c('age', 'transplant_female', 'genpop_female', 'irr_female',
                                                         'transplant_male', 'genpop_male', 'irr_male'))




## Australian Estimated post-transplant survival (EPTS) calculator lookup table
epts_table <- read_excel(file.path(inputsloc, 'Australian-EPTS-Calculator.xlsx'), sheet = 'cutpoints', col_names = c('raw', 'score'))



# View datasets ----
## Data dictionaries
# generate_dictionary(CancerInDonor)
# generate_dictionary(CancerNonSkinTumours)
# generate_dictionary(CancerSkinTumours)
# generate_dictionary(CentreTransfers)
# generate_dictionary(Comorbidities)
# generate_dictionary(CourseOfTreatments)
# generate_dictionary(Haemodialysis)
# generate_dictionary(OtherComorbidities)
# generate_dictionary(Patients)
# generate_dictionary(Transplants)
# generate_dictionary(TransplantSerumCreatinine)
# generate_dictionary(BloodGroup)
# generate_dictionary(DonorDetails)
# generate_dictionary(OMStatusHistory)


## Lists of variable names
# colnames(CancerInDonor)
# colnames(CancerNonSkinTumours)
# colnames(CancerSkinTumours)
# colnames(CentreTransfers)
# colnames(Comorbidities)
# colnames(CourseOfTreatments)
# colnames(Haemodialysis)
# colnames(OtherComorbidities)
# colnames(Patients)
# colnames(Transplants)
# colnames(TransplantSerumCreatinine)
# colnames(BloodGroup)
# colnames(DonorDetails)
# colnames(OMStatusHistory)


# Create cohort 1: Patients activated on the waiting list for a first or subsequent transplant ----
## Date of transplant (wide format, one row per patient, columns for each transplant)
tx <- Transplants %>%
  mutate(txtype=ifelse(donorsourcecode==100,'Deceased','Living')) %>%
  dplyr::select(id, graftno, transplantdate, txtype) 

tx_wide <- tx %>%
  pivot_wider(names_from = graftno,
              names_prefix = 'tx',
              values_from = c(transplantdate, txtype))


## Date first activated on waitlist for each transplant
waitlistdate <- OMStatusHistory %>%
  filter(organprogramname=='Kidney',
         waitstatus==2) %>%
  arrange(id, waitdate) %>% 
  group_by(id) %>%
  mutate(first_waitdate = min(waitdate)) %>%
  ungroup() %>%
  left_join(tx_wide) %>%
  mutate(interval5 = lubridate::interval(transplantdate_tx4, transplantdate_tx5),
         interval4 = lubridate::interval(transplantdate_tx3, transplantdate_tx4),
         interval3 = lubridate::interval(transplantdate_tx2, transplantdate_tx3),
         interval2 = lubridate::interval(transplantdate_tx1, transplantdate_tx2)) %>%
  mutate(graftno = case_when(
    waitdate %within% interval5 ~ 5,
    waitdate %within% interval4 ~ 4,
    waitdate %within% interval3 ~ 3,
    waitdate %within% interval2 ~ 2,
    TRUE ~ 1)) %>% 
  distinct(id, graftno, .keep_all=TRUE) %>%
  dplyr::select(id, graftno, waitdate)


## Date of death
death <- Patients %>%
  dplyr::select(id,dateofdeath)

## Sex and birthdate (based on age and date at first RRT)
age_sex <- Patients %>%
  distinct(id,.keep_all=TRUE) %>%
  mutate(female=(gendercode=='F')*1) %>%
  mutate(birthdate=rrtstartdate-((age+0.5)*365.25)) %>% # Add 0.5 to age since on average people will be halfway through their age (e.g. 44 could mean 44.01 or 44.99)
  rename(rrtstart_age=age) %>%
  dplyr::select(id,rrtstartdate,rrtstart_age,birthdate,female)

## Height and weight (for determining drug doses)
height_weight <- Patients %>%
  dplyr::select(id, height, weight)

## Bloodgroup
bloodgroup <- BloodGroup %>% 
  mutate(bloodgroup=case_when(
    bloodgroupcode %in% c('O') ~ 'O',
    bloodgroupcode %in% c('A','A1','A2') ~ 'A',
    bloodgroupcode %in% c('B') ~ 'B',
    bloodgroupcode %in% c('AB','A1B','A2B') ~ 'AB'),
    bloodgroup=factor(bloodgroup,levels=c('O','A','B','AB'))) %>%
  mutate(bloodgroup_o=(bloodgroup=='O')*1, # Create dummy variables for each bloodgroup
         bloodgroup_a=(bloodgroup=='A')*1,
         bloodgroup_b=(bloodgroup=='B')*1,
         bloodgroup_ab=(bloodgroup=='AB')*1) %>%
  dplyr::select(id,bloodgroup,bloodgroup_o,bloodgroup_a,bloodgroup_b,bloodgroup_ab)


## Cancer diagnosis date
cancer <- bind_rows(CancerNonSkinTumours, CancerSkinTumours) %>%
  mutate(diagnosisdate = as_date(ifelse(is.na(diagnosisdate), 
                                        firstdiagnosisposttransplantdate, 
                                        diagnosisdate))) %>%
  left_join(waitlistdate %>% pivot_wider(
    id_cols = 'id',
    names_from = 'graftno',
    names_prefix = 'waitdate',
    values_from = 'waitdate')) %>%
  left_join(tx %>% pivot_wider(
    id_cols = 'id',
    names_from = 'graftno',
    values_from = c('transplantdate', 'txtype'))) %>%
  pivot_longer(
    cols = starts_with('waitdate'),
    names_to = 'waitdate_graftno',
    values_to = 'waitdate') %>%
  pivot_longer(
    cols = starts_with('transplantdate_'),
             names_to = 'transplantdate_graftno',
             values_to = 'transplantdate') %>%
  pivot_longer(
    cols = starts_with('txtype_'),
             names_to = 'txtype_graftno',
             values_to = 'txtype') %>% 
  mutate(waitdate_graftno = str_extract(waitdate_graftno, '\\d'),
         transplantdate_graftno = str_extract(transplantdate_graftno, '\\d'),
         txtype_graftno = str_extract(txtype_graftno, '\\d'),
         graftno = as.numeric(waitdate_graftno)) %>% 
  filter((waitdate_graftno == transplantdate_graftno) & (transplantdate_graftno == txtype_graftno)) %>%
  filter((diagnosisdate >= waitdate) & (diagnosisdate <= transplantdate)) %>%
  group_by(id, graftno) %>%
  mutate(firstdiagnosisdate = min(diagnosisdate)) %>%
  ungroup() %>%
  distinct(id, graftno, firstdiagnosisdate) 


## Comorbidities (any history up until each date reported, assume suspected (S) = yes)
comorbs <- Comorbidities %>%
  arrange(id,codate) %>%
  group_by(id) %>%
  mutate(everchroniclung=cummax((chroniclungcode %in% c('S','Y'))*1),
         evercoronaryartery=cummax((coronaryarterycode %in% c('S','Y'))*1),
         everperipheralvascular=cummax((peripheralvascularcode %in% c('S','Y'))*1),
         evercerebrovascular=cummax((cerebrovascularcode %in% c('S','Y'))*1),
         everdiabetes=cummax((diabetescode %in% c('O','P','Q'))*1),
         evercancer=cummax((cancerevercode %in% c('S','Y'))*1),
         everhepatitisc=cummax((hepatitisccode %in% c(1))*1)) %>%
  ungroup() %>%
  mutate(comorbs=
           everchroniclung + 
           evercoronaryartery + 
           everperipheralvascular + 
           evercerebrovascular + 
           everdiabetes + 
           evercancer + 
           everhepatitisc)

## Only keep comorbidities from the date closest to but before each waitlisting, 
## or otherwise the earliest date after waitlisting, for each transplant
comorbs_waitlist1 <- comorbs %>%
  right_join(waitlistdate %>% filter(graftno == 1)) %>%
  mutate(datediff_before=ifelse(codate<=waitdate,waitdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>waitdate,codate-waitdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=waitdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
         (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
         everperipheralvascular,evercerebrovascular,everdiabetes,
         evercancer,everhepatitisc) %>%
  mutate(graftno = 1)
  

comorbs_waitlist2 <- comorbs %>%
  right_join(waitlistdate %>% filter(graftno == 2)) %>%
  mutate(datediff_before=ifelse(codate<=waitdate,waitdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>waitdate,codate-waitdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=waitdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 2)


comorbs_waitlist3 <- comorbs %>%
  right_join(waitlistdate %>% filter(graftno == 3)) %>%
  mutate(datediff_before=ifelse(codate<=waitdate,waitdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>waitdate,codate-waitdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=waitdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 3)

comorbs_waitlist4 <- comorbs %>%
  right_join(waitlistdate %>% filter(graftno == 4)) %>%
  mutate(datediff_before=ifelse(codate<=waitdate,waitdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>waitdate,codate-waitdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=waitdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 4)


comorbs_waitlist5 <- comorbs %>%
  right_join(waitlistdate %>% filter(graftno == 5)) %>%
  mutate(datediff_before=ifelse(codate<=waitdate,waitdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>waitdate,codate-waitdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=waitdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 5)


comorbs_waitlist <- comorbs_waitlist1 %>%
  rbind(comorbs_waitlist2) %>%
  rbind(comorbs_waitlist3) %>%
  rbind(comorbs_waitlist4) %>%
  rbind(comorbs_waitlist5)


## Combine datasets together
cohort1 <- waitlistdate %>%
  left_join(tx) %>%
  left_join(death) %>%
  left_join(age_sex) %>%
  left_join(height_weight) %>%
  left_join(bloodgroup) %>%
  left_join(cancer) %>%
  left_join(comorbs_waitlist) %>%
  arrange(waitdate,id) # sort by first waitlisting date
  
  
## Remove patients who received a transplant before their first waitlisting 
cohort1 <- cohort1 %>%
  filter(transplantdate>=waitdate | is.na(transplantdate)) 

## Remove patients who started RRT (renal replacement therapy) before the 
## earliest available waitlisting data (27th June 2006)
cohort1 <- cohort1 %>%
  filter(rrtstartdate>=min(waitdate))


## Create a variable for age at first waitlisting 
cohort1 <- cohort1 %>%
  mutate(waitlist_age=as.integer((waitdate-birthdate)/365.25))


## Create a variable for number of previous transplants
cohort1 <- cohort1 %>%
  mutate(prevtxcount = graftno - 1,
         prevtx = factor(prevtxcount != 0, 
                         levels = c(FALSE, TRUE), 
                         labels = c("No", "Yes")))

## Create variables for survival analyses (time to first transplant, time to cancer, time to death)
cohort1 <- cohort1 %>%
  mutate(startdate=waitdate,
         starttime=0,
         #
         enddate_tx=pmin(transplantdate, dateofdeath, firstdiagnosisdate, 
                         as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_tx=as.numeric(pmax(enddate_tx-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_tx=time_tx,
         tx=ifelse(!is.na(transplantdate) & transplantdate<=enddate_tx,1,0),
         #
         enddate_dtx=pmin(transplantdate, dateofdeath, firstdiagnosisdate, 
                         as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_dtx=as.numeric(pmax(enddate_dtx-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_dtx=time_dtx,
         dtx=ifelse(!is.na(transplantdate) & transplantdate<=enddate_dtx & txtype=='Deceased',1,0),       
         #
         enddate_ltx=pmin(transplantdate, dateofdeath, firstdiagnosisdate, 
                         as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_ltx=as.numeric(pmax(enddate_ltx-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_ltx=time_ltx,
         ltx=ifelse(!is.na(transplantdate) & transplantdate<=enddate_ltx & txtype=='Living',1,0),
         #
         enddate_cancer=pmin(transplantdate, dateofdeath, firstdiagnosisdate, 
                         as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_cancer=as.numeric(pmax(enddate_cancer-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_cancer=time_cancer,
         cancer=ifelse(!is.na(firstdiagnosisdate) & firstdiagnosisdate<=enddate_cancer,1,0),
         #
         enddate_wtlstdth=pmin(transplantdate, dateofdeath, firstdiagnosisdate, 
                         as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_wtlstdth=as.numeric(pmax(enddate_wtlstdth-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_wtlstdth=time_wtlstdth,
         wtlstdth=ifelse(!is.na(dateofdeath) & dateofdeath<=enddate_wtlstdth,1,0),
         #
         enddate_wtlstcdth=pmin(transplantdate, dateofdeath, 
                               as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_wtlstcdth=as.numeric(pmax(enddate_wtlstcdth-firstdiagnosisdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_wtlstcdth=time_wtlstcdth,
         wtlstcdth=ifelse(!is.na(dateofdeath) & dateofdeath<=enddate_wtlstcdth,1,0))




# Create cohort 2: Patients who receive a deceased donor transplant ----
## All transplants, kidneys only
tx <- Transplants %>%
  filter(multiorgancode=='N') %>%
  dplyr::select(id, graftno, transplantdate, ageattransplant, transplantstatus,
                alivestatus, graftfailuredate, graftfailurecausecode, lastfollowupdate, 
                endtransplantdate)

## Date of death
deathdate <- Patients %>%
  dplyr::select(id, dateofdeath)

## Cause of death  
deathcause <- Patients %>%
  dplyr::select(id, deathcausecode)

## Donor age
donor_age <- DonorDetails %>%
  dplyr::select(id, graftno, donorage)

## Donor type
donor_type <- DonorDetails %>%
  mutate(donortype=case_when(
    ddonid=='' & donor_heartbeatingcode!='N' ~ 'Living',
    ddonid!='' & donor_heartbeatingcode=='' ~ 'Deceased',
    ddonid!='' & donor_heartbeatingcode=='Y' & 
      (donorage>=60 | 
         (donorage>=50 & 
            ((donor_hypertensioncode=='Yes') + 
               (donor_deathcategorycode==3) + 
               (donor_creatinineterminal>=(1.5*88.42)))>=2))  ~ 'DBDECD',
    ddonid!='' & donor_heartbeatingcode=='Y' ~ 'DBDSCD',
    ddonid!='' & donor_heartbeatingcode=='N' ~ 'DCD',
    TRUE ~ NA_character_),
    donortype=factor(donortype,levels=c('Living','DCD','DBDSCD','DBDECD'))) %>%
  dplyr::select(id, graftno, donortype)


## Transplant type
donor_deceased <- donor_type %>%
  mutate(donordeceased = case_when(
    donortype == 'Living' ~ 0,
    donortype %in% c('DCD','DBDSCD','DBDECD') ~ 1,
    TRUE ~ NA_real_)) %>%
  dplyr::select(-donortype)


## Donor sex
donor_sex <- DonorDetails %>%
  mutate(donorfemale = (donorgendercode=='F')*1) %>%
  dplyr::select(id, graftno, donorfemale)


## Donor KDPI
donor_kdpi <- DonorDetails %>%
  mutate(donorkdpi = kdpi) %>%
  dplyr::select(id, graftno, donorkdpi)


## Donor PBM (primary brain malignancy)
donor_pbm <- DonorDetails %>%
  left_join(CancerInDonor) %>%
  mutate(donorpbm = ifelse(siteofcancercategorycode %in% c(191,192), # 191=Brain, 192=Other CNS
                           1,
                           0)) %>% 
  dplyr::select(id, graftno, donorpbm)


## Post-transplant cancer
posttx_cancer <- bind_rows(CancerNonSkinTumours, CancerSkinTumours) %>%
  mutate(diagnosisdate = as_date(ifelse(is.na(diagnosisdate), 
                                        firstdiagnosisposttransplantdate, 
                                        diagnosisdate))) %>%
  left_join(tx %>% pivot_wider(
    id_cols = 'id',
    names_from = 'graftno',
    values_from = c('transplantdate', 'graftfailuredate'))) %>%
  pivot_longer(
    cols = starts_with('transplantdate_'),
    names_to = 'transplantdate_graftno',
    values_to = 'transplantdate') %>%
  pivot_longer(
    cols = starts_with('graftfailuredate_'),
    names_to = 'graftfailuredate_graftno',
    values_to = 'graftfailuredate') %>% 
  mutate(transplantdate_graftno = str_extract(transplantdate_graftno, '\\d'),
         graftfailuredate_graftno = str_extract(graftfailuredate_graftno, '\\d'),
         graftno = as.numeric(transplantdate_graftno)) %>% 
  filter(transplantdate_graftno == graftfailuredate_graftno) %>%
  filter((diagnosisdate >= transplantdate) & (diagnosisdate <= graftfailuredate)) %>%
  group_by(id, graftno) %>%
  mutate(firstdiagnosisdate = min(diagnosisdate)) %>%
  ungroup() %>%
  left_join(age_sex) %>%
  mutate(ageatcancer = as.numeric(firstdiagnosisdate - birthdate) / 365.25) %>%
  distinct(id, graftno, firstdiagnosisdate, ageatcancer)


## Only keep comorbidities from the date closest to but before each transplant, 
## or otherwise the earliest date after transplant 
comorbs_tx <- comorbs %>%
  right_join(tx) %>%
  mutate(datediff_before=ifelse(codate<=transplantdate,transplantdate-codate,NA), # Calculate difference between comorbidity date and transplant date (only if comorbidity date is before transplant date)
         datediff_after=ifelse(codate>transplantdate,codate-transplantdate,NA)) %>% # Calculate difference between comorbidity date and transplant date (only if comorbidity date is after transplant date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=transplantdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc)




comorbs_tx1 <- comorbs %>%
  right_join(tx %>% filter(graftno == 1)) %>%
  mutate(datediff_before=ifelse(codate<=transplantdate,transplantdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>transplantdate,codate-transplantdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=transplantdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 1)

comorbs_tx2 <- comorbs %>%
  right_join(tx %>% filter(graftno == 2)) %>%
  mutate(datediff_before=ifelse(codate<=transplantdate,transplantdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>transplantdate,codate-transplantdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=transplantdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 2)

comorbs_tx3 <- comorbs %>%
  right_join(tx %>% filter(graftno == 3)) %>%
  mutate(datediff_before=ifelse(codate<=transplantdate,transplantdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>transplantdate,codate-transplantdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=transplantdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 3)

comorbs_tx4 <- comorbs %>%
  right_join(tx %>% filter(graftno == 4)) %>%
  mutate(datediff_before=ifelse(codate<=transplantdate,transplantdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>transplantdate,codate-transplantdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=transplantdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 4)

comorbs_tx5 <- comorbs %>%
  right_join(tx %>% filter(graftno == 5)) %>%
  mutate(datediff_before=ifelse(codate<=transplantdate,transplantdate-codate,NA), # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is before waitlist date)
         datediff_after=ifelse(codate>transplantdate,codate-transplantdate,NA)) %>% # Calculate difference between comorbidity date and waitlist date (only if comorbidity date is after waitlist date)
  group_by(id) %>%
  mutate(codate_ever_before=max((codate<=transplantdate)*1),
         mindatediff_before=min(datediff_before,na.rm=TRUE),
         mindatediff_after=min(datediff_after,na.rm=TRUE)) %>%
  ungroup() %>%
  filter((codate_ever_before==1 & datediff_before==mindatediff_before) |
           (codate_ever_before==0 & datediff_after==mindatediff_after)) %>%
  dplyr::select(id,codate,comorbs,everchroniclung,evercoronaryartery,
                everperipheralvascular,evercerebrovascular,everdiabetes,
                evercancer,everhepatitisc) %>%
  mutate(graftno = 5)


comorbs_tx <- comorbs_tx1 %>%
  rbind(comorbs_tx2) %>%
  rbind(comorbs_tx3) %>%
  rbind(comorbs_tx4) %>%
  rbind(comorbs_tx5)



## Combine transplants with donor characteristics, post-transplant cancer, and patient characteristics
cohort2 <- tx %>%
  left_join(deathdate) %>%
  left_join(deathcause) %>%
  left_join(donor_age) %>%
  left_join(donor_deceased) %>%
  left_join(donor_type) %>%
  left_join(donor_pbm) %>%
  left_join(donor_sex) %>%
  left_join(donor_kdpi) %>%
  left_join(posttx_cancer) %>%
  left_join(age_sex) %>%
  left_join(bloodgroup) %>%
  left_join(comorbs_tx) %>%
  arrange(transplantdate,id)


## Create a variable for number of previous transplants
cohort2 <- cohort2 %>%
  mutate(prevtxcount = graftno - 1,
         prevtx = factor(prevtxcount != 0, 
                         levels = c(FALSE, TRUE), 
                         labels = c("No", "Yes")))

## Create variables for survival analyses (time to post-tx cancer, time to transplant failure, time to death censored at transplant failure)
cohort2 <- cohort2 %>%
  mutate(startdate=transplantdate,
         starttime=0,
         #
         enddate_ltxfail=pmin(graftfailuredate,lastfollowupdate,endtransplantdate,dateofdeath,
                             as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_ltxfail=as.numeric(pmax(enddate_ltxfail-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_ltxfail=time_ltxfail,
         ltxfail=ifelse(!is.na(graftfailuredate) & graftfailuredate<=enddate_ltxfail,1,0),
         #
         enddate_txfail=pmin(graftfailuredate,lastfollowupdate,endtransplantdate,dateofdeath,firstdiagnosisdate,
                             as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_txfail=as.numeric(pmax(enddate_txfail-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_txfail=time_txfail,
         txfail=ifelse(!is.na(graftfailuredate) & graftfailuredate<=enddate_txfail,1,0),
         #
         enddate_txcfail=pmin(graftfailuredate,lastfollowupdate,endtransplantdate,dateofdeath,
                             as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_txcfail=as.numeric(pmax(enddate_txcfail-firstdiagnosisdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_txcfail=time_txcfail,
         txcfail=ifelse(!is.na(graftfailuredate) & graftfailuredate<=enddate_txcfail,1,0),
         #
         enddate_cancer=pmin(firstdiagnosisdate,graftfailuredate,lastfollowupdate,endtransplantdate,dateofdeath,
                             as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_cancer=as.numeric(pmax(enddate_cancer-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_cancer=time_cancer,
         cancer=ifelse(!is.na(firstdiagnosisdate) & firstdiagnosisdate<=enddate_cancer,1,0),
         #
         enddate_txdeath=pmin(graftfailuredate,lastfollowupdate,endtransplantdate,dateofdeath,firstdiagnosisdate,
                             as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_txdeath=as.numeric(pmax(enddate_txdeath-startdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_txdeath=time_txdeath,
         txdeath=ifelse(!is.na(dateofdeath) & dateofdeath<=enddate_txdeath,1,0),
         #
         enddate_txcdeath=pmin(graftfailuredate,lastfollowupdate,endtransplantdate,dateofdeath,
                              as.Date('31dec2019',format='%d%b%Y'),na.rm=TRUE),
         time_txcdeath=as.numeric(pmax(enddate_txcdeath-firstdiagnosisdate,1)), # Minimum time is 1 day (if end date is same as start date)
         endtime_txcdeath=time_txcdeath,
         txcdeath=ifelse(!is.na(dateofdeath) & dateofdeath<=enddate_txcdeath,1,0))



# Patient characteristics ----
## Number of previous transplants
cohort1 %>% tabyl(prevtxcount)

model_prevtxcount <- glm(data = cohort1,
                         family = poisson,
                         formula = prevtxcount ~ 1)

coef_prevtxcount_int_mean <- model_prevtxcount$coefficients["(Intercept)"] %>% as.numeric()
coef_prevtxcount_int_se <- summary(model_prevtxcount)$coefficients["(Intercept)", "Std. Error"] %>% as.numeric()


## Bloodgroup
model_bloodgroup <- multinom(data = cohort1,
                             family = binomial,
                             formula = bloodgroup ~ prevtxcount)

coef_bldgrp_bldgrpa_int_mean <- summary(model_bloodgroup)$coefficients['A','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpa_prevtxcount_mean <- summary(model_bloodgroup)$coefficients['A','prevtxcount'] %>% as.numeric()
coef_bldgrp_bldgrpa_int_se <- summary(model_bloodgroup)$standard.errors['A','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpa_prevtxcount_se <- summary(model_bloodgroup)$standard.errors['A','prevtxcount'] %>% as.numeric()

coef_bldgrp_bldgrpb_int_mean <- summary(model_bloodgroup)$coefficients['B','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpb_prevtxcount_mean <- summary(model_bloodgroup)$coefficients['B','prevtxcount'] %>% as.numeric()
coef_bldgrp_bldgrpb_int_se <- summary(model_bloodgroup)$standard.errors['B','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpb_prevtxcount_se <- summary(model_bloodgroup)$standard.errors['B','prevtxcount'] %>% as.numeric()

coef_bldgrp_bldgrpab_int_mean <- summary(model_bloodgroup)$coefficients['AB','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpab_prevtxcount_mean <- summary(model_bloodgroup)$coefficients['AB','prevtxcount'] %>% as.numeric()
coef_bldgrp_bldgrpab_int_se <- summary(model_bloodgroup)$standard.errors['AB','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpab_prevtxcount_se <- summary(model_bloodgroup)$standard.errors['AB','prevtxcount'] %>% as.numeric()


## Sex (female)
model_female <- glm(data=cohort1,
                    family=binomial,
                    formula= female ~ bloodgroup + prevtxcount)

coef_female_int_mean <- model_female$coefficients["(Intercept)"] %>% as.numeric()
coef_female_bldgrpa_mean <- model_female$coefficients['bloodgroupA'] %>% as.numeric()
coef_female_bldgrpb_mean <- model_female$coefficients['bloodgroupB'] %>% as.numeric()
coef_female_bldgrpab_mean <- model_female$coefficients['bloodgroupAB'] %>% as.numeric()
coef_female_prevtxcount_mean <- model_female$coefficients['prevtxcount'] %>% as.numeric()

coef_female_int_se <- summary(model_female)$coefficients["(Intercept)", "Std. Error"] %>% as.numeric()
coef_female_bldgrpa_se <- summary(model_female)$coefficients['bloodgroupA', "Std. Error"] %>% as.numeric()
coef_female_bldgrpb_se <- summary(model_female)$coefficients['bloodgroupB', "Std. Error"] %>% as.numeric()
coef_female_bldgrpab_se <- summary(model_female)$coefficients['bloodgroupAB', "Std. Error"] %>% as.numeric()
coef_female_prevtxcount_se <- summary(model_female)$coefficients['prevtxcount', "Std. Error"] %>% as.numeric()



## Age at first waitlisting
model_agewtlst <- glm(data=cohort1,
                    family=gaussian,
                    formula= waitlist_age ~ bloodgroup + female + prevtxcount)

coef_agewtlst_int_mean <- model_agewtlst$coefficients["(Intercept)"] %>% as.numeric()
coef_agewtlst_bldgrpa_mean <- model_agewtlst$coefficients['bloodgroupA'] %>% as.numeric()
coef_agewtlst_bldgrpb_mean <- model_agewtlst$coefficients['bloodgroupB'] %>% as.numeric()
coef_agewtlst_bldgrpab_mean <- model_agewtlst$coefficients['bloodgroupAB'] %>% as.numeric()
coef_agewtlst_female_mean <- model_agewtlst$coefficients['female'] %>% as.numeric()
coef_agewtlst_prevtxcount_mean <- model_agewtlst$coefficients['prevtxcount'] %>% as.numeric()
rmse_agewtlst <- sqrt(sum(model_agewtlst$residuals^2) / model_agewtlst$df.null)

coef_agewtlst_int_se <- summary(model_agewtlst)$coefficients["(Intercept)", "Std. Error"] %>% as.numeric()
coef_agewtlst_bldgrpa_se <- summary(model_agewtlst)$coefficients['bloodgroupA', "Std. Error"] %>% as.numeric()
coef_agewtlst_bldgrpb_se <- summary(model_agewtlst)$coefficients['bloodgroupB', "Std. Error"] %>% as.numeric()
coef_agewtlst_bldgrpab_se <- summary(model_agewtlst)$coefficients['bloodgroupAB', "Std. Error"] %>% as.numeric()
coef_agewtlst_female_se <- summary(model_agewtlst)$coefficients['female', "Std. Error"] %>% as.numeric()
coef_agewtlst_prevtxcount_se <- summary(model_agewtlst)$coefficients['prevtxcount', "Std. Error"] %>% as.numeric()



## Comorbidities
model_comorbs <- glm(data=cohort1,
                     family=poisson,
                     formula= comorbs ~ bloodgroup + female + waitlist_age + prevtxcount)

coef_comorbs_int_mean <- model_comorbs$coefficients["(Intercept)"] %>% as.numeric()
coef_comorbs_bldgrpa_mean <- model_comorbs$coefficients['bloodgroupA'] %>% as.numeric()
coef_comorbs_bldgrpb_mean <- model_comorbs$coefficients['bloodgroupB'] %>% as.numeric()
coef_comorbs_bldgrpab_mean <- model_comorbs$coefficients['bloodgroupAB'] %>% as.numeric()
coef_comorbs_female_mean <- model_comorbs$coefficients['female'] %>% as.numeric()
coef_comorbs_agewtlst_mean <- model_comorbs$coefficients['waitlist_age'] %>% as.numeric()
coef_comorbs_prevtxcount_mean <- model_comorbs$coefficients['prevtxcount'] %>% as.numeric()

coef_comorbs_int_se <- summary(model_comorbs)$coefficients["(Intercept)", "Std. Error"] %>% as.numeric()
coef_comorbs_bldgrpa_se <- summary(model_comorbs)$coefficients['bloodgroupA', "Std. Error"] %>% as.numeric()
coef_comorbs_bldgrpb_se <- summary(model_comorbs)$coefficients['bloodgroupB', "Std. Error"] %>% as.numeric()
coef_comorbs_bldgrpab_se <- summary(model_comorbs)$coefficients['bloodgroupAB', "Std. Error"] %>% as.numeric()
coef_comorbs_female_se <- summary(model_comorbs)$coefficients['female', "Std. Error"] %>% as.numeric()
coef_comorbs_agewtlst_se <- summary(model_comorbs)$coefficients['waitlist_age', "Std. Error"] %>% as.numeric()
coef_comorbs_prevtxcount_se <- summary(model_comorbs)$coefficients['prevtxcount', "Std. Error"] %>% as.numeric()




# Time to deceased donor transplant ----
## Run alternative parametric survival models to find the best model
## Explanation of how to get from survreg output to flexsurvreg output for use in rweibull():
## http://installers.treeagesoftware.com/treeagepro/PDF/R+Paremeterization+vs+TP.pdf

## Loop through all in-built distributions available for flexsurvreg()
# dists <- flexsurv.dists %>% as_tibble() %>% colnames()
# 
# for(dist in dists) {
#   tryCatch({
#   i <- grep(paste0('^',dist,'$'),dists)
# 
#   if (i==1) {
#     curr_t <- as.numeric(Sys.time())
#     prev_t <- curr_t
#     t <- 0
#   }
#   if (i>1) {
#     prev_t <- curr_t
#     curr_t <- as.numeric(Sys.time())
#     t <- round(curr_t - prev_t,0)
#   }
# 
#   progress <- paste0('Model ',
#                      i,
#                      ' of ',
#                      length(dists),
#                      ' (',
#                      dist,
#                      '): ',
#                      round(t,0),
#                      's')
#   print(progress)
# 
#   model <- flexsurvreg(data=cohort1,
#                        dist=dist,
#                        formula= Surv(starttime,endtime_dtx,dtx) ~
#                          bloodgroup + female + waitlist_age + comorbs + prevtxcount)
# 
#   distname <- sub('\\.','',dist)
# 
#   saveRDS(model,file.path(modelsloc,paste0('timetodtx_',distname,'.RDS')))
# 
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }
# 
# 
# ## Spline model
# model <- flexsurvspline(data=cohort1,
#                         knots=c(log(365.25*8)),
#                         formula= Surv(starttime,endtime_dtx,dtx) ~
#                           bloodgroup + female + waitlist_age + comorbs + prevtxcount)
# saveRDS(model,file.path(modelsloc,'timetodtx_spline.RDS'))
# 
# 
# ## Read all models into R, produce plots, and print AIC values
# for (dist in c(dists,'spline')) {
#   tryCatch({
#   assign(paste0('timetodtx_',dist),
#          readRDS(paste0(modelsloc,'timetodtx_',dist,'.RDS')))
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }


## Visually assess goodness-of-fit
# plot(timetodtx_genf, xlim=c(0,365.25*5), main='dist = genf')
# plot(timetodtx_genf.orig, xlim=c(0,365.25*5), main='dist = genf.orig')
# plot(timetodtx_gengamma, xlim=c(0,365.25*5), main='dist = gengamma')
# plot(timetodtx_gengamma.orig, xlim=c(0,365.25*5), main='dist = gengamma.orig')
# plot(timetodtx_exp, xlim=c(0,365.25*5), main='dist = exp')
# plot(timetodtx_weibull, xlim=c(0,365.25*5), main='dist = weibull')
# plot(timetodtx_weibullPH, xlim=c(0,365.25*5), main='dist = weibullPH')
# plot(timetodtx_lnorm, xlim=c(0,365.25*5), main='dist = lnorm')
# plot(timetodtx_gamma, xlim=c(0,365.25*5), main='dist = gamma')
# plot(timetodtx_gompertz, xlim=c(0,365.25*5), main='dist = gompertz')
# plot(timetodtx_llogis, xlim=c(0,365.25*5), main='dist = llogis')
# plot(timetodtx_exponential, xlim=c(0,365.25*5), main='dist = exponential')
# plot(timetodtx_lognormal, xlim=c(0,365.25*5), main='dist = lognormal')
# plot(timetodtx_spline, xlim=c(0,365.25*5), main='dist = spline')


## Assess goodness-of-fit with AIC
# timetodtx_genf$AIC
# timetodtx_genf.orig$AIC
# timetodtx_gengamma$AIC
# timetodtx_gengamma.orig$AIC
# timetodtx_exp$AIC
# timetodtx_weibull$AIC
# timetodtx_weibullPH$AIC
# timetodtx_lnorm$AIC
# timetodtx_gamma$AIC
# timetodtx_gompertz$AIC
# timetodtx_llogis$AIC
# timetodtx_exponential$AIC
# timetodtx_lognormal$AIC
# timetodtx_spline$AIC


## Use WeibullPH - close to lowest AIC, and provides a better visual fit than lowest AIC, and simpler model
model_timetodtx <- readRDS(paste0(modelsloc,'timetodtx_','weibullPH','.RDS'))

timetodtx_shape_mean <- model_timetodtx$coefficients['shape'] %>% exp() %>% as.numeric()
timetodtx_scale_mean <- model_timetodtx$coefficients['scale'] %>% exp() %>% as.numeric()
coef_timetodtx_bldgrpa_mean <- model_timetodtx$coefficients['bloodgroupA'] %>% as.numeric()
coef_timetodtx_bldgrpb_mean <- model_timetodtx$coefficients['bloodgroupB'] %>% as.numeric()
coef_timetodtx_bldgrpab_mean <- model_timetodtx$coefficients['bloodgroupAB'] %>% as.numeric()
coef_timetodtx_female_mean <- model_timetodtx$coefficients['female'] %>% as.numeric()
coef_timetodtx_agewtlst_mean <- model_timetodtx$coefficients['waitlist_age'] %>% as.numeric()
coef_timetodtx_comorbs_mean <- model_timetodtx$coefficients['comorbs'] %>% as.numeric()
coef_timetodtx_prevtxcount_mean <- model_timetodtx$coefficients['prevtxcount'] %>% as.numeric()

timetodtx_shape_se <- model_timetodtx$res['shape', "se"] %>% as.numeric()
timetodtx_scale_se <- model_timetodtx$res['scale', "se"] %>% as.numeric()
coef_timetodtx_bldgrpa_se <- model_timetodtx$res['bloodgroupA', "se"] %>% as.numeric()
coef_timetodtx_bldgrpb_se <- model_timetodtx$res['bloodgroupB', "se"] %>% as.numeric()
coef_timetodtx_bldgrpab_se <- model_timetodtx$res['bloodgroupAB', "se"] %>% as.numeric()
coef_timetodtx_female_se <- model_timetodtx$res['female', "se"] %>% as.numeric()
coef_timetodtx_agewtlst_se <- model_timetodtx$res['waitlist_age', "se"] %>% as.numeric()
coef_timetodtx_comorbs_se <- model_timetodtx$res['comorbs', "se"] %>% as.numeric()
coef_timetodtx_prevtxcount_se <- model_timetodtx$res['prevtxcount', "se"] %>% as.numeric()



# Time to living donor transplant ----
## Loop through all in-built distributions available for flexsurvreg()
# dists <- flexsurv.dists %>% as_tibble() %>% colnames()
# 
# for(dist in dists) {
#   tryCatch({
#   i <- grep(paste0('^',dist,'$'),dists)
# 
#   if (i==1) {
#     curr_t <- as.numeric(Sys.time())
#     prev_t <- curr_t
#     t <- 0
#   }
#   if (i>1) {
#     prev_t <- curr_t
#     curr_t <- as.numeric(Sys.time())
#     t <- round(curr_t - prev_t,0)
#   }
# 
#   progress <- paste0('Model ',
#                      i,
#                      ' of ',
#                      length(dists),
#                      ' (',
#                      dist,
#                      '): ',
#                      round(t,0),
#                      's')
#   print(progress)
# 
#   model <- flexsurvreg(data=cohort1,
#                        dist=dist,
#                        formula= Surv(starttime,endtime_ltx,ltx) ~
#                          bloodgroup + female + waitlist_age + comorbs + prevtxcount)
# 
#   distname <- sub('\\.','',dist)
# 
#   saveRDS(model,file.path(modelsloc,paste0('timetoltx_',distname,'.RDS')))
# 
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }
# 
# 
# ## Spline model
# model <- flexsurvspline(data=cohort1,
#                         knots=c(log(365.25*8)),
#                         formula= Surv(starttime,endtime_ltx,ltx) ~
#                           bloodgroup + female + waitlist_age + comorbs + prevtxcount)
# saveRDS(model,file.path(modelsloc,'timetoltx_spline.RDS'))
# 
# 
# ## Read all models into R, produce plots, and print AIC values
# for (dist in c(dists,'spline')) {
#   tryCatch({
#   assign(paste0('timetoltx_',dist),
#          readRDS(paste0(modelsloc,'timetoltx_',dist,'.RDS')))
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }


## Visually assess goodness-of-fit
# plot(timetoltx_genf, xlim=c(0,365.25*5), main='dist = genf')
# plot(timetoltx_genf.orig, xlim=c(0,365.25*5), main='dist = genf.orig')
# plot(timetoltx_gengamma, xlim=c(0,365.25*5), main='dist = gengamma')
# plot(timetoltx_gengamma.orig, xlim=c(0,365.25*5), main='dist = gengamma.orig')
# plot(timetoltx_exp, xlim=c(0,365.25*5), main='dist = exp')
# plot(timetoltx_weibull, xlim=c(0,365.25*5), main='dist = weibull')
# plot(timetoltx_weibullPH, xlim=c(0,365.25*5), main='dist = weibullPH')
# plot(timetoltx_lnorm, xlim=c(0,365.25*5), main='dist = lnorm')
# plot(timetoltx_gamma, xlim=c(0,365.25*5), main='dist = gamma')
# plot(timetoltx_gompertz, xlim=c(0,365.25*5), main='dist = gompertz')
# plot(timetoltx_llogis, xlim=c(0,365.25*5), main='dist = llogis')
# plot(timetoltx_exponential, xlim=c(0,365.25*5), main='dist = exponential')
# plot(timetoltx_lognormal, xlim=c(0,365.25*5), main='dist = lognormal')
# plot(timetoltx_spline, xlim=c(0,365.25*5), main='dist = spline')


## Assess goodness-of-fit with AIC
# timetoltx_genf$AIC
# timetoltx_genf.orig$AIC
# timetoltx_gengamma$AIC
# timetoltx_gengamma.orig$AIC
# timetoltx_exp$AIC
# timetoltx_weibull$AIC
# timetoltx_weibullPH$AIC
# timetoltx_lnorm$AIC
# timetoltx_gamma$AIC
# timetoltx_gompertz$AIC
# timetoltx_llogis$AIC
# timetoltx_exponential$AIC
# timetoltx_lognormal$AIC
# timetoltx_spline$AIC



## Use generalised F - close to lowest AIC, and provides a better visual fit than lowest AIC, and simpler model
model_timetoltx <- readRDS(paste0(modelsloc,'timetoltx_','genf','.RDS'))

timetoltx_mu_mean <- model_timetoltx$coefficients['mu'] %>% as.numeric()
timetoltx_sigma_mean <- model_timetoltx$coefficients['sigma'] %>% exp() %>% as.numeric()
timetoltx_Q_mean <- model_timetoltx$coefficients['Q'] %>% as.numeric()
timetoltx_P_mean <- model_timetoltx$coefficients['P'] %>% exp() %>% as.numeric()
coef_timetoltx_bldgrpa_mean <- model_timetoltx$coefficients['bloodgroupA'] %>% as.numeric()
coef_timetoltx_bldgrpb_mean <- model_timetoltx$coefficients['bloodgroupB'] %>% as.numeric()
coef_timetoltx_bldgrpab_mean <- model_timetoltx$coefficients['bloodgroupAB'] %>% as.numeric()
coef_timetoltx_female_mean <- model_timetoltx$coefficients['female'] %>% as.numeric()
coef_timetoltx_agewtlst_mean <- model_timetoltx$coefficients['waitlist_age'] %>% as.numeric()
coef_timetoltx_comorbs_mean <- model_timetoltx$coefficients['comorbs'] %>% as.numeric()
coef_timetoltx_prevtxcount_mean <- model_timetoltx$coefficients['prevtxcount'] %>% as.numeric()

timetoltx_mu_se <- model_timetoltx$res['mu', "se"] %>% as.numeric()
timetoltx_sigma_se <- model_timetoltx$res['sigma', "se"] %>% as.numeric()
timetoltx_Q_se <- model_timetoltx$res['Q', "se"] %>% as.numeric()
timetoltx_P_se <- model_timetoltx$res['P', "se"] %>% as.numeric()
coef_timetoltx_bldgrpa_se <- model_timetoltx$res['bloodgroupA', "se"] %>% as.numeric()
coef_timetoltx_bldgrpb_se <- model_timetoltx$res['bloodgroupB', "se"] %>% as.numeric()
coef_timetoltx_bldgrpab_se <- model_timetoltx$res['bloodgroupAB', "se"] %>% as.numeric()
coef_timetoltx_female_se <- model_timetoltx$res['female', "se"] %>% as.numeric()
coef_timetoltx_agewtlst_se <- model_timetoltx$res['waitlist_age', "se"] %>% as.numeric()
coef_timetoltx_comorbs_se <- model_timetoltx$res['comorbs', "se"] %>% as.numeric()
coef_timetoltx_prevtxcount_se <- model_timetoltx$res['prevtxcount', "se"] %>% as.numeric()



# Cancer incidence on waiting list (dialysis), standardised by age and sex ----
## Calculate total incidence of all cancers by age and sex
cancer_baseline_agesex <- aihw_cancerincidence %>% 
  filter(datatype %in% c('Actual'),
         dplyr::between(year, 2013, 2017),
         sex %in% c('Females', 'Males'),
         agegroup != 'All ages combined') %>%
  group_by(cancersite, sex, agegroup) %>% # Average rate over the last 5 years (assume population was steady over this period)
  mutate(avgrate = mean(agespecificrate, na.rm=TRUE)) %>%
  ungroup() %>%
  distinct(cancersite, sex, agegroup, avgrate) %>%
  group_by(sex, agegroup) %>% # Total rate of all cancers (by agegroup and sex)
  mutate(rate = sum(avgrate, na.rm=TRUE)) %>%
  ungroup() %>%
  distinct(sex, agegroup, rate)


## Calculate total incidence of all cancers (overall)
cancer_baseline_overall <- aihw_cancerincidence %>% 
  filter(datatype %in% c('Actual'),
         dplyr::between(year, 2013, 2017),
         sex == 'Persons',
         agegroup == 'All ages combined') %>%
  group_by(cancersite) %>% # Average rate over the last 5 years (assume population was steady over this period)
  mutate(avgrate = mean(agespecificrate, na.rm=TRUE)) %>%
  ungroup() %>%
  distinct(cancersite, avgrate) %>%
  mutate(rate = sum(avgrate, na.rm=TRUE)) %>%
  distinct(rate) %>%
  as.numeric()
  

## Cancer incidence rate ratio (compared dialysis vs. general population) 
## Based on Wong et al., 2016
cancer_dialysis_overall <- 22.2 * 100
cancer_irr_dialysis <- cancer_dialysis_overall / cancer_baseline_overall



## Create lookup tables of cancer incidence by age and sex
cancer_baseline <- cancer_baseline_agesex %>%
  mutate(ageyearscovered = ifelse(agegroup == '90+', 11, 5)) %>% 
  uncount(ageyearscovered) %>%
  group_by(sex, agegroup) %>%
  mutate(agegroupstart = as.numeric(str_extract(agegroup, '\\d{2}(?=[[:punct:]\\+])')),
         age = agegroupstart + row_number() - 1,
         female = (sex=='Females')*1,
         baseline = rate/100000) %>%
  ungroup() %>%
  dplyr::select(age, female, baseline)
  

cancer_wtlst <- cancer_baseline %>%
  mutate(annualrate = baseline * cancer_irr_dialysis) %>%
  dplyr::select(age, female, annualrate)




# Mortality by age and sex ----
## Standardised mortality ratio for people on dialysis (vs. general population)
## Based on ANZDATA Annual Report 2021 - Chapter 3 Figure 3.2.1 Dialysis Mortality
## Data behind figure extracted using Web Plot Digitizer: https://automeris.io/WebPlotDigitizer/
smr_dialysis <- anzdata_mortality_dialysis %>%
  rename(agegroup = age) %>%
  uncount(c(30, 10, 10, 10, 10, 10, 21)) %>% # Create a copy of each row based on how many ages it represents
  mutate(age = seq_len(n()) - 1) %>%
  uncount(2) %>% # Create a copy of each row, one for females, one for males
  group_by(age) %>%
  mutate(female = seq_len(n()) - 1) %>%
  ungroup() %>%
  mutate(smr_dialysis = ifelse(female == 1, irr_female, irr_male)) %>%
  dplyr::select(age, female, smr_dialysis)


## Standardised mortality ratio for people with kidney transplant (vs. general population)
## Based on ANZDATA Annual Report 2021 - Chapter 3 Figure 3.2.2 Transplant Mortality
## Data behind figure extracted using Web Plot Digitizer: https://automeris.io/WebPlotDigitizer/
smr_transplant <- anzdata_mortality_transplant %>%
  mutate(transplant_male = ifelse(is.na(transplant_male),
                                  dplyr::lead(transplant_male) * (transplant_female / dplyr::lead(transplant_female)),
                                  transplant_male),
         irr_male = ifelse(is.na(irr_male), 
                           transplant_male / genpop_male, 
                           irr_male)) %>%
  rename(agegroup = age) %>%
  uncount(c(30, 10, 10, 10, 10, 10, 21)) %>% # Create a copy of each row based on how many ages it represents
  mutate(age = seq_len(n()) - 1) %>%
  uncount(2) %>% # Create a copy of each row, one for females, one for males
  group_by(age) %>%
  mutate(female = seq_len(n()) - 1) %>%
  ungroup() %>%
  mutate(smr_transplant = ifelse(female == 1, irr_female, irr_male)) %>%
  dplyr::select(age, female, smr_transplant)



## Relative survival for people with cancer (vs. general population)
## Based on AIHW cancer data: https://www.aihw.gov.au/reports/cancer/cancer-data-in-australia/contents/about
cancer_relativesurvival_agesexyearssite <- aihw_cancersurvival %>%
  filter(survivaltype %in% c('Relative'),
         str_detect(period, '2013[:punct:]2017'), # The hyphen was the wrong type (long vs. short) so this was the easiest way to match
         sex %in% c('Females', 'Males'),
         agegroup != 'All ages combined') %>%
  mutate(survival = replace_na(as.numeric(survival), 1)) %>% # If there were no deaths, then relative survival is 1
  dplyr::select(cancersite, agegroup, sex, yearsafterdiagnosis, survival)


cancer_incidence_agesexsite <- aihw_cancerincidence %>%
  filter(datatype %in% c('Actual'),
         dplyr::between(year, 2013, 2017),
         sex %in% c('Females', 'Males'),
         agegroup != 'All ages combined') %>%
  group_by(cancersite, sex, agegroup) %>% # Cumulative incidence over the last 5 years
  mutate(cumulative_incidence = sum(count, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(agegroup = ifelse(agegroup %in% c('85-89', '90+'), '85+', agegroup)) %>%
  group_by(agegroup, sex) %>%
  mutate(cumulative_incidence = sum(cumulative_incidence)) %>%
  ungroup() %>%
  distinct(cancersite, agegroup, sex, cumulative_incidence)


cancer_relativesurvival_summary <- cancer_relativesurvival_agesexyearssite %>%
  left_join(cancer_incidence_agesexsite) %>%
  group_by(agegroup, sex, yearsafterdiagnosis) %>%
  mutate(total_cumulative_incidence = sum(cumulative_incidence, na.rm=TRUE),
         weight = cumulative_incidence / total_cumulative_incidence,
         weighted_survival = survival * weight,
         avgrelsurvival_cancer = sum(weighted_survival, na.rm=TRUE)) %>%
  ungroup() %>% 
  distinct(agegroup, sex, yearsafterdiagnosis, avgrelsurvival_cancer) %>%
  pivot_wider(id_cols = c(agegroup),
              values_from = avgrelsurvival_cancer,
              names_from = c(sex, yearsafterdiagnosis))


cancer_relativesurvival_agesex <- cancer_relativesurvival_agesexyearssite %>%
  left_join(cancer_incidence_agesexsite) %>%
  group_by(agegroup, sex, yearsafterdiagnosis) %>%
  mutate(total_cumulative_incidence = sum(cumulative_incidence, na.rm=TRUE),
         weight = cumulative_incidence / total_cumulative_incidence,
         weighted_survival = survival * weight,
         avgrelsurvival_cancer = sum(weighted_survival, na.rm=TRUE)) %>%
  ungroup() %>% 
  distinct(agegroup, sex, yearsafterdiagnosis, avgrelsurvival_cancer) %>%
  mutate(ageyearscovered = ifelse(agegroup == '85+', 16, 5),
         female = (sex == 'Females')*1,
         agegroupstart = as.numeric(str_extract(agegroup, '\\d{2}(?=[[:punct:]\\+])'))) %>%
  uncount(ageyearscovered) %>%
  group_by(agegroup, sex, yearsafterdiagnosis) %>%
  mutate(age = agegroupstart + row_number() - 1,
         years = yearsafterdiagnosis) %>%
  ungroup() %>%
  distinct(age, female, years, avgrelsurvival_cancer) %>%
  arrange(age, female, years) %>%
  group_by(age, female) %>%
  mutate(annualavgrelsurvival_cancer = ifelse(years == 1,
                                              avgrelsurvival_cancer,
                                              pmin(avgrelsurvival_cancer / lag(avgrelsurvival_cancer), 1))) %>% # Relative survival can't be greater than 100%
  ungroup() %>%
  dplyr::select(age, female, years, annualavgrelsurvival_cancer) %>%
  pivot_wider(names_from = 'years',
              names_prefix = 'annualavgrelsurvival_cancer_yr',
              values_from = 'annualavgrelsurvival_cancer') %>%
  dplyr::select(age, female, starts_with('annualavgrelsurvival_cancer_yr'))



## Create lookup tables for mortality
mortality_baseline <- lifetables_long_2017_2019 %>%
  rename(baseline=qx) %>%
  dplyr::select(age,female,baseline)


mortality_wtlst <- mortality_baseline %>%
  left_join(smr_dialysis) %>%
  mutate(annualrate=baseline * smr_dialysis) %>%
  dplyr::select(age,female,annualrate)


mortality_offlstc <- mortality_baseline %>%
  left_join(smr_dialysis) %>%
  left_join(cancer_relativesurvival_agesex) %>%
  mutate(annualrate_yr1 = 1 - annualavgrelsurvival_cancer_yr1*(1 - baseline*smr_dialysis),
         annualrate_yr2 = 1 - annualavgrelsurvival_cancer_yr2*(1 - baseline*smr_dialysis),
         annualrate_yr3 = 1 - annualavgrelsurvival_cancer_yr3*(1 - baseline*smr_dialysis),
         annualrate_yr4 = 1 - annualavgrelsurvival_cancer_yr4*(1 - baseline*smr_dialysis),
         annualrate_yr5 = 1 - annualavgrelsurvival_cancer_yr5*(1 - baseline*smr_dialysis)) %>%
  dplyr::select(age, female, starts_with('annualrate_yr'))


mortality_long_offlstc <- mortality_offlstc %>%
  pivot_longer(cols = starts_with('annualrate_yr'),
               names_to = 'years',
               values_to = 'annualrate') %>%
  mutate(years = as.numeric(str_extract(years, '\\d')))


mortality_tx <- mortality_baseline %>%
  left_join(smr_transplant) %>%
  mutate(annualrate=baseline * smr_transplant) %>%
  dplyr::select(age,female,annualrate)


mortality_txc <- mortality_baseline %>%
  left_join(smr_transplant) %>%
  left_join(cancer_relativesurvival_agesex) %>%
  mutate(annualrate_yr1 = 1 - annualavgrelsurvival_cancer_yr1*(1 - baseline*smr_transplant),
         annualrate_yr2 = 1 - annualavgrelsurvival_cancer_yr2*(1 - baseline*smr_transplant),
         annualrate_yr3 = 1 - annualavgrelsurvival_cancer_yr3*(1 - baseline*smr_transplant),
         annualrate_yr4 = 1 - annualavgrelsurvival_cancer_yr4*(1 - baseline*smr_transplant),
         annualrate_yr5 = 1 - annualavgrelsurvival_cancer_yr5*(1 - baseline*smr_transplant)) %>%
  dplyr::select(age, female, starts_with('annualrate_yr'))


mortality_long_txc <- mortality_txc %>%
  pivot_longer(cols = starts_with('annualrate_yr'),
               names_to = 'years',
               values_to = 'annualrate') %>%
  mutate(years = as.numeric(str_extract(years, '\\d')))


mortality_tf <- mortality_wtlst


mortality_tfc <- mortality_offlstc


mortality_long_tfc <- mortality_tfc %>%
  pivot_longer(cols = starts_with('annualrate_yr'),
               names_to = 'years',
               values_to = 'annualrate') %>%
  mutate(years = as.numeric(str_extract(years, '\\d')))



# Donor characteristics ----
## Donor PBM (simulated before other donor characteristics so characteristics of new PBM donors can be simulated correctly)
model_donorpbm <- glm(data=cohort2,
                      family=binomial,
                      formula= donorpbm ~ donordeceased + ageattransplant + 
                        female + bloodgroup + comorbs + prevtxcount)

coef_donorpbm_int_mean <- model_donorpbm$coefficients["(Intercept)"] %>% as.numeric()
coef_donorpbm_donordeceased_mean <- model_donorpbm$coefficients['donordeceased'] %>% as.numeric()
coef_donorpbm_agetx_mean <- model_donorpbm$coefficients['ageattransplant'] %>% as.numeric()
coef_donorpbm_female_mean <- model_donorpbm$coefficients['female'] %>% as.numeric()
coef_donorpbm_bldgrpa_mean <- model_donorpbm$coefficients['bloodgroupA'] %>% as.numeric()
coef_donorpbm_bldgrpb_mean <- model_donorpbm$coefficients['bloodgroupB'] %>% as.numeric()
coef_donorpbm_bldgrpab_mean <- model_donorpbm$coefficients['bloodgroupAB'] %>% as.numeric()
coef_donorpbm_comorbs_mean <- model_donorpbm$coefficients['comorbs'] %>% as.numeric()
coef_donorpbm_prevtxcount_mean <- model_donorpbm$coefficients['prevtxcount'] %>% as.numeric()

coef_donorpbm_int_se <- summary(model_donorpbm)$coefficients["(Intercept)", "Std. Error"] %>% as.numeric()
coef_donorpbm_donordeceased_se <- summary(model_donorpbm)$coefficients['donordeceased', "Std. Error"] %>% as.numeric()
coef_donorpbm_agetx_se <- summary(model_donorpbm)$coefficients['ageattransplant', "Std. Error"] %>% as.numeric()
coef_donorpbm_female_se <- summary(model_donorpbm)$coefficients['female', "Std. Error"] %>% as.numeric()
coef_donorpbm_bldgrpa_se <- summary(model_donorpbm)$coefficients['bloodgroupA', "Std. Error"] %>% as.numeric()
coef_donorpbm_bldgrpb_se <- summary(model_donorpbm)$coefficients['bloodgroupB', "Std. Error"] %>% as.numeric()
coef_donorpbm_bldgrpab_se <- summary(model_donorpbm)$coefficients['bloodgroupAB', "Std. Error"] %>% as.numeric()
coef_donorpbm_comorbs_se <- summary(model_donorpbm)$coefficients['comorbs', "Std. Error"] %>% as.numeric()
coef_donorpbm_prevtxcount_se <- summary(model_donorpbm)$coefficients['prevtxcount', "Std. Error"] %>% as.numeric()



## Deceased donor type
model_donortype <- multinom(data = filter(cohort2, donortype != 'Living'),
                           family = binomial,
                           formula = donortype ~ ageattransplant + bloodgroup + 
                             female + comorbs + prevtxcount + donorpbm)

coef_donortype_dbdscd_int_mean <- summary(model_donortype)$coefficients['DBDSCD','(Intercept)'] %>% as.numeric()
coef_donortype_dbdscd_agetx_mean <- summary(model_donortype)$coefficients['DBDSCD','ageattransplant'] %>% as.numeric()
coef_donortype_dbdscd_bldgrpa_mean <- summary(model_donortype)$coefficients['DBDSCD','bloodgroupA'] %>% as.numeric()
coef_donortype_dbdscd_bldgrpb_mean <- summary(model_donortype)$coefficients['DBDSCD','bloodgroupB'] %>% as.numeric()
coef_donortype_dbdscd_bldgrpab_mean <- summary(model_donortype)$coefficients['DBDSCD','bloodgroupAB'] %>% as.numeric()
coef_donortype_dbdscd_female_mean <- summary(model_donortype)$coefficients['DBDSCD','female'] %>% as.numeric()
coef_donortype_dbdscd_comorbs_mean <- summary(model_donortype)$coefficients['DBDSCD','comorbs'] %>% as.numeric()
coef_donortype_dbdscd_prevtxcount_mean <- summary(model_donortype)$coefficients['DBDSCD','prevtxcount'] %>% as.numeric()
coef_donortype_dbdscd_donorpbm_mean <- summary(model_donortype)$coefficients['DBDSCD','donorpbm'] %>% as.numeric()

coef_donortype_dbdscd_int_se <- summary(model_donortype)$standard.errors['DBDSCD','(Intercept)'] %>% as.numeric()
coef_donortype_dbdscd_agetx_se <- summary(model_donortype)$standard.errors['DBDSCD','ageattransplant'] %>% as.numeric()
coef_donortype_dbdscd_bldgrpa_se <- summary(model_donortype)$standard.errors['DBDSCD','bloodgroupA'] %>% as.numeric()
coef_donortype_dbdscd_bldgrpb_se <- summary(model_donortype)$standard.errors['DBDSCD','bloodgroupB'] %>% as.numeric()
coef_donortype_dbdscd_bldgrpab_se <- summary(model_donortype)$standard.errors['DBDSCD','bloodgroupAB'] %>% as.numeric()
coef_donortype_dbdscd_female_se <- summary(model_donortype)$standard.errors['DBDSCD','female'] %>% as.numeric()
coef_donortype_dbdscd_comorbs_se <- summary(model_donortype)$standard.errors['DBDSCD','comorbs'] %>% as.numeric()
coef_donortype_dbdscd_prevtxcount_se <- summary(model_donortype)$standard.errors['DBDSCD','prevtxcount'] %>% as.numeric()
coef_donortype_dbdscd_donorpbm_se <- summary(model_donortype)$standard.errors['DBDSCD','donorpbm'] %>% as.numeric()

coef_donortype_dbdecd_int_mean <- summary(model_donortype)$coefficients['DBDECD','(Intercept)'] %>% as.numeric()
coef_donortype_dbdecd_agetx_mean <- summary(model_donortype)$coefficients['DBDECD','ageattransplant'] %>% as.numeric()
coef_donortype_dbdecd_bldgrpa_mean <- summary(model_donortype)$coefficients['DBDECD','bloodgroupA'] %>% as.numeric()
coef_donortype_dbdecd_bldgrpb_mean <- summary(model_donortype)$coefficients['DBDECD','bloodgroupB'] %>% as.numeric()
coef_donortype_dbdecd_bldgrpab_mean <- summary(model_donortype)$coefficients['DBDECD','bloodgroupAB'] %>% as.numeric()
coef_donortype_dbdecd_female_mean <- summary(model_donortype)$coefficients['DBDECD','female'] %>% as.numeric()
coef_donortype_dbdecd_comorbs_mean <- summary(model_donortype)$coefficients['DBDECD','comorbs'] %>% as.numeric()
coef_donortype_dbdecd_prevtxcount_mean <- summary(model_donortype)$coefficients['DBDECD','prevtxcount'] %>% as.numeric()
coef_donortype_dbdecd_donorpbm_mean <- summary(model_donortype)$coefficients['DBDECD','donorpbm'] %>% as.numeric()

coef_donortype_dbdecd_int_se <- summary(model_donortype)$standard.errors['DBDECD','(Intercept)'] %>% as.numeric()
coef_donortype_dbdecd_agetx_se <- summary(model_donortype)$standard.errors['DBDECD','ageattransplant'] %>% as.numeric()
coef_donortype_dbdecd_bldgrpa_se <- summary(model_donortype)$standard.errors['DBDECD','bloodgroupA'] %>% as.numeric()
coef_donortype_dbdecd_bldgrpb_se <- summary(model_donortype)$standard.errors['DBDECD','bloodgroupB'] %>% as.numeric()
coef_donortype_dbdecd_bldgrpab_se <- summary(model_donortype)$standard.errors['DBDECD','bloodgroupAB'] %>% as.numeric()
coef_donortype_dbdecd_female_se <- summary(model_donortype)$standard.errors['DBDECD','female'] %>% as.numeric()
coef_donortype_dbdecd_comorbs_se <- summary(model_donortype)$standard.errors['DBDECD','comorbs'] %>% as.numeric()
coef_donortype_dbdecd_prevtxcount_se <- summary(model_donortype)$standard.errors['DBDECD','prevtxcount'] %>% as.numeric()
coef_donortype_dbdecd_donorpbm_se <- summary(model_donortype)$standard.errors['DBDECD','donorpbm'] %>% as.numeric()


## Donor sex
model_donorfemale <- glm(data=cohort2,
                         family=binomial,
                         formula= donorfemale ~ donortype + donorpbm + 
                           ageattransplant + bloodgroup + female + comorbs + 
                           prevtxcount)

coef_donorfemale_int_mean <- model_donorfemale$coefficients["(Intercept)"] %>% as.numeric()
coef_donorfemale_donortypedcd_mean <- model_donorfemale$coefficients['donortypeDCD'] %>% as.numeric()
coef_donorfemale_donortypedbdscd_mean <- model_donorfemale$coefficients['donortypeDBDSCD'] %>% as.numeric()
coef_donorfemale_donortypedbdecd_mean <- model_donorfemale$coefficients['donortypeDBDECD'] %>% as.numeric()
coef_donorfemale_donorpbm_mean <- model_donorfemale$coefficients['donorpbm'] %>% as.numeric()
coef_donorfemale_agetx_mean <- model_donorfemale$coefficients['ageattransplant'] %>% as.numeric()
coef_donorfemale_bldgrpa_mean <- model_donorfemale$coefficients['bloodgroupA'] %>% as.numeric()
coef_donorfemale_bldgrpb_mean <- model_donorfemale$coefficients['bloodgroupB'] %>% as.numeric()
coef_donorfemale_bldgrpab_mean <- model_donorfemale$coefficients['bloodgroupAB'] %>% as.numeric()
coef_donorfemale_female_mean <- model_donorfemale$coefficients['female'] %>% as.numeric()
coef_donorfemale_comorbs_mean <- model_donorfemale$coefficients['comorbs'] %>% as.numeric()
coef_donorfemale_prevtxcount_mean <- model_donorfemale$coefficients['prevtxcount'] %>% as.numeric()

coef_donorfemale_int_se <- summary(model_donorfemale)$coefficients["(Intercept)", "Std. Error"] %>% as.numeric()
coef_donorfemale_donortypedcd_se <- summary(model_donorfemale)$coefficients['donortypeDCD', "Std. Error"] %>% as.numeric()
coef_donorfemale_donortypedbdscd_se <- summary(model_donorfemale)$coefficients['donortypeDBDSCD', "Std. Error"] %>% as.numeric()
coef_donorfemale_donortypedbdecd_se <- summary(model_donorfemale)$coefficients['donortypeDBDECD', "Std. Error"] %>% as.numeric()
coef_donorfemale_donorpbm_se <- summary(model_donorfemale)$coefficients['donorpbm', "Std. Error"] %>% as.numeric()
coef_donorfemale_agetx_se <- summary(model_donorfemale)$coefficients['ageattransplant', "Std. Error"] %>% as.numeric()
coef_donorfemale_bldgrpa_se <- summary(model_donorfemale)$coefficients['bloodgroupA', "Std. Error"] %>% as.numeric()
coef_donorfemale_bldgrpb_se <- summary(model_donorfemale)$coefficients['bloodgroupB', "Std. Error"] %>% as.numeric()
coef_donorfemale_bldgrpab_se <- summary(model_donorfemale)$coefficients['bloodgroupAB', "Std. Error"] %>% as.numeric()
coef_donorfemale_female_se <- summary(model_donorfemale)$coefficients['female', "Std. Error"] %>% as.numeric()
coef_donorfemale_comorbs_se <- summary(model_donorfemale)$coefficients['comorbs', "Std. Error"] %>% as.numeric()
coef_donorfemale_prevtxcount_se <- summary(model_donorfemale)$coefficients['prevtxcount', "Std. Error"] %>% as.numeric()



## Donor age
model_donorage <- glm(data = cohort2,
                      family = gaussian,
                      formula = donorage ~ donorfemale + donortype + donorpbm + 
                        ageattransplant + bloodgroup + female + comorbs + 
                        prevtxcount)

coef_donorage_int_mean <- model_donorage$coefficients['(Intercept)'] %>% as.numeric()
coef_donorage_donorfemale_mean <- model_donorage$coefficients['donorfemale'] %>% as.numeric()
coef_donorage_donortypedcd_mean <- model_donorage$coefficients['donortypeDCD'] %>% as.numeric()
coef_donorage_donortypedbdscd_mean <- model_donorage$coefficients['donortypeDBDSCD'] %>% as.numeric()
coef_donorage_donortypedbdecd_mean <- model_donorage$coefficients['donortypeDBDECD'] %>% as.numeric()
coef_donorage_donorpbm_mean <- model_donorage$coefficients['donorpbm'] %>% as.numeric()
coef_donorage_agetx_mean <- model_donorage$coefficients['ageattransplant'] %>% as.numeric()
coef_donorage_bldgrpa_mean <- model_donorage$coefficients['bloodgroupA'] %>% as.numeric()
coef_donorage_bldgrpb_mean <- model_donorage$coefficients['bloodgroupB'] %>% as.numeric()
coef_donorage_bldgrpab_mean <- model_donorage$coefficients['bloodgroupAB'] %>% as.numeric()
coef_donorage_female_mean <- model_donorage$coefficients['female'] %>% as.numeric()
coef_donorage_comorbs_mean <- model_donorage$coefficients['comorbs'] %>% as.numeric()
coef_donorage_prevtxcount_mean <- model_donorage$coefficients['prevtxcount'] %>% as.numeric()
rmse_donorage <- sqrt(sum(model_donorage$residuals^2) / model_donorage$df.null)

coef_donorage_int_se <- summary(model_donorage)$coefficients['(Intercept)', "Std. Error"] %>% as.numeric()
coef_donorage_donorfemale_se <- summary(model_donorage)$coefficients['donorfemale', "Std. Error"] %>% as.numeric()
coef_donorage_donortypedcd_se <- summary(model_donorage)$coefficients['donortypeDCD', "Std. Error"] %>% as.numeric()
coef_donorage_donortypedbdscd_se <- summary(model_donorage)$coefficients['donortypeDBDSCD', "Std. Error"] %>% as.numeric()
coef_donorage_donortypedbdecd_se <- summary(model_donorage)$coefficients['donortypeDBDECD', "Std. Error"] %>% as.numeric()
coef_donorage_donorpbm_se <- summary(model_donorage)$coefficients['donorpbm', "Std. Error"] %>% as.numeric()
coef_donorage_agetx_se <- summary(model_donorage)$coefficients['ageattransplant', "Std. Error"] %>% as.numeric()
coef_donorage_bldgrpa_se <- summary(model_donorage)$coefficients['bloodgroupA', "Std. Error"] %>% as.numeric()
coef_donorage_bldgrpb_se <- summary(model_donorage)$coefficients['bloodgroupB', "Std. Error"] %>% as.numeric()
coef_donorage_bldgrpab_se <- summary(model_donorage)$coefficients['bloodgroupAB', "Std. Error"] %>% as.numeric()
coef_donorage_female_se <- summary(model_donorage)$coefficients['female', "Std. Error"] %>% as.numeric()
coef_donorage_comorbs_se <- summary(model_donorage)$coefficients['comorbs', "Std. Error"] %>% as.numeric()
coef_donorage_prevtxcount_se <- summary(model_donorage)$coefficients['prevtxcount', "Std. Error"] %>% as.numeric()



## Donor KDPI
## Opted for linear regression of log-odds of KDPI (to transform KDPI to a continuous variable)
## alternatively could use beta regression, but linear seems simpler and more than adequate
cohort2 <- cohort2 %>% 
  mutate(donorkdpi_logodds = log((donorkdpi/100) / (1 - (donorkdpi/100))))

model_donorkdpi <- glm(data = cohort2 %>% filter(!is.na(donorkdpi_logodds) & donorkdpi_logodds != Inf),
                       family = gaussian,
                       formula = donorkdpi_logodds ~ donorage + donorfemale + 
                         donortype + donorpbm + ageattransplant + bloodgroup + 
                         female + comorbs + prevtxcount)

coef_donorkdpi_int_mean <- model_donorkdpi$coefficients['(Intercept)'] %>% as.numeric()
coef_donorkdpi_donorfemale_mean <- model_donorkdpi$coefficients['donorfemale'] %>% as.numeric()
coef_donorkdpi_donorage_mean <- model_donorkdpi$coefficients['donorage'] %>% as.numeric()
coef_donorkdpi_donortypedbdscd_mean <- model_donorkdpi$coefficients['donortypeDBDSCD'] %>% as.numeric()
coef_donorkdpi_donortypedbdecd_mean <- model_donorkdpi$coefficients['donortypeDBDECD'] %>% as.numeric()
coef_donorkdpi_donorpbm_mean <- model_donorkdpi$coefficients['donorpbm'] %>% as.numeric()
coef_donorkdpi_agetx_mean <- model_donorkdpi$coefficients['ageattransplant'] %>% as.numeric()
coef_donorkdpi_bldgrpa_mean <- model_donorkdpi$coefficients['bloodgroupA'] %>% as.numeric()
coef_donorkdpi_bldgrpb_mean <- model_donorkdpi$coefficients['bloodgroupB'] %>% as.numeric()
coef_donorkdpi_bldgrpab_mean <- model_donorkdpi$coefficients['bloodgroupAB'] %>% as.numeric()
coef_donorkdpi_female_mean <- model_donorkdpi$coefficients['female'] %>% as.numeric()
coef_donorkdpi_comorbs_mean <- model_donorkdpi$coefficients['comorbs'] %>% as.numeric()
coef_donorkdpi_prevtxcount_mean <- model_donorkdpi$coefficients['prevtxcount'] %>% as.numeric()
rmse_donorkdpi <- sqrt(sum(model_donorkdpi$residuals^2) / model_donorkdpi$df.null)

coef_donorkdpi_int_se <- summary(model_donorkdpi)$coefficients['(Intercept)', "Std. Error"] %>% as.numeric()
coef_donorkdpi_donorfemale_se <- summary(model_donorkdpi)$coefficients['donorfemale', "Std. Error"] %>% as.numeric()
coef_donorkdpi_donorage_se <- summary(model_donorkdpi)$coefficients['donorage', "Std. Error"] %>% as.numeric()
coef_donorkdpi_donortypedbdscd_se <- summary(model_donorkdpi)$coefficients['donortypeDBDSCD', "Std. Error"] %>% as.numeric()
coef_donorkdpi_donortypedbdecd_se <- summary(model_donorkdpi)$coefficients['donortypeDBDECD', "Std. Error"] %>% as.numeric()
coef_donorkdpi_donorpbm_se <- summary(model_donorkdpi)$coefficients['donorpbm', "Std. Error"] %>% as.numeric()
coef_donorkdpi_agetx_se <- summary(model_donorkdpi)$coefficients['ageattransplant', "Std. Error"] %>% as.numeric()
coef_donorkdpi_bldgrpa_se <- summary(model_donorkdpi)$coefficients['bloodgroupA', "Std. Error"] %>% as.numeric()
coef_donorkdpi_bldgrpb_se <- summary(model_donorkdpi)$coefficients['bloodgroupB', "Std. Error"] %>% as.numeric()
coef_donorkdpi_bldgrpab_se <- summary(model_donorkdpi)$coefficients['bloodgroupAB', "Std. Error"] %>% as.numeric()
coef_donorkdpi_female_se <- summary(model_donorkdpi)$coefficients['female', "Std. Error"] %>% as.numeric()
coef_donorkdpi_comorbs_se <- summary(model_donorkdpi)$coefficients['comorbs', "Std. Error"] %>% as.numeric()
coef_donorkdpi_prevtxcount_se <- summary(model_donorkdpi)$coefficients['prevtxcount', "Std. Error"] %>% as.numeric()



# Time to recipient cancer from deceased donor transplant ----
# Loop through all in-built distributions available for flexsurvreg()
# dists <- flexsurv.dists %>% as_tibble() %>% colnames()
# 
# for(dist in dists) {
# 
#   tryCatch({
#   i <- grep(paste0('^',dist,'$'),dists)
# 
#   if (i==1) {
#     curr_t <- as.numeric(Sys.time())
#     prev_t <- curr_t
#     t <- 0
#   }
#   if (i>1) {
#     prev_t <- curr_t
#     curr_t <- as.numeric(Sys.time())
#     t <- round(curr_t - prev_t,0)
#   }
# 
#   progress <- paste0('Model ',
#                      i,
#                      ' of ',
#                      length(dists),
#                      ' (',
#                      dist,
#                      '): ',
#                      round(t,0),
#                      's')
#   print(progress)
# 
#   model <- flexsurvreg(data=cohort2,
#                        dist=dist,
#                        formula= Surv(starttime,endtime_cancer,cancer) ~
#                          donortype + donorage + donorfemale + donorkdpi + ageattransplant + female +
#                          bloodgroup + comorbs + prevtxcount)
# 
#   distname <- sub('\\.','',dist)
# 
#   saveRDS(model,file.path(modelsloc,paste0('timetotxcancer_',distname,'.RDS')))
# 
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }
# 
# 
# ## Spline model
# model <- flexsurvspline(data=cohort2,
#                         k=5, # Tried a few different numbers, as well as manually specifying locations. 5 seems optimal
#                         formula= Surv(starttime,endtime_cancer,cancer) ~
#                           donortype + donorage + donorfemale + donorkdpi + ageattransplant + female +
#                           bloodgroup + comorbs + prevtxcount)
# saveRDS(model,file.path(modelsloc,'timetotxcancer_spline.RDS'))
# 
# 
# ## Read all models into R, produce plots, and print AIC values
# for (dist in c(dists,'spline')) {
#   tryCatch({
#     assign(paste0('timetotxcancer_',dist),
#            readRDS(paste0(modelsloc,'timetotxcancer_',dist,'.RDS')))
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }



## Visually assess goodness-of-fit
# plot(timetotxcancer_genf, xlim=c(0,365.25*25), main='dist = genf')
# plot(timetotxcancer_genf.orig, xlim=c(0,365.25*25), main='dist = genf.orig')
# plot(timetotxcancer_gengamma, xlim=c(0,365.25*25), main='dist = gengamma')
# plot(timetotxcancer_gengamma.orig, xlim=c(0,365.25*25), main='dist = gengamma.orig')
# plot(timetotxcancer_exp, xlim=c(0,365.25*25), main='dist = exp')
# plot(timetotxcancer_weibull, xlim=c(0,365.25*25), main='dist = weibull')
# plot(timetotxcancer_weibullPH, xlim=c(0,365.25*25), main='dist = weibullPH')
# plot(timetotxcancer_lnorm, xlim=c(0,365.25*25), main='dist = lnorm')
# plot(timetotxcancer_gamma, xlim=c(0,365.25*25), main='dist = gamma')
# plot(timetotxcancer_gompertz, xlim=c(0,365.25*25), main='dist = gompertz')
# plot(timetotxcancer_llogis, xlim=c(0,365.25*25), main='dist = llogis')
# plot(timetotxcancer_exponential, xlim=c(0,365.25*25), main='dist = exponential')
# plot(timetotxcancer_lognormal, xlim=c(0,365.25*25), main='dist = lognormal')
# plot(timetotxcancer_spline, xlim=c(0,365.25*25), main='dist = spline')


## Assess goodness-of-fit with AIC
# timetotxcancer_genf$AIC
# timetotxcancer_genf.orig$AIC
# timetotxcancer_gengamma$AIC
# timetotxcancer_gengamma.orig$AIC
# timetotxcancer_exp$AIC
# timetotxcancer_weibull$AIC
# timetotxcancer_weibullPH$AIC
# timetotxcancer_lnorm$AIC
# timetotxcancer_gamma$AIC
# timetotxcancer_gompertz$AIC
# timetotxcancer_llogis$AIC
# timetotxcancer_exponential$AIC
# timetotxcancer_lognormal$AIC
# timetotxcancer_spline$AIC


## Use weibullPH - lowest AIC and all models have a good visual fit
model_timetotxcancer <- readRDS(paste0(modelsloc,'timetotxcancer_','weibullPH','.RDS'))

timetotxcancer_shape_mean <- model_timetotxcancer$coefficients['shape'] %>% exp() %>% as.numeric()
timetotxcancer_scale_mean <- model_timetotxcancer$coefficients['scale'] %>% exp() %>% as.numeric()
coef_timetotxcancer_donorage_mean <- model_timetotxcancer$coefficients['donorage'] %>% as.numeric()
coef_timetotxcancer_donorfemale_mean <- model_timetotxcancer$coefficients['donorfemale'] %>% as.numeric()
coef_timetotxcancer_donorkdpi_mean <- model_timetotxcancer$coefficients['donorkdpi'] %>% as.numeric()
coef_timetotxcancer_donortypedbdscd_mean <- model_timetotxcancer$coefficients['donortypeDBDSCD'] %>% as.numeric()
coef_timetotxcancer_donortypedbdecd_mean <- model_timetotxcancer$coefficients['donortypeDBDECD'] %>% as.numeric()
coef_timetotxcancer_bldgrpa_mean <- model_timetotxcancer$coefficients['bloodgroupA'] %>% as.numeric()
coef_timetotxcancer_bldgrpb_mean <- model_timetotxcancer$coefficients['bloodgroupB'] %>% as.numeric()
coef_timetotxcancer_bldgrpab_mean <- model_timetotxcancer$coefficients['bloodgroupAB'] %>% as.numeric()
coef_timetotxcancer_female_mean <- model_timetotxcancer$coefficients['female'] %>% as.numeric()
coef_timetotxcancer_agetx_mean <- model_timetotxcancer$coefficients['ageattransplant'] %>% as.numeric()
coef_timetotxcancer_comorbs_mean <- model_timetotxcancer$coefficients['comorbs'] %>% as.numeric()
coef_timetotxcancer_prevtxcount_mean <- model_timetotxcancer$coefficients['prevtxcount'] %>% as.numeric()

timetotxcancer_shape_se <- model_timetotxcancer$res['shape', "se"] %>% as.numeric()
timetotxcancer_scale_se <- model_timetotxcancer$res['scale', "se"] %>% as.numeric()
coef_timetotxcancer_donorage_se <- model_timetotxcancer$res['donorage', "se"] %>% as.numeric()
coef_timetotxcancer_donorfemale_se <- model_timetotxcancer$res['donorfemale', "se"] %>% as.numeric()
coef_timetotxcancer_donorkdpi_se <- model_timetotxcancer$res['donorkdpi', "se"] %>% as.numeric()
coef_timetotxcancer_donortypedbdscd_se <- model_timetotxcancer$res['donortypeDBDSCD', "se"] %>% as.numeric()
coef_timetotxcancer_donortypedbdecd_se <- model_timetotxcancer$res['donortypeDBDECD', "se"] %>% as.numeric()
coef_timetotxcancer_bldgrpa_se <- model_timetotxcancer$res['bloodgroupA', "se"] %>% as.numeric()
coef_timetotxcancer_bldgrpb_se <- model_timetotxcancer$res['bloodgroupB', "se"] %>% as.numeric()
coef_timetotxcancer_bldgrpab_se <- model_timetotxcancer$res['bloodgroupAB', "se"] %>% as.numeric()
coef_timetotxcancer_female_se <- model_timetotxcancer$res['female', "se"] %>% as.numeric()
coef_timetotxcancer_agetx_se <- model_timetotxcancer$res['ageattransplant', "se"] %>% as.numeric()
coef_timetotxcancer_comorbs_se <- model_timetotxcancer$res['comorbs', "se"] %>% as.numeric()
coef_timetotxcancer_prevtxcount_se <- model_timetotxcancer$res['prevtxcount', "se"] %>% as.numeric()




# Time to recipient cancer from living donor transplant ----
## Loop through all in-built distributions available for flexsurvreg()
# dists <- flexsurv.dists %>% as_tibble() %>% colnames()
# 
# for(dist in dists) {
# 
#   tryCatch({
#   i <- grep(paste0('^',dist,'$'),dists)
# 
#   if (i==1) {
#     curr_t <- as.numeric(Sys.time())
#     prev_t <- curr_t
#     t <- 0
#   }
#   if (i>1) {
#     prev_t <- curr_t
#     curr_t <- as.numeric(Sys.time())
#     t <- round(curr_t - prev_t,0)
#   }
# 
#   progress <- paste0('Model ',
#                      i,
#                      ' of ',
#                      length(dists),
#                      ' (',
#                      dist,
#                      '): ',
#                      round(t,0),
#                      's')
#   print(progress)
# 
#   model <- flexsurvreg(data=cohort2 %>% filter(donortype == "Living"),
#                        dist=dist,
#                        formula= Surv(starttime,endtime_cancer,cancer) ~
#                          donorage + donorfemale + ageattransplant + female +
#                          bloodgroup + comorbs + prevtxcount)
# 
#   distname <- sub('\\.','',dist)
# 
#   saveRDS(model,file.path(modelsloc,paste0('timetoltxcancer_',distname,'.RDS')))
# 
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }
# 
# 
# ## Spline model
# model <- flexsurvspline(data=cohort2 %>% filter(donortype == "Living"),
#                         k=5, # Tried a few different numbers, as well as manually specifying locations. 5 seems optimal
#                         formula= Surv(starttime,endtime_cancer,cancer) ~
#                           donorage + donorfemale + ageattransplant + female +
#                           bloodgroup + comorbs + prevtxcount)
# saveRDS(model,file.path(modelsloc,'timetoltxcancer_spline.RDS'))
# 
# 
# ## Read all models into R, produce plots, and print AIC values
# for (dist in c(dists,'spline')) {
#   tryCatch({
#     assign(paste0('timetoltxcancer_',dist),
#            readRDS(paste0(modelsloc,'timetoltxcancer_',dist,'.RDS')))
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }



## Visually assess goodness-of-fit
# plot(timetoltxcancer_genf, xlim=c(0,365.25*25), main='dist = genf')
# plot(timetoltxcancer_genf.orig, xlim=c(0,365.25*25), main='dist = genf.orig')
# plot(timetoltxcancer_gengamma, xlim=c(0,365.25*25), main='dist = gengamma')
# plot(timetoltxcancer_gengamma.orig, xlim=c(0,365.25*25), main='dist = gengamma.orig')
# plot(timetoltxcancer_exp, xlim=c(0,365.25*25), main='dist = exp')
# plot(timetoltxcancer_weibull, xlim=c(0,365.25*25), main='dist = weibull')
# plot(timetoltxcancer_weibullPH, xlim=c(0,365.25*25), main='dist = weibullPH')
# plot(timetoltxcancer_lnorm, xlim=c(0,365.25*25), main='dist = lnorm')
# plot(timetoltxcancer_gamma, xlim=c(0,365.25*25), main='dist = gamma')
# plot(timetoltxcancer_gompertz, xlim=c(0,365.25*25), main='dist = gompertz')
# plot(timetoltxcancer_llogis, xlim=c(0,365.25*25), main='dist = llogis')
# plot(timetoltxcancer_exponential, xlim=c(0,365.25*25), main='dist = exponential')
# plot(timetoltxcancer_lognormal, xlim=c(0,365.25*25), main='dist = lognormal')
# plot(timetoltxcancer_spline, xlim=c(0,365.25*25), main='dist = spline')


## Assess goodness-of-fit with AIC
# timetoltxcancer_genf$AIC
# timetoltxcancer_genf.orig$AIC
# timetoltxcancer_gengamma$AIC
# timetoltxcancer_gengamma.orig$AIC
# timetoltxcancer_exp$AIC
# timetoltxcancer_weibull$AIC
# timetoltxcancer_weibullPH$AIC
# timetoltxcancer_lnorm$AIC
# timetoltxcancer_gamma$AIC
# timetoltxcancer_gompertz$AIC
# timetoltxcancer_llogis$AIC
# timetoltxcancer_exponential$AIC
# timetoltxcancer_lognormal$AIC
# timetoltxcancer_spline$AIC


## Use weibullPH - same as deceased donor model, and all models have similar AIC and good visual fit
model_timetoltxcancer <- readRDS(paste0(modelsloc,'timetoltxcancer_','weibullPH','.RDS'))

timetoltxcancer_shape_mean <- model_timetoltxcancer$coefficients['shape'] %>% exp() %>% as.numeric()
timetoltxcancer_scale_mean <- model_timetoltxcancer$coefficients['scale'] %>% exp() %>% as.numeric()
coef_timetoltxcancer_donorage_mean <- model_timetoltxcancer$coefficients['donorage'] %>% as.numeric()
coef_timetoltxcancer_donorfemale_mean <- model_timetoltxcancer$coefficients['donorfemale'] %>% as.numeric()
coef_timetoltxcancer_bldgrpa_mean <- model_timetoltxcancer$coefficients['bloodgroupA'] %>% as.numeric()
coef_timetoltxcancer_bldgrpb_mean <- model_timetoltxcancer$coefficients['bloodgroupB'] %>% as.numeric()
coef_timetoltxcancer_bldgrpab_mean <- model_timetoltxcancer$coefficients['bloodgroupAB'] %>% as.numeric()
coef_timetoltxcancer_female_mean <- model_timetoltxcancer$coefficients['female'] %>% as.numeric()
coef_timetoltxcancer_agetx_mean <- model_timetoltxcancer$coefficients['ageattransplant'] %>% as.numeric()
coef_timetoltxcancer_comorbs_mean <- model_timetoltxcancer$coefficients['comorbs'] %>% as.numeric()
coef_timetoltxcancer_prevtxcount_mean <- model_timetoltxcancer$coefficients['prevtxcount'] %>% as.numeric()

timetoltxcancer_shape_se <- model_timetoltxcancer$res['shape', "se"] %>% as.numeric()
timetoltxcancer_scale_se <- model_timetoltxcancer$res['scale', "se"] %>% as.numeric()
coef_timetoltxcancer_donorage_se <- model_timetoltxcancer$res['donorage', "se"] %>% as.numeric()
coef_timetoltxcancer_donorfemale_se <- model_timetoltxcancer$res['donorfemale', "se"] %>% as.numeric()
coef_timetoltxcancer_bldgrpa_se <- model_timetoltxcancer$res['bloodgroupA', "se"] %>% as.numeric()
coef_timetoltxcancer_bldgrpb_se <- model_timetoltxcancer$res['bloodgroupB', "se"] %>% as.numeric()
coef_timetoltxcancer_bldgrpab_se <- model_timetoltxcancer$res['bloodgroupAB', "se"] %>% as.numeric()
coef_timetoltxcancer_female_se <- model_timetoltxcancer$res['female', "se"] %>% as.numeric()
coef_timetoltxcancer_agetx_se <- model_timetoltxcancer$res['ageattransplant', "se"] %>% as.numeric()
coef_timetoltxcancer_comorbs_se <- model_timetoltxcancer$res['comorbs', "se"] %>% as.numeric()
coef_timetoltxcancer_prevtxcount_se <- model_timetoltxcancer$res['prevtxcount', "se"] %>% as.numeric()


# 
# # Time to living donor transplant failure (without cancer) ----
# ## Loop through all in-built distributions available for flexsurvreg()
# dists <- flexsurv.dists %>% as_tibble() %>% colnames()
# 
# for(dist in dists) {
# 
#   tryCatch({
#   i <- grep(paste0('^',dist,'$'),dists)
# 
#   if (i==1) {
#     curr_t <- as.numeric(Sys.time())
#     prev_t <- curr_t
#     t <- 0
#   }
#   if (i>1) {
#     prev_t <- curr_t
#     curr_t <- as.numeric(Sys.time())
#     t <- round(curr_t - prev_t,0)
#   }
# 
#   progress <- paste0('Model ',
#                      i,
#                      ' of ',
#                      length(dists),
#                      ' (',
#                      dist,
#                      '): ',
#                      round(t,0),
#                      's')
#   print(progress)
# 
#   model <- flexsurvreg(data=cohort2 %>% filter(donortype == "Living"),
#                        dist=dist,
#                        formula= Surv(starttime,endtime_txfail,txfail) ~
#                          donorage + donorfemale + ageattransplant + female +
#                          bloodgroup + comorbs + prevtxcount)
# 
#   distname <- sub('\\.','',dist)
# 
#   saveRDS(model,file.path(modelsloc,paste0('timetoltxfail_',distname,'.RDS')))
# 
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }
# 
# 
# ## Spline model
# model <- flexsurvspline(data=cohort2 %>% filter(donortype == "Living"),
#                         k=5, # Tried a few different numbers, as well as manually specifying locations. 5 seems optimal
#                         formula= Surv(starttime,endtime_txfail,txfail) ~
#                           donorage + donorfemale + ageattransplant + female +
#                           bloodgroup + comorbs + prevtxcount)
# saveRDS(model,file.path(modelsloc,'timetoltxfail_spline.RDS'))
# 
# 
# ## Read all models into R, produce plots, and print AIC values
# for (dist in c(dists,'spline')) {
#   tryCatch({
#   assign(paste0('timetoltxfail_',dist),
#          readRDS(paste0(modelsloc,'timetoltxfail_',dist,'.RDS')))
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }


## Visually assess goodness-of-fit
# plot(timetoltxfail_genf, xlim=c(0,365.25*25), main='dist = genf')
# plot(timetoltxfail_genf.orig, xlim=c(0,365.25*25), main='dist = genf.orig')
# plot(timetoltxfail_gengamma, xlim=c(0,365.25*25), main='dist = gengamma')
# plot(timetoltxfail_gengamma.orig, xlim=c(0,365.25*25), main='dist = gengamma.orig')
# plot(timetoltxfail_exp, xlim=c(0,365.25*25), main='dist = exp')
# plot(timetoltxfail_weibull, xlim=c(0,365.25*25), main='dist = weibull')
# plot(timetoltxfail_weibullPH, xlim=c(0,365.25*25), main='dist = weibullPH')
# plot(timetoltxfail_lnorm, xlim=c(0,365.25*25), main='dist = lnorm')
# plot(timetoltxfail_gamma, xlim=c(0,365.25*25), main='dist = gamma')
# plot(timetoltxfail_gompertz, xlim=c(0,365.25*25), main='dist = gompertz')
# plot(timetoltxfail_llogis, xlim=c(0,365.25*25), main='dist = llogis')
# plot(timetoltxfail_exponential, xlim=c(0,365.25*25), main='dist = exponential')
# plot(timetoltxfail_lognormal, xlim=c(0,365.25*25), main='dist = lognormal')
# plot(timetoltxfail_spline, xlim=c(0,365.25*25), main='dist = spline')


## Assess goodness-of-fit with AIC
# timetoltxfail_genf$AIC
# timetoltxfail_genf.orig$AIC
# timetoltxfail_gengamma$AIC
# timetoltxfail_gengamma.orig$AIC
# timetoltxfail_exp$AIC
# timetoltxfail_weibull$AIC
# timetoltxfail_weibullPH$AIC
# timetoltxfail_lnorm$AIC
# timetoltxfail_gamma$AIC
# timetoltxfail_gompertz$AIC
# timetoltxfail_llogis$AIC
# timetoltxfail_exponential$AIC
# timetoltxfail_lognormal$AIC
# timetoltxfail_spline$AIC


## Use spline - lowest AIC and easily the best visual fit
model_timetoltxfail <- readRDS(paste0(modelsloc,'timetoltxfail_','spline','.RDS'))

timetoltxfail_splinegamma0_mean <- model_timetoltxfail$coefficients['gamma0'] %>% as.numeric()
timetoltxfail_splinegamma1_mean <- model_timetoltxfail$coefficients['gamma1'] %>% as.numeric()
timetoltxfail_splinegamma2_mean <- model_timetoltxfail$coefficients['gamma2'] %>% as.numeric()
timetoltxfail_splinegamma3_mean <- model_timetoltxfail$coefficients['gamma3'] %>% as.numeric()
timetoltxfail_splinegamma4_mean <- model_timetoltxfail$coefficients['gamma4'] %>% as.numeric()
timetoltxfail_splinegamma5_mean <- model_timetoltxfail$coefficients['gamma5'] %>% as.numeric()
timetoltxfail_splinegamma6_mean <- model_timetoltxfail$coefficients['gamma6'] %>% as.numeric()
timetoltxfail_splinegamma_mean <- c(timetoltxfail_splinegamma0_mean,
                                   timetoltxfail_splinegamma1_mean,
                                   timetoltxfail_splinegamma2_mean,
                                   timetoltxfail_splinegamma3_mean,
                                   timetoltxfail_splinegamma4_mean,
                                   timetoltxfail_splinegamma5_mean,
                                   timetoltxfail_splinegamma6_mean)
timetoltxfail_splineknots <- model_timetoltxfail$knots %>% as.numeric()
coef_timetoltxfail_donorage_mean <- model_timetoltxfail$coefficients['donorage'] %>% as.numeric()
coef_timetoltxfail_donorfemale_mean <- model_timetoltxfail$coefficients['donorfemale'] %>% as.numeric()
coef_timetoltxfail_bldgrpa_mean <- model_timetoltxfail$coefficients['bloodgroupA'] %>% as.numeric()
coef_timetoltxfail_bldgrpb_mean <- model_timetoltxfail$coefficients['bloodgroupB'] %>% as.numeric()
coef_timetoltxfail_bldgrpab_mean <- model_timetoltxfail$coefficients['bloodgroupAB'] %>% as.numeric()
coef_timetoltxfail_female_mean <- model_timetoltxfail$coefficients['female'] %>% as.numeric()
coef_timetoltxfail_agetx_mean <- model_timetoltxfail$coefficients['ageattransplant'] %>% as.numeric()
coef_timetoltxfail_comorbs_mean <- model_timetoltxfail$coefficients['comorbs'] %>% as.numeric()
coef_timetoltxfail_prevtxcount_mean <- model_timetoltxfail$coefficients['prevtxcount'] %>% as.numeric()

timetoltxfail_splinegamma0_se <- model_timetoltxfail$res['gamma0', "se"] %>% as.numeric()
timetoltxfail_splinegamma1_se <- model_timetoltxfail$res['gamma1', "se"] %>% as.numeric()
timetoltxfail_splinegamma2_se <- model_timetoltxfail$res['gamma2', "se"] %>% as.numeric()
timetoltxfail_splinegamma3_se <- model_timetoltxfail$res['gamma3', "se"] %>% as.numeric()
timetoltxfail_splinegamma4_se <- model_timetoltxfail$res['gamma4', "se"] %>% as.numeric()
timetoltxfail_splinegamma5_se <- model_timetoltxfail$res['gamma5', "se"] %>% as.numeric()
timetoltxfail_splinegamma6_se <- model_timetoltxfail$res['gamma6', "se"] %>% as.numeric()
timetoltxfail_splinegamma_se <- c(timetoltxfail_splinegamma0_se,
                                 timetoltxfail_splinegamma1_se,
                                 timetoltxfail_splinegamma2_se,
                                 timetoltxfail_splinegamma3_se,
                                 timetoltxfail_splinegamma4_se,
                                 timetoltxfail_splinegamma5_se,
                                 timetoltxfail_splinegamma6_se)
coef_timetoltxfail_donorage_se <- model_timetoltxfail$res['donorage', "se"] %>% as.numeric()
coef_timetoltxfail_donorfemale_se <- model_timetoltxfail$res['donorfemale', "se"] %>% as.numeric()
coef_timetoltxfail_bldgrpa_se <- model_timetoltxfail$res['bloodgroupA', "se"] %>% as.numeric()
coef_timetoltxfail_bldgrpb_se <- model_timetoltxfail$res['bloodgroupB', "se"] %>% as.numeric()
coef_timetoltxfail_bldgrpab_se <- model_timetoltxfail$res['bloodgroupAB', "se"] %>% as.numeric()
coef_timetoltxfail_female_se <- model_timetoltxfail$res['female', "se"] %>% as.numeric()
coef_timetoltxfail_agetx_se <- model_timetoltxfail$res['ageattransplant', "se"] %>% as.numeric()
coef_timetoltxfail_comorbs_se <- model_timetoltxfail$res['comorbs', "se"] %>% as.numeric()
coef_timetoltxfail_prevtxcount_se <- model_timetoltxfail$res['prevtxcount', "se"] %>% as.numeric()



# Time to living donor transplant failure (with cancer) ----
# ## Loop through all in-built distributions available for flexsurvreg()
# dists <- flexsurv.dists %>% as_tibble() %>% colnames()
# 
# for(dist in dists) {
# 
#   tryCatch({
#   i <- grep(paste0('^',dist,'$'),dists)
# 
#   if (i==1) {
#     curr_t <- as.numeric(Sys.time())
#     prev_t <- curr_t
#     t <- 0
#   }
#   if (i>1) {
#     prev_t <- curr_t
#     curr_t <- as.numeric(Sys.time())
#     t <- round(curr_t - prev_t,0)
#   }
# 
#   progress <- paste0('Model ',
#                      i,
#                      ' of ',
#                      length(dists),
#                      ' (',
#                      dist,
#                      '): ',
#                      round(t,0),
#                      's')
#   print(progress)
# 
#   model <- flexsurvreg(data=cohort2 %>% filter(donortype == "Living"),
#                        dist=dist,
#                        formula= Surv(starttime,endtime_txcfail,txcfail) ~
#                          donorage + donorfemale + ageattransplant + female +
#                          bloodgroup + comorbs + prevtxcount + ageatcancer)
# 
#   distname <- sub('\\.','',dist)
# 
#   saveRDS(model,file.path(modelsloc,paste0('timetoltxcfail_',distname,'.RDS')))
# 
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }
# 
# 
# ## Spline model
# model <- flexsurvspline(data=cohort2 %>% filter(donortype == "Living"),
#                         k=5, # Tried a few different numbers, as well as manually specifying locations. 5 seems optimal
#                         formula= Surv(starttime,endtime_txcfail,txcfail) ~
#                           donorage + donorfemale + ageattransplant + female +
#                           bloodgroup + comorbs + prevtxcount + ageatcancer)
# saveRDS(model,file.path(modelsloc,'timetoltxcfail_spline.RDS'))
# 
# 
# ## Read all models into R, produce plots, and print AIC values
# for (dist in c(dists,'spline')) {
#   tryCatch({
#   assign(paste0('timetoltxcfail_',dist),
#          readRDS(paste0(modelsloc,'timetoltxcfail_',dist,'.RDS')))
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }


## Visually assess goodness-of-fit
# plot(timetoltxcfail_genf, xlim=c(0,365.25*25), main='dist = genf')
# plot(timetoltxcfail_genf.orig, xlim=c(0,365.25*25), main='dist = genf.orig')
# plot(timetoltxcfail_gengamma, xlim=c(0,365.25*25), main='dist = gengamma')
# plot(timetoltxcfail_gengamma.orig, xlim=c(0,365.25*25), main='dist = gengamma.orig')
# plot(timetoltxcfail_exp, xlim=c(0,365.25*25), main='dist = exp')
# plot(timetoltxcfail_weibull, xlim=c(0,365.25*25), main='dist = weibull')
# plot(timetoltxcfail_weibullPH, xlim=c(0,365.25*25), main='dist = weibullPH')
# plot(timetoltxcfail_lnorm, xlim=c(0,365.25*25), main='dist = lnorm')
# plot(timetoltxcfail_gamma, xlim=c(0,365.25*25), main='dist = gamma')
# plot(timetoltxcfail_gompertz, xlim=c(0,365.25*25), main='dist = gompertz')
# plot(timetoltxcfail_llogis, xlim=c(0,365.25*25), main='dist = llogis')
# plot(timetoltxcfail_exponential, xlim=c(0,365.25*25), main='dist = exponential')
# plot(timetoltxcfail_lognormal, xlim=c(0,365.25*25), main='dist = lognormal')
# plot(timetoltxcfail_spline, xlim=c(0,365.25*25), main='dist = spline')


## Assess goodness-of-fit with AIC
# timetoltxcfail_genf$AIC
# timetoltxcfail_genf.orig$AIC
# timetoltxcfail_gengamma$AIC
# timetoltxcfail_gengamma.orig$AIC
# timetoltxcfail_exp$AIC
# timetoltxcfail_weibull$AIC
# timetoltxcfail_weibullPH$AIC
# timetoltxcfail_lnorm$AIC
# timetoltxcfail_gamma$AIC
# timetoltxcfail_gompertz$AIC
# timetoltxcfail_llogis$AIC
# timetoltxcfail_exponential$AIC
# timetoltxcfail_lognormal$AIC
# timetoltxcfail_spline$AIC


## Use generalised gamma - simpler than spline model, second lowest AIC and best visual fit 
## Note: no standard errors available because no observations were censored 
model_timetoltxcfail <- readRDS(paste0(modelsloc,'timetoltxcfail_','gengamma','.RDS'))

timetoltxcfail_mu_mean <- model_timetoltxcfail$coefficients['mu'] %>% as.numeric()
timetoltxcfail_sigma_mean <- model_timetoltxcfail$coefficients['sigma'] %>% exp() %>% as.numeric()
timetoltxcfail_Q_mean <- model_timetoltxcfail$coefficients['Q'] %>% as.numeric()
coef_timetoltxcfail_donorage_mean <- model_timetoltxcfail$coefficients['donorage'] %>% as.numeric()
coef_timetoltxcfail_donorfemale_mean <- model_timetoltxcfail$coefficients['donorfemale'] %>% as.numeric()
coef_timetoltxcfail_bldgrpa_mean <- model_timetoltxcfail$coefficients['bloodgroupA'] %>% as.numeric()
coef_timetoltxcfail_bldgrpb_mean <- model_timetoltxcfail$coefficients['bloodgroupB'] %>% as.numeric()
coef_timetoltxcfail_bldgrpab_mean <- model_timetoltxcfail$coefficients['bloodgroupAB'] %>% as.numeric()
coef_timetoltxcfail_female_mean <- model_timetoltxcfail$coefficients['female'] %>% as.numeric()
coef_timetoltxcfail_agetx_mean <- model_timetoltxcfail$coefficients['ageattransplant'] %>% as.numeric()
coef_timetoltxcfail_comorbs_mean <- model_timetoltxcfail$coefficients['comorbs'] %>% as.numeric()
coef_timetoltxcfail_prevtxcount_mean <- model_timetoltxcfail$coefficients['prevtxcount'] %>% as.numeric()
coef_timetoltxcfail_agetxc_mean <- model_timetoltxcfail$coefficients['ageatcancer'] %>% as.numeric()

timetoltxcfail_mu_se <- model_timetoltxcfail$res['mu', "se"] %>% as.numeric()
timetoltxcfail_sigma_se <- model_timetoltxcfail$res['sigma', "se"] %>% as.numeric()
timetoltxcfail_Q_se <- model_timetoltxcfail$res['Q', "se"] %>% as.numeric()
coef_timetoltxcfail_donorage_se <- model_timetoltxcfail$res['donorage', "se"] %>% as.numeric()
coef_timetoltxcfail_donorfemale_se <- model_timetoltxcfail$res['donorfemale', "se"] %>% as.numeric()
coef_timetoltxcfail_bldgrpa_se <- model_timetoltxcfail$res['bloodgroupA', "se"] %>% as.numeric()
coef_timetoltxcfail_bldgrpb_se <- model_timetoltxcfail$res['bloodgroupB', "se"] %>% as.numeric()
coef_timetoltxcfail_bldgrpab_se <- model_timetoltxcfail$res['bloodgroupAB', "se"] %>% as.numeric()
coef_timetoltxcfail_female_se <- model_timetoltxcfail$res['female', "se"] %>% as.numeric()
coef_timetoltxcfail_agetx_se <- model_timetoltxcfail$res['ageattransplant', "se"] %>% as.numeric()
coef_timetoltxcfail_comorbs_se <- model_timetoltxcfail$res['comorbs', "se"] %>% as.numeric()
coef_timetoltxcfail_prevtxcount_se <- model_timetoltxcfail$res['prevtxcount', "se"] %>% as.numeric()
coef_timetoltxcfail_agetxc_se <- model_timetoltxcfail$res['ageatcancer', "se"] %>% as.numeric()


# Time to deceased donor transplant failure (without cancer) ----
# # Loop through all in-built distributions available for flexsurvreg()
# dists <- flexsurv.dists %>% as_tibble() %>% colnames()
# 
# for(dist in dists) {
# 
#   tryCatch({
#   i <- grep(paste0('^',dist,'$'),dists)
# 
#   if (i==1) {
#     curr_t <- as.numeric(Sys.time())
#     prev_t <- curr_t
#     t <- 0
#   }
#   if (i>1) {
#     prev_t <- curr_t
#     curr_t <- as.numeric(Sys.time())
#     t <- round(curr_t - prev_t,0)
#   }
# 
#   progress <- paste0('Model ',
#                      i,
#                      ' of ',
#                      length(dists),
#                      ' (',
#                      dist,
#                      '): ',
#                      round(t,0),
#                      's')
#   print(progress)
# 
#   model <- flexsurvreg(data=cohort2 %>% filter(donortype != "Living"),
#                        dist=dist,
#                        formula= Surv(starttime,endtime_txfail,txfail) ~
#                          donortype + donorage + donorfemale + donorkdpi + ageattransplant + female +
#                          bloodgroup + comorbs + prevtxcount)
# 
#   distname <- sub('\\.','',dist)
# 
#   saveRDS(model,file.path(modelsloc,paste0('timetotxfail_',distname,'.RDS')))
# 
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }
# 
# 
# ## Spline model
# model <- flexsurvspline(data=cohort2 %>% filter(donortype != "Living"),
#                         k=5, # Tried a few different numbers, as well as manually specifying locations. 5 seems optimal
#                         formula= Surv(starttime,endtime_txfail,txfail) ~
#                           donortype + donorage + donorfemale + donorkdpi + ageattransplant + female +
#                           bloodgroup + comorbs + prevtxcount)
# saveRDS(model,file.path(modelsloc,'timetotxfail_spline.RDS'))
# 
# 
# ## Read all models into R, produce plots, and print AIC values
# for (dist in c(dists,'spline')) {
#   tryCatch({
#   assign(paste0('timetotxfail_',dist),
#          readRDS(paste0(modelsloc,'timetotxfail_',dist,'.RDS')))
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }


## Visually assess goodness-of-fit
# plot(timetotxfail_genf, xlim=c(0,365.25*25), main='dist = genf')
# plot(timetotxfail_genf.orig, xlim=c(0,365.25*25), main='dist = genf.orig')
# plot(timetotxfail_gengamma, xlim=c(0,365.25*25), main='dist = gengamma')
# plot(timetotxfail_gengamma.orig, xlim=c(0,365.25*25), main='dist = gengamma.orig')
# plot(timetotxfail_exp, xlim=c(0,365.25*25), main='dist = exp')
# plot(timetotxfail_weibull, xlim=c(0,365.25*25), main='dist = weibull')
# plot(timetotxfail_weibullPH, xlim=c(0,365.25*25), main='dist = weibullPH')
# plot(timetotxfail_lnorm, xlim=c(0,365.25*25), main='dist = lnorm')
# plot(timetotxfail_gamma, xlim=c(0,365.25*25), main='dist = gamma')
# plot(timetotxfail_gompertz, xlim=c(0,365.25*25), main='dist = gompertz')
# plot(timetotxfail_llogis, xlim=c(0,365.25*25), main='dist = llogis')
# plot(timetotxfail_exponential, xlim=c(0,365.25*25), main='dist = exponential')
# plot(timetotxfail_lognormal, xlim=c(0,365.25*25), main='dist = lognormal')
# plot(timetotxfail_spline, xlim=c(0,365.25*25), main='dist = spline')


## Assess goodness-of-fit with AIC
# timetotxfail_genf$AIC
# timetotxfail_genf.orig$AIC
# timetotxfail_gengamma$AIC
# timetotxfail_gengamma.orig$AIC
# timetotxfail_exp$AIC
# timetotxfail_weibull$AIC
# timetotxfail_weibullPH$AIC
# timetotxfail_lnorm$AIC
# timetotxfail_gamma$AIC
# timetotxfail_gompertz$AIC
# timetotxfail_llogis$AIC
# timetotxfail_exponential$AIC
# timetotxfail_lognormal$AIC
# timetotxfail_spline$AIC


## Use spline - lowest AIC and easily the best visual fit
model_timetotxfail <- readRDS(paste0(modelsloc,'timetotxfail_','spline','.RDS'))

timetotxfail_splinegamma0_mean <- model_timetotxfail$coefficients['gamma0'] %>% as.numeric()
timetotxfail_splinegamma1_mean <- model_timetotxfail$coefficients['gamma1'] %>% as.numeric()
timetotxfail_splinegamma2_mean <- model_timetotxfail$coefficients['gamma2'] %>% as.numeric()
timetotxfail_splinegamma3_mean <- model_timetotxfail$coefficients['gamma3'] %>% as.numeric()
timetotxfail_splinegamma4_mean <- model_timetotxfail$coefficients['gamma4'] %>% as.numeric()
timetotxfail_splinegamma5_mean <- model_timetotxfail$coefficients['gamma5'] %>% as.numeric()
timetotxfail_splinegamma6_mean <- model_timetotxfail$coefficients['gamma6'] %>% as.numeric()
timetotxfail_splinegamma_mean <- c(timetotxfail_splinegamma0_mean,
                                   timetotxfail_splinegamma1_mean,
                                   timetotxfail_splinegamma2_mean,
                                   timetotxfail_splinegamma3_mean,
                                   timetotxfail_splinegamma4_mean,
                                   timetotxfail_splinegamma5_mean,
                                   timetotxfail_splinegamma6_mean)
timetotxfail_splineknots <- model_timetotxfail$knots %>% as.numeric()
coef_timetotxfail_donortypedbdscd_mean <- model_timetotxfail$coefficients['donortypeDBDSCD'] %>% as.numeric()
coef_timetotxfail_donortypedbdecd_mean <- model_timetotxfail$coefficients['donortypeDBDECD'] %>% as.numeric()
coef_timetotxfail_donorage_mean <- model_timetotxfail$coefficients['donorage'] %>% as.numeric()
coef_timetotxfail_donorfemale_mean <- model_timetotxfail$coefficients['donorfemale'] %>% as.numeric()
coef_timetotxfail_donorkdpi_mean <- model_timetotxfail$coefficients['donorkdpi'] %>% as.numeric()
coef_timetotxfail_bldgrpa_mean <- model_timetotxfail$coefficients['bloodgroupA'] %>% as.numeric()
coef_timetotxfail_bldgrpb_mean <- model_timetotxfail$coefficients['bloodgroupB'] %>% as.numeric()
coef_timetotxfail_bldgrpab_mean <- model_timetotxfail$coefficients['bloodgroupAB'] %>% as.numeric()
coef_timetotxfail_female_mean <- model_timetotxfail$coefficients['female'] %>% as.numeric()
coef_timetotxfail_agetx_mean <- model_timetotxfail$coefficients['ageattransplant'] %>% as.numeric()
coef_timetotxfail_comorbs_mean <- model_timetotxfail$coefficients['comorbs'] %>% as.numeric()
coef_timetotxfail_prevtxcount_mean <- model_timetotxfail$coefficients['prevtxcount'] %>% as.numeric()

timetotxfail_splinegamma0_se <- model_timetotxfail$res['gamma0', "se"] %>% as.numeric()
timetotxfail_splinegamma1_se <- model_timetotxfail$res['gamma1', "se"] %>% as.numeric()
timetotxfail_splinegamma2_se <- model_timetotxfail$res['gamma2', "se"] %>% as.numeric()
timetotxfail_splinegamma3_se <- model_timetotxfail$res['gamma3', "se"] %>% as.numeric()
timetotxfail_splinegamma4_se <- model_timetotxfail$res['gamma4', "se"] %>% as.numeric()
timetotxfail_splinegamma5_se <- model_timetotxfail$res['gamma5', "se"] %>% as.numeric()
timetotxfail_splinegamma6_se <- model_timetotxfail$res['gamma6', "se"] %>% as.numeric()
timetotxfail_splinegamma_se <- c(timetotxfail_splinegamma0_se,
                                 timetotxfail_splinegamma1_se,
                                 timetotxfail_splinegamma2_se,
                                 timetotxfail_splinegamma3_se,
                                 timetotxfail_splinegamma4_se,
                                 timetotxfail_splinegamma5_se,
                                 timetotxfail_splinegamma6_se)
coef_timetotxfail_donortypedbdscd_se <- model_timetotxfail$res['donortypeDBDSCD', "se"] %>% as.numeric()
coef_timetotxfail_donortypedbdecd_se <- model_timetotxfail$res['donortypeDBDECD', "se"] %>% as.numeric()
coef_timetotxfail_donorage_se <- model_timetotxfail$res['donorage', "se"] %>% as.numeric()
coef_timetotxfail_donorfemale_se <- model_timetotxfail$res['donorfemale', "se"] %>% as.numeric()
coef_timetotxfail_donorkdpi_se <- model_timetotxfail$res['donorkdpi', "se"] %>% as.numeric()
coef_timetotxfail_bldgrpa_se <- model_timetotxfail$res['bloodgroupA', "se"] %>% as.numeric()
coef_timetotxfail_bldgrpb_se <- model_timetotxfail$res['bloodgroupB', "se"] %>% as.numeric()
coef_timetotxfail_bldgrpab_se <- model_timetotxfail$res['bloodgroupAB', "se"] %>% as.numeric()
coef_timetotxfail_female_se <- model_timetotxfail$res['female', "se"] %>% as.numeric()
coef_timetotxfail_agetx_se <- model_timetotxfail$res['ageattransplant', "se"] %>% as.numeric()
coef_timetotxfail_comorbs_se <- model_timetotxfail$res['comorbs', "se"] %>% as.numeric()
coef_timetotxfail_prevtxcount_se <- model_timetotxfail$res['prevtxcount', "se"] %>% as.numeric()


# Time to deceased donor transplant failure (after cancer diagnosis) ----
## Loop through all in-built distributions available for flexsurvreg()
# dists <- flexsurv.dists %>% as_tibble() %>% colnames()
# 
# for(dist in dists) {
# 
#   tryCatch({
#   i <- grep(paste0('^',dist,'$'),dists)
# 
#   if (i==1) {
#     curr_t <- as.numeric(Sys.time())
#     prev_t <- curr_t
#     t <- 0
#   }
#   if (i>1) {
#     prev_t <- curr_t
#     curr_t <- as.numeric(Sys.time())
#     t <- round(curr_t - prev_t,0)
#   }
# 
#   progress <- paste0('Model ',
#                      i,
#                      ' of ',
#                      length(dists),
#                      ' (',
#                      dist,
#                      '): ',
#                      round(t,0),
#                      's')
#   print(progress)
# 
#   model <- flexsurvreg(data=cohort2,
#                        dist=dist,
#                        formula= Surv(starttime,endtime_txcfail,txcfail) ~
#                          donortype + donorage + donorfemale + donorkdpi + ageattransplant + female +
#                          bloodgroup + comorbs + prevtxcount + ageatcancer)
# 
#   distname <- sub('\\.','',dist)
# 
#   saveRDS(model,file.path(modelsloc,paste0('timetotxcfail_',distname,'.RDS')))
# 
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }
# 
# 
# ## Spline model
# model <- flexsurvspline(data=cohort2,
#                         k=5, # Tried a few different numbers, as well as manually specifying locations. 5 seems optimal
#                         formula= Surv(starttime,endtime_txcfail,txcfail) ~
#                           donortype + donorage + donorfemale + donorkdpi + ageattransplant + female +
#                           bloodgroup + comorbs + prevtxcount + ageatcancer)
# saveRDS(model,file.path(modelsloc,'timetotxcfail_spline.RDS'))
# 
# 
# ## Read all models into R, produce plots, and print AIC values
# for (dist in c(dists,'spline')) {
#   tryCatch({
#   assign(paste0('timetotxcfail_',dist),
#          readRDS(paste0(modelsloc,'timetotxcfail_',dist,'.RDS')))
#   }, error=function(e){cat(conditionMessage(e),'\n')}) # Close tryCatch error handling
# }


## Visually assess goodness-of-fit
# plot(timetotxcfail_genf, xlim=c(0,365.25*25), main='dist = genf')
# plot(timetotxcfail_genf.orig, xlim=c(0,365.25*25), main='dist = genf.orig')
# plot(timetotxcfail_gengamma, xlim=c(0,365.25*25), main='dist = gengamma')
# plot(timetotxcfail_gengamma.orig, xlim=c(0,365.25*25), main='dist = gengamma.orig')
# plot(timetotxcfail_exp, xlim=c(0,365.25*25), main='dist = exp')
# plot(timetotxcfail_weibull, xlim=c(0,365.25*25), main='dist = weibull')
# plot(timetotxcfail_weibullPH, xlim=c(0,365.25*25), main='dist = weibullPH')
# plot(timetotxcfail_lnorm, xlim=c(0,365.25*25), main='dist = lnorm')
# plot(timetotxcfail_gamma, xlim=c(0,365.25*25), main='dist = gamma')
# plot(timetotxcfail_gompertz, xlim=c(0,365.25*25), main='dist = gompertz')
# plot(timetotxcfail_llogis, xlim=c(0,365.25*25), main='dist = llogis')
# plot(timetotxcfail_exponential, xlim=c(0,365.25*25), main='dist = exponential')
# plot(timetotxcfail_lognormal, xlim=c(0,365.25*25), main='dist = lognormal')
# plot(timetotxcfail_spline, xlim=c(0,365.25*25), main='dist = spline')


## Assess goodness-of-fit with AIC
# timetotxcfail_genf$AIC
# timetotxcfail_genf.orig$AIC
# timetotxcfail_gengamma$AIC
# timetotxcfail_gengamma.orig$AIC
# timetotxcfail_exp$AIC
# timetotxcfail_weibull$AIC
# timetotxcfail_weibullPH$AIC
# timetotxcfail_lnorm$AIC
# timetotxcfail_gamma$AIC
# timetotxcfail_gompertz$AIC
# timetotxcfail_llogis$AIC
# timetotxcfail_exponential$AIC
# timetotxcfail_lognormal$AIC
# timetotxcfail_spline$AIC


## Use generalised Gamma - lowest AIC and easily the best visual fit
model_timetotxcfail <- readRDS(paste0(modelsloc,'timetotxcfail_','gengamma','.RDS'))

timetotxcfail_mu_mean <- model_timetotxcfail$coefficients['mu'] %>% as.numeric()
timetotxcfail_sigma_mean <- model_timetotxcfail$coefficients['sigma'] %>% exp() %>% as.numeric()
timetotxcfail_Q_mean <- model_timetotxcfail$coefficients['Q'] %>% as.numeric()
coef_timetotxcfail_donortypedbdscd_mean <- model_timetotxcfail$coefficients['donortypeDBDSCD'] %>% as.numeric()
coef_timetotxcfail_donortypedbdecd_mean <- model_timetotxcfail$coefficients['donortypeDBDECD'] %>% as.numeric()
coef_timetotxcfail_donorage_mean <- model_timetotxcfail$coefficients['donorage'] %>% as.numeric()
coef_timetotxcfail_donorfemale_mean <- model_timetotxcfail$coefficients['donorfemale'] %>% as.numeric()
coef_timetotxcfail_donorkdpi_mean <- model_timetotxcfail$coefficients['donorkdpi'] %>% as.numeric()
coef_timetotxcfail_bldgrpa_mean <- model_timetotxcfail$coefficients['bloodgroupA'] %>% as.numeric()
coef_timetotxcfail_bldgrpb_mean <- model_timetotxcfail$coefficients['bloodgroupB'] %>% as.numeric()
coef_timetotxcfail_bldgrpab_mean <- model_timetotxcfail$coefficients['bloodgroupAB'] %>% as.numeric()
coef_timetotxcfail_female_mean <- model_timetotxcfail$coefficients['female'] %>% as.numeric()
coef_timetotxcfail_agetx_mean <- model_timetotxcfail$coefficients['ageattransplant'] %>% as.numeric()
coef_timetotxcfail_comorbs_mean <- model_timetotxcfail$coefficients['comorbs'] %>% as.numeric()
coef_timetotxcfail_prevtxcount_mean <- model_timetotxcfail$coefficients['prevtxcount'] %>% as.numeric()
coef_timetotxcfail_agetxc_mean <- model_timetotxcfail$coefficients['ageatcancer'] %>% as.numeric()

timetotxcfail_mu_se <- model_timetotxcfail$res['mu', "se"] %>% as.numeric()
timetotxcfail_sigma_se <- model_timetotxcfail$res['sigma', "se"] %>% as.numeric()
timetotxcfail_Q_se <- model_timetotxcfail$res['Q', "se"] %>% as.numeric()
coef_timetotxcfail_donortypedbdscd_se <- model_timetotxcfail$res['donortypeDBDSCD', "se"] %>% as.numeric()
coef_timetotxcfail_donortypedbdecd_se <- model_timetotxcfail$res['donortypeDBDECD', "se"] %>% as.numeric()
coef_timetotxcfail_donorage_se <- model_timetotxcfail$res['donorage', "se"] %>% as.numeric()
coef_timetotxcfail_donorfemale_se <- model_timetotxcfail$res['donorfemale', "se"] %>% as.numeric()
coef_timetotxcfail_donorkdpi_se <- model_timetotxcfail$res['donorkdpi', "se"] %>% as.numeric()
coef_timetotxcfail_bldgrpa_se <- model_timetotxcfail$res['bloodgroupA', "se"] %>% as.numeric()
coef_timetotxcfail_bldgrpb_se <- model_timetotxcfail$res['bloodgroupB', "se"] %>% as.numeric()
coef_timetotxcfail_bldgrpab_se <- model_timetotxcfail$res['bloodgroupAB', "se"] %>% as.numeric()
coef_timetotxcfail_female_se <- model_timetotxcfail$res['female', "se"] %>% as.numeric()
coef_timetotxcfail_agetx_se <- model_timetotxcfail$res['ageattransplant', "se"] %>% as.numeric()
coef_timetotxcfail_comorbs_se <- model_timetotxcfail$res['comorbs', "se"] %>% as.numeric()
coef_timetotxcfail_prevtxcount_se <- model_timetotxcfail$res['prevtxcount', "se"] %>% as.numeric()
coef_timetotxcfail_agetxc_se <- model_timetotxcfail$res['ageatcancer', "se"] %>% as.numeric()




# Save statistical results for use as model inputs ----
save(
  file=file.path(inputsloc,'statistical_inputs.RData'),
  #
  coef_prevtxcount_int_mean,
  coef_prevtxcount_int_se,
  #
  coef_bldgrp_bldgrpa_int_mean,
  coef_bldgrp_bldgrpa_prevtxcount_mean,
  #
  coef_bldgrp_bldgrpa_int_se,
  coef_bldgrp_bldgrpa_prevtxcount_se,
  #
  coef_bldgrp_bldgrpb_int_mean,
  coef_bldgrp_bldgrpb_prevtxcount_mean,
  #
  coef_bldgrp_bldgrpb_int_se,
  coef_bldgrp_bldgrpb_prevtxcount_se,
  #
  coef_bldgrp_bldgrpab_int_mean,
  coef_bldgrp_bldgrpab_prevtxcount_mean,
  #
  coef_bldgrp_bldgrpab_int_se,
  coef_bldgrp_bldgrpab_prevtxcount_se,
  #
  coef_female_int_mean,
  coef_female_bldgrpa_mean,
  coef_female_bldgrpb_mean,
  coef_female_bldgrpab_mean,
  coef_female_prevtxcount_mean,
  #
  coef_female_int_se,
  coef_female_bldgrpa_se,
  coef_female_bldgrpb_se,
  coef_female_bldgrpab_se,
  coef_female_prevtxcount_se,
  #
  coef_agewtlst_int_mean,
  coef_agewtlst_bldgrpa_mean,
  coef_agewtlst_bldgrpb_mean,
  coef_agewtlst_bldgrpab_mean,
  coef_agewtlst_female_mean,
  coef_agewtlst_prevtxcount_mean,
  rmse_agewtlst,
  #
  coef_agewtlst_int_se,
  coef_agewtlst_bldgrpa_se,
  coef_agewtlst_bldgrpb_se,
  coef_agewtlst_bldgrpab_se,
  coef_agewtlst_female_se,
  coef_agewtlst_prevtxcount_se,
  #
  coef_comorbs_int_mean,
  coef_comorbs_bldgrpa_mean,
  coef_comorbs_bldgrpb_mean,
  coef_comorbs_bldgrpab_mean,
  coef_comorbs_female_mean,
  coef_comorbs_prevtxcount_mean,
  coef_comorbs_agewtlst_mean,
  #
  coef_comorbs_int_se,
  coef_comorbs_bldgrpa_se,
  coef_comorbs_bldgrpb_se,
  coef_comorbs_bldgrpab_se,
  coef_comorbs_female_se,
  coef_comorbs_prevtxcount_se,
  coef_comorbs_agewtlst_se,
  #
  epts_table,
  #
  timetodtx_shape_mean,
  timetodtx_scale_mean,
  coef_timetodtx_bldgrpa_mean,
  coef_timetodtx_bldgrpb_mean,
  coef_timetodtx_bldgrpab_mean,
  coef_timetodtx_female_mean,
  coef_timetodtx_agewtlst_mean,
  coef_timetodtx_comorbs_mean,
  coef_timetodtx_prevtxcount_mean,
  #
  timetodtx_shape_se,
  timetodtx_scale_se,
  coef_timetodtx_bldgrpa_se,
  coef_timetodtx_bldgrpb_se,
  coef_timetodtx_bldgrpab_se,
  coef_timetodtx_female_se,
  coef_timetodtx_agewtlst_se,
  coef_timetodtx_comorbs_se,
  coef_timetodtx_prevtxcount_se,
  #
  timetoltx_mu_mean,
  timetoltx_sigma_mean,
  timetoltx_Q_mean,
  timetoltx_P_mean,
  coef_timetoltx_bldgrpa_mean,
  coef_timetoltx_bldgrpb_mean,
  coef_timetoltx_bldgrpab_mean,
  coef_timetoltx_female_mean,
  coef_timetoltx_agewtlst_mean,
  coef_timetoltx_comorbs_mean,
  coef_timetoltx_prevtxcount_mean,
  #
  timetoltx_mu_se,
  timetoltx_sigma_se,
  timetoltx_Q_se,
  timetoltx_P_se,
  coef_timetoltx_bldgrpa_se,
  coef_timetoltx_bldgrpb_se,
  coef_timetoltx_bldgrpab_se,
  coef_timetoltx_female_se,
  coef_timetoltx_agewtlst_se,
  coef_timetoltx_comorbs_se,
  coef_timetoltx_prevtxcount_se,
  #
  cancer_wtlst,
  #
  mortality_baseline,
  mortality_wtlst,
  mortality_offlstc,
  mortality_long_offlstc,
  mortality_tx,
  mortality_txc,
  mortality_long_txc,
  mortality_tf,
  mortality_tfc,
  mortality_long_tfc,
  #
  coef_donorpbm_int_mean,
  coef_donorpbm_donordeceased_mean,
  coef_donorpbm_agetx_mean,
  coef_donorpbm_female_mean,
  coef_donorpbm_bldgrpa_mean,
  coef_donorpbm_bldgrpb_mean,
  coef_donorpbm_bldgrpab_mean,
  coef_donorpbm_comorbs_mean,
  coef_donorpbm_prevtxcount_mean,
  #
  coef_donorpbm_int_se,
  coef_donorpbm_donordeceased_se,
  coef_donorpbm_agetx_se,
  coef_donorpbm_female_se,
  coef_donorpbm_bldgrpa_se,
  coef_donorpbm_bldgrpb_se,
  coef_donorpbm_bldgrpab_se,
  coef_donorpbm_comorbs_se,
  coef_donorpbm_prevtxcount_se,
  #
  coef_donortype_dbdscd_int_mean,
  coef_donortype_dbdscd_agetx_mean,
  coef_donortype_dbdscd_bldgrpa_mean,
  coef_donortype_dbdscd_bldgrpb_mean,
  coef_donortype_dbdscd_bldgrpab_mean,
  coef_donortype_dbdscd_female_mean,
  coef_donortype_dbdscd_comorbs_mean,
  coef_donortype_dbdscd_prevtxcount_mean,
  coef_donortype_dbdscd_donorpbm_mean,
  #
  coef_donortype_dbdscd_int_se,
  coef_donortype_dbdscd_agetx_se,
  coef_donortype_dbdscd_bldgrpa_se,
  coef_donortype_dbdscd_bldgrpb_se,
  coef_donortype_dbdscd_bldgrpab_se,
  coef_donortype_dbdscd_female_se,
  coef_donortype_dbdscd_comorbs_se,
  coef_donortype_dbdscd_prevtxcount_se,
  coef_donortype_dbdscd_donorpbm_se,
  #
  coef_donortype_dbdecd_int_mean,
  coef_donortype_dbdecd_agetx_mean,
  coef_donortype_dbdecd_bldgrpa_mean,
  coef_donortype_dbdecd_bldgrpb_mean,
  coef_donortype_dbdecd_bldgrpab_mean,
  coef_donortype_dbdecd_female_mean,
  coef_donortype_dbdecd_comorbs_mean,
  coef_donortype_dbdecd_prevtxcount_mean,
  coef_donortype_dbdecd_donorpbm_mean,
  #
  coef_donortype_dbdecd_int_se,
  coef_donortype_dbdecd_agetx_se,
  coef_donortype_dbdecd_bldgrpa_se,
  coef_donortype_dbdecd_bldgrpb_se,
  coef_donortype_dbdecd_bldgrpab_se,
  coef_donortype_dbdecd_female_se,
  coef_donortype_dbdecd_comorbs_se,
  coef_donortype_dbdecd_prevtxcount_se,
  coef_donortype_dbdecd_donorpbm_se,
  #
  coef_donorfemale_int_mean,
  coef_donorfemale_donortypedcd_mean,
  coef_donorfemale_donortypedbdscd_mean,
  coef_donorfemale_donortypedbdecd_mean,
  coef_donorfemale_donorpbm_mean,
  coef_donorfemale_agetx_mean,
  coef_donorfemale_bldgrpa_mean,
  coef_donorfemale_bldgrpb_mean,
  coef_donorfemale_bldgrpab_mean,
  coef_donorfemale_female_mean,
  coef_donorfemale_comorbs_mean,
  coef_donorfemale_prevtxcount_mean,
  #
  coef_donorfemale_int_se,
  coef_donorfemale_donortypedcd_se,
  coef_donorfemale_donortypedbdscd_se,
  coef_donorfemale_donortypedbdecd_se,
  coef_donorfemale_donorpbm_se,
  coef_donorfemale_agetx_se,
  coef_donorfemale_bldgrpa_se,
  coef_donorfemale_bldgrpb_se,
  coef_donorfemale_bldgrpab_se,
  coef_donorfemale_female_se,
  coef_donorfemale_comorbs_se,
  coef_donorfemale_prevtxcount_se,
  #
  coef_donorage_int_mean,
  coef_donorage_donorfemale_mean,
  coef_donorage_donortypedcd_mean,
  coef_donorage_donortypedbdscd_mean,
  coef_donorage_donortypedbdecd_mean,
  coef_donorage_donorpbm_mean,
  coef_donorage_agetx_mean,
  coef_donorage_bldgrpa_mean,
  coef_donorage_bldgrpb_mean,
  coef_donorage_bldgrpab_mean,
  coef_donorage_female_mean,
  coef_donorage_comorbs_mean,
  coef_donorage_prevtxcount_mean,
  rmse_donorage,
  #
  coef_donorage_int_se,
  coef_donorage_donorfemale_se,
  coef_donorage_donortypedcd_se,
  coef_donorage_donortypedbdscd_se,
  coef_donorage_donortypedbdecd_se,
  coef_donorage_donorpbm_se,
  coef_donorage_agetx_se,
  coef_donorage_bldgrpa_se,
  coef_donorage_bldgrpb_se,
  coef_donorage_bldgrpab_se,
  coef_donorage_female_se,
  coef_donorage_comorbs_se,
  coef_donorage_prevtxcount_se,
  #
  coef_donorkdpi_int_mean,
  coef_donorkdpi_donorfemale_mean,
  coef_donorkdpi_donorage_mean,
  coef_donorkdpi_donortypedbdscd_mean,
  coef_donorkdpi_donortypedbdecd_mean,
  coef_donorkdpi_donorpbm_mean,
  coef_donorkdpi_agetx_mean,
  coef_donorkdpi_bldgrpa_mean,
  coef_donorkdpi_bldgrpb_mean,
  coef_donorkdpi_bldgrpab_mean,
  coef_donorkdpi_female_mean,
  coef_donorkdpi_comorbs_mean,
  coef_donorkdpi_prevtxcount_mean,
  rmse_donorkdpi,
  #
  coef_donorkdpi_int_se,
  coef_donorkdpi_donorfemale_se,
  coef_donorkdpi_donorage_se,
  coef_donorkdpi_donortypedbdscd_se,
  coef_donorkdpi_donortypedbdecd_se,
  coef_donorkdpi_donorpbm_se,
  coef_donorkdpi_agetx_se,
  coef_donorkdpi_bldgrpa_se,
  coef_donorkdpi_bldgrpb_se,
  coef_donorkdpi_bldgrpab_se,
  coef_donorkdpi_female_se,
  coef_donorkdpi_comorbs_se,
  coef_donorkdpi_prevtxcount_se,
  #
  timetotxcancer_shape_mean,
  timetotxcancer_scale_mean,
  coef_timetotxcancer_donorage_mean,
  coef_timetotxcancer_donorfemale_mean,
  coef_timetotxcancer_donorkdpi_mean,
  coef_timetotxcancer_donortypedbdscd_mean,
  coef_timetotxcancer_donortypedbdecd_mean,
  coef_timetotxcancer_bldgrpa_mean,
  coef_timetotxcancer_bldgrpb_mean,
  coef_timetotxcancer_bldgrpab_mean,
  coef_timetotxcancer_female_mean,
  coef_timetotxcancer_agetx_mean,
  coef_timetotxcancer_comorbs_mean,
  coef_timetotxcancer_prevtxcount_mean,
  #
  timetotxcancer_shape_se,
  timetotxcancer_scale_se,
  coef_timetotxcancer_donorage_se,
  coef_timetotxcancer_donorfemale_se,
  coef_timetotxcancer_donorkdpi_se,
  coef_timetotxcancer_donortypedbdscd_se,
  coef_timetotxcancer_donortypedbdecd_se,
  coef_timetotxcancer_bldgrpa_se,
  coef_timetotxcancer_bldgrpb_se,
  coef_timetotxcancer_bldgrpab_se,
  coef_timetotxcancer_female_se,
  coef_timetotxcancer_agetx_se,
  coef_timetotxcancer_comorbs_se,
  coef_timetotxcancer_prevtxcount_se,
  #
  #
  timetoltxcancer_shape_mean,
  timetoltxcancer_scale_mean,
  coef_timetoltxcancer_donorage_mean,
  coef_timetoltxcancer_donorfemale_mean,
  coef_timetoltxcancer_bldgrpa_mean,
  coef_timetoltxcancer_bldgrpb_mean,
  coef_timetoltxcancer_bldgrpab_mean,
  coef_timetoltxcancer_female_mean,
  coef_timetoltxcancer_agetx_mean,
  coef_timetoltxcancer_comorbs_mean,
  coef_timetoltxcancer_prevtxcount_mean,
  #
  timetoltxcancer_shape_se,
  timetoltxcancer_scale_se,
  coef_timetoltxcancer_donorage_se,
  coef_timetoltxcancer_donorfemale_se,
  coef_timetoltxcancer_bldgrpa_se,
  coef_timetoltxcancer_bldgrpb_se,
  coef_timetoltxcancer_bldgrpab_se,
  coef_timetoltxcancer_female_se,
  coef_timetoltxcancer_agetx_se,
  coef_timetoltxcancer_comorbs_se,
  coef_timetoltxcancer_prevtxcount_se,
  #
  timetoltxfail_splinegamma_mean,
  timetoltxfail_splineknots,
  coef_timetoltxfail_donorage_mean,
  coef_timetoltxfail_donorfemale_mean,
  coef_timetoltxfail_bldgrpa_mean,
  coef_timetoltxfail_bldgrpb_mean,
  coef_timetoltxfail_bldgrpab_mean,
  coef_timetoltxfail_female_mean,
  coef_timetoltxfail_agetx_mean,
  coef_timetoltxfail_comorbs_mean,
  coef_timetoltxfail_prevtxcount_mean,
  #
  timetoltxfail_splinegamma_se,
  coef_timetoltxfail_donorage_se,
  coef_timetoltxfail_donorfemale_se,
  coef_timetoltxfail_bldgrpa_se,
  coef_timetoltxfail_bldgrpb_se,
  coef_timetoltxfail_bldgrpab_se,
  coef_timetoltxfail_female_se,
  coef_timetoltxfail_agetx_se,
  coef_timetoltxfail_comorbs_se,
  coef_timetoltxfail_prevtxcount_se,
  #
  timetotxfail_splinegamma_mean,
  timetotxfail_splineknots,
  coef_timetotxfail_donortypedbdscd_mean,
  coef_timetotxfail_donortypedbdecd_mean,
  coef_timetotxfail_donorage_mean,
  coef_timetotxfail_donorfemale_mean,
  coef_timetotxfail_donorkdpi_mean,
  coef_timetotxfail_bldgrpa_mean,
  coef_timetotxfail_bldgrpb_mean,
  coef_timetotxfail_bldgrpab_mean,
  coef_timetotxfail_female_mean,
  coef_timetotxfail_agetx_mean,
  coef_timetotxfail_comorbs_mean,
  coef_timetotxfail_prevtxcount_mean,
  #
  timetotxfail_splinegamma_se,
  coef_timetotxfail_donortypedbdscd_se,
  coef_timetotxfail_donortypedbdecd_se,
  coef_timetotxfail_donorage_se,
  coef_timetotxfail_donorfemale_se,
  coef_timetotxfail_donorkdpi_se,
  coef_timetotxfail_bldgrpa_se,
  coef_timetotxfail_bldgrpb_se,
  coef_timetotxfail_bldgrpab_se,
  coef_timetotxfail_female_se,
  coef_timetotxfail_agetx_se,
  coef_timetotxfail_comorbs_se,
  coef_timetotxfail_prevtxcount_se,
  #
  timetoltxcfail_mu_mean,
  timetoltxcfail_sigma_mean,
  timetoltxcfail_Q_mean,
  coef_timetoltxcfail_donorage_mean,
  coef_timetoltxcfail_donorfemale_mean,
  coef_timetoltxcfail_bldgrpa_mean,
  coef_timetoltxcfail_bldgrpb_mean,
  coef_timetoltxcfail_bldgrpab_mean,
  coef_timetoltxcfail_female_mean,
  coef_timetoltxcfail_agetx_mean,
  coef_timetoltxcfail_comorbs_mean,
  coef_timetoltxcfail_prevtxcount_mean,
  coef_timetoltxcfail_agetxc_mean,
  #
  timetoltxcfail_mu_se,
  timetoltxcfail_sigma_se,
  timetoltxcfail_Q_se,
  coef_timetoltxcfail_donorage_se,
  coef_timetoltxcfail_donorfemale_se,
  coef_timetoltxcfail_bldgrpa_se,
  coef_timetoltxcfail_bldgrpb_se,
  coef_timetoltxcfail_bldgrpab_se,
  coef_timetoltxcfail_female_se,
  coef_timetoltxcfail_agetx_se,
  coef_timetoltxcfail_comorbs_se,
  coef_timetoltxcfail_prevtxcount_se,
  coef_timetoltxcfail_agetxc_se,
  #
  timetotxcfail_mu_mean,
  timetotxcfail_sigma_mean,
  timetotxcfail_Q_mean,
  coef_timetotxcfail_donortypedbdscd_mean,
  coef_timetotxcfail_donortypedbdecd_mean,
  coef_timetotxcfail_donorage_mean,
  coef_timetotxcfail_donorfemale_mean,
  coef_timetotxcfail_donorkdpi_mean,
  coef_timetotxcfail_bldgrpa_mean,
  coef_timetotxcfail_bldgrpb_mean,
  coef_timetotxcfail_bldgrpab_mean,
  coef_timetotxcfail_female_mean,
  coef_timetotxcfail_agetx_mean,
  coef_timetotxcfail_comorbs_mean,
  coef_timetotxcfail_prevtxcount_mean,
  coef_timetotxcfail_agetxc_mean,
  #
  timetotxcfail_mu_se,
  timetotxcfail_sigma_se,
  timetotxcfail_Q_se,
  coef_timetotxcfail_donortypedbdscd_se,
  coef_timetotxcfail_donortypedbdecd_se,
  coef_timetotxcfail_donorage_se,
  coef_timetotxcfail_donorfemale_se,
  coef_timetotxcfail_donorkdpi_se,
  coef_timetotxcfail_bldgrpa_se,
  coef_timetotxcfail_bldgrpb_se,
  coef_timetotxcfail_bldgrpab_se,
  coef_timetotxcfail_female_se,
  coef_timetotxcfail_agetx_se,
  coef_timetotxcfail_comorbs_se,
  coef_timetotxcfail_prevtxcount_se,
  coef_timetotxcfail_agetxc_se
)



# Print statistical inputs to a table ----
load(file = file.path(inputsloc,"statistical_inputs.RData"))

## Create a function to format numbers appropriately
fmt <- function(x) {
  if (round(x, 2) == 0) {
    format(signif(x, 1), scientific = FALSE)
  } else {
    format(round(x, 2), nsmall = 2, scientific = FALSE)
  }
}

disp <- function(x, y) {
  paste0(fmt(get(paste0("coef_", x, "_", y, "_mean"))),
         " (", fmt(get(paste0("coef_", x, "_", y, "_se"))), ")")
}


## Patient characteristics
patient_chars <- data.frame()

patient_chars[2, 1] <- disp("prevtxcount", "int")

patient_chars[2, 2] <- disp("bldgrp_bldgrpa", "int")
patient_chars[3, 2] <- disp("bldgrp_bldgrpa", "prevtxcount")

patient_chars[2, 3] <- disp("bldgrp_bldgrpb", "int")
patient_chars[3, 3] <- disp("bldgrp_bldgrpb", "prevtxcount")

patient_chars[2, 4] <- disp("bldgrp_bldgrpab", "int")
patient_chars[3, 4] <- disp("bldgrp_bldgrpab", "prevtxcount")

patient_chars[2, 5] <- disp("female", "int")
patient_chars[3, 5] <- disp("female", "prevtxcount")
patient_chars[4, 5] <- disp("female", "bldgrpa")
patient_chars[5, 5] <- disp("female", "bldgrpb")
patient_chars[6, 5] <- disp("female", "bldgrpab")

patient_chars[1, 6] <- fmt(rmse_agewtlst)
patient_chars[2, 6] <- disp("agewtlst", "int")
patient_chars[3, 6] <- disp("agewtlst", "prevtxcount")
patient_chars[4, 6] <- disp("agewtlst", "bldgrpa")
patient_chars[5, 6] <- disp("agewtlst", "bldgrpb")
patient_chars[6, 6] <- disp("agewtlst", "bldgrpab")
patient_chars[7, 6] <- disp("agewtlst", "female")

patient_chars[2, 7] <- disp("comorbs", "int")
patient_chars[3, 7] <- disp("comorbs", "prevtxcount")
patient_chars[4, 7] <- disp("comorbs", "bldgrpa")
patient_chars[5, 7] <- disp("comorbs", "bldgrpb")
patient_chars[6, 7] <- disp("comorbs", "bldgrpab")
patient_chars[7, 7] <- disp("comorbs", "female")
patient_chars[8, 7] <- disp("comorbs", "agewtlst")

patient_chars[is.na(patient_chars)] <- "-"



## Donor characteristics
donor_chars <- data.frame()

donor_chars[2, 1] <- disp("donorpbm", "int")
donor_chars[3, 1] <- disp("donorpbm", "prevtxcount")
donor_chars[4, 1] <- disp("donorpbm", "bldgrpa")
donor_chars[5, 1] <- disp("donorpbm", "bldgrpb")
donor_chars[6, 1] <- disp("donorpbm", "bldgrpab")
donor_chars[7, 1] <- disp("donorpbm", "female")
donor_chars[8, 1] <- disp("donorpbm", "comorbs")
donor_chars[9, 1] <- disp("donorpbm", "agetx")
donor_chars[10, 1] <- disp("donorpbm", "donordeceased")

donor_chars[2, 2] <- disp("donortype_dbdscd", "int")
donor_chars[3, 2] <- disp("donortype_dbdscd", "prevtxcount")
donor_chars[4, 2] <- disp("donortype_dbdscd", "bldgrpa")
donor_chars[5, 2] <- disp("donortype_dbdscd", "bldgrpb")
donor_chars[6, 2] <- disp("donortype_dbdscd", "bldgrpab")
donor_chars[7, 2] <- disp("donortype_dbdscd", "female")
donor_chars[8, 2] <- disp("donortype_dbdscd", "comorbs")
donor_chars[9, 2] <- disp("donortype_dbdscd", "agetx")
donor_chars[11, 2] <- disp("donortype_dbdscd", "donorpbm")

donor_chars[2, 3] <- disp("donortype_dbdecd", "int")
donor_chars[3, 3] <- disp("donortype_dbdecd", "prevtxcount")
donor_chars[4, 3] <- disp("donortype_dbdecd", "bldgrpa")
donor_chars[5, 3] <- disp("donortype_dbdecd", "bldgrpb")
donor_chars[6, 3] <- disp("donortype_dbdecd", "bldgrpab")
donor_chars[7, 3] <- disp("donortype_dbdecd", "female")
donor_chars[8, 3] <- disp("donortype_dbdecd", "comorbs")
donor_chars[9, 3] <- disp("donortype_dbdecd", "agetx")
donor_chars[11, 3] <- disp("donortype_dbdecd", "donorpbm")

donor_chars[2, 4] <- disp("donorfemale", "int")
donor_chars[3, 4] <- disp("donorfemale", "prevtxcount")
donor_chars[4, 4] <- disp("donorfemale", "bldgrpa")
donor_chars[5, 4] <- disp("donorfemale", "bldgrpb")
donor_chars[6, 4] <- disp("donorfemale", "bldgrpab")
donor_chars[7, 4] <- disp("donorfemale", "female")
donor_chars[8, 4] <- disp("donorfemale", "comorbs")
donor_chars[9, 4] <- disp("donorfemale", "agetx")
donor_chars[11, 4] <- disp("donorfemale", "donorpbm")
donor_chars[12, 4] <- disp("donorfemale", "donortypedcd")
donor_chars[13, 4] <- disp("donorfemale", "donortypedbdscd")
donor_chars[14, 4] <- disp("donorfemale", "donortypedbdecd")

donor_chars[1, 5] <- fmt(rmse_donorage)
donor_chars[2, 5] <- disp("donorage", "int")
donor_chars[3, 5] <- disp("donorage", "prevtxcount")
donor_chars[4, 5] <- disp("donorage", "bldgrpa")
donor_chars[5, 5] <- disp("donorage", "bldgrpb")
donor_chars[6, 5] <- disp("donorage", "bldgrpab")
donor_chars[7, 5] <- disp("donorage", "female")
donor_chars[8, 5] <- disp("donorage", "comorbs")
donor_chars[9, 5] <- disp("donorage", "agetx")
donor_chars[11, 5] <- disp("donorage", "donorpbm")
donor_chars[12, 5] <- disp("donorage", "donortypedcd")
donor_chars[13, 5] <- disp("donorage", "donortypedbdscd")
donor_chars[14, 5] <- disp("donorage", "donortypedbdecd")
donor_chars[15, 5] <- disp("donorage", "donorfemale")

donor_chars[1, 6] <- fmt(rmse_donorkdpi)
donor_chars[2, 6] <- disp("donorkdpi", "int")
donor_chars[3, 6] <- disp("donorkdpi", "prevtxcount")
donor_chars[4, 6] <- disp("donorkdpi", "bldgrpa")
donor_chars[5, 6] <- disp("donorkdpi", "bldgrpb")
donor_chars[6, 6] <- disp("donorkdpi", "bldgrpab")
donor_chars[7, 6] <- disp("donorkdpi", "female")
donor_chars[8, 6] <- disp("donorkdpi", "comorbs")
donor_chars[9, 6] <- disp("donorkdpi", "agetx")
donor_chars[11, 6] <- disp("donorkdpi", "donorpbm")
donor_chars[13, 6] <- disp("donorkdpi", "donortypedbdscd")
donor_chars[14, 6] <- disp("donorkdpi", "donortypedbdecd")
donor_chars[15, 6] <- disp("donorkdpi", "donorfemale")
donor_chars[16, 6] <- disp("donorkdpi", "donorage")

donor_chars[is.na(donor_chars)] <- "-"


## Transition probabilities
transition_probs <- data.frame()

transition_probs[1, 1] <- paste0(fmt(timetodtx_shape_mean), " (", fmt(timetodtx_shape_se), ")")
transition_probs[2, 1] <- paste0(fmt(timetodtx_scale_mean), " (", fmt(timetodtx_scale_se), ")")
transition_probs[21, 1] <- disp("timetodtx", "prevtxcount")
transition_probs[22, 1] <- disp("timetodtx", "bldgrpa")
transition_probs[23, 1] <- disp("timetodtx", "bldgrpb")
transition_probs[24, 1] <- disp("timetodtx", "bldgrpab")
transition_probs[25, 1] <- disp("timetodtx", "female")
transition_probs[26, 1] <- disp("timetodtx", "agewtlst")
transition_probs[27, 1] <- disp("timetodtx", "comorbs")

transition_probs[3, 2] <- paste0(fmt(timetoltx_mu_mean), " (", fmt(timetoltx_mu_se), ")")
transition_probs[4, 2] <- paste0(fmt(timetoltx_sigma_mean), " (", fmt(timetoltx_sigma_se), ")")
transition_probs[5, 2] <- paste0(fmt(timetoltx_Q_mean), " (", fmt(timetoltx_Q_se), ")")
transition_probs[6, 2] <- paste0(fmt(timetoltx_P_mean), " (", fmt(timetoltx_P_se), ")")
transition_probs[21, 2] <- disp("timetoltx", "prevtxcount")
transition_probs[22, 2] <- disp("timetoltx", "bldgrpa")
transition_probs[23, 2] <- disp("timetoltx", "bldgrpb")
transition_probs[24, 2] <- disp("timetoltx", "bldgrpab")
transition_probs[25, 2] <- disp("timetoltx", "female")
transition_probs[26, 2] <- disp("timetoltx", "agewtlst")
transition_probs[27, 2] <- disp("timetoltx", "comorbs")

transition_probs[1, 3] <- paste0(fmt(timetotxcancer_shape_mean), " (", fmt(timetotxcancer_shape_se), ")")
transition_probs[2, 3] <- paste0(fmt(timetotxcancer_scale_mean), " (", fmt(timetotxcancer_scale_se), ")")
transition_probs[21, 3] <- disp("timetotxcancer", "prevtxcount")
transition_probs[22, 3] <- disp("timetotxcancer", "bldgrpa")
transition_probs[23, 3] <- disp("timetotxcancer", "bldgrpb")
transition_probs[24, 3] <- disp("timetotxcancer", "bldgrpab")
transition_probs[25, 3] <- disp("timetotxcancer", "female")
transition_probs[27, 3] <- disp("timetotxcancer", "comorbs")
transition_probs[28, 3] <- disp("timetotxcancer", "agetx")
transition_probs[29, 3] <- disp("timetotxcancer", "donortypedbdscd")
transition_probs[30, 3] <- disp("timetotxcancer", "donortypedbdecd")
transition_probs[31, 3] <- disp("timetotxcancer", "donorfemale")
transition_probs[32, 3] <- disp("timetotxcancer", "donorage")
transition_probs[33, 3] <- disp("timetotxcancer", "donorkdpi")

transition_probs[1, 4] <- paste0(fmt(timetoltxcancer_shape_mean), " (", fmt(timetoltxcancer_shape_se), ")")
transition_probs[2, 4] <- paste0(fmt(timetoltxcancer_scale_mean), " (", fmt(timetoltxcancer_scale_se), ")")
transition_probs[21, 4] <- disp("timetoltxcancer", "prevtxcount")
transition_probs[22, 4] <- disp("timetoltxcancer", "bldgrpa")
transition_probs[23, 4] <- disp("timetoltxcancer", "bldgrpb")
transition_probs[24, 4] <- disp("timetoltxcancer", "bldgrpab")
transition_probs[25, 4] <- disp("timetoltxcancer", "female")
transition_probs[27, 4] <- disp("timetoltxcancer", "comorbs")
transition_probs[28, 4] <- disp("timetoltxcancer", "agetx")
transition_probs[31, 4] <- disp("timetoltxcancer", "donorfemale")
transition_probs[32, 4] <- disp("timetoltxcancer", "donorage")

transition_probs[7, 5] <- paste0(fmt(timetoltxfail_splinegamma_mean[1]), " (", fmt(timetoltxfail_splinegamma_se[1]), ")")
transition_probs[8, 5] <- paste0(fmt(timetoltxfail_splinegamma_mean[2]), " (", fmt(timetoltxfail_splinegamma_se[2]), ")")
transition_probs[9, 5] <- paste0(fmt(timetoltxfail_splinegamma_mean[3]), " (", fmt(timetoltxfail_splinegamma_se[3]), ")")
transition_probs[10, 5] <- paste0(fmt(timetoltxfail_splinegamma_mean[4]), " (", fmt(timetoltxfail_splinegamma_se[4]), ")")
transition_probs[11, 5] <- paste0(fmt(timetoltxfail_splinegamma_mean[5]), " (", fmt(timetoltxfail_splinegamma_se[5]), ")")
transition_probs[12, 5] <- paste0(fmt(timetoltxfail_splinegamma_mean[6]), " (", fmt(timetoltxfail_splinegamma_se[6]), ")")
transition_probs[13, 5] <- paste0(fmt(timetoltxfail_splinegamma_mean[7]), " (", fmt(timetoltxfail_splinegamma_se[7]), ")")
transition_probs[14, 5] <- fmt(timetoltxfail_splineknots[1])
transition_probs[15, 5] <- fmt(timetoltxfail_splineknots[2])
transition_probs[16, 5] <- fmt(timetoltxfail_splineknots[3])
transition_probs[17, 5] <- fmt(timetoltxfail_splineknots[4])
transition_probs[18, 5] <- fmt(timetoltxfail_splineknots[5])
transition_probs[19, 5] <- fmt(timetoltxfail_splineknots[6])
transition_probs[20, 5] <- fmt(timetoltxfail_splineknots[7])
transition_probs[21, 5] <- disp("timetoltxfail", "prevtxcount")
transition_probs[22, 5] <- disp("timetoltxfail", "bldgrpa")
transition_probs[23, 5] <- disp("timetoltxfail", "bldgrpb")
transition_probs[24, 5] <- disp("timetoltxfail", "bldgrpab")
transition_probs[25, 5] <- disp("timetoltxfail", "female")
transition_probs[27, 5] <- disp("timetoltxfail", "comorbs")
transition_probs[28, 5] <- disp("timetoltxfail", "agetx")
transition_probs[31, 5] <- disp("timetoltxfail", "donorfemale")
transition_probs[32, 5] <- disp("timetoltxfail", "donorage")

transition_probs[3, 6] <- paste0(fmt(timetoltxcfail_mu_mean), " (", fmt(timetoltxcfail_mu_se), ")")
transition_probs[4, 6] <- paste0(fmt(timetoltxcfail_sigma_mean), " (", fmt(timetoltxcfail_sigma_se), ")")
transition_probs[5, 6] <- paste0(fmt(timetoltxcfail_Q_mean), " (", fmt(timetoltxcfail_Q_se), ")")
transition_probs[21, 6] <- disp("timetoltxcfail", "prevtxcount")
transition_probs[22, 6] <- disp("timetoltxcfail", "bldgrpa")
transition_probs[23, 6] <- disp("timetoltxcfail", "bldgrpb")
transition_probs[24, 6] <- disp("timetoltxcfail", "bldgrpab")
transition_probs[25, 6] <- disp("timetoltxcfail", "female")
transition_probs[27, 6] <- disp("timetoltxcfail", "comorbs")
transition_probs[28, 6] <- disp("timetoltxcfail", "agetx")
transition_probs[31, 6] <- disp("timetoltxcfail", "donorfemale")
transition_probs[32, 6] <- disp("timetoltxcfail", "donorage")
transition_probs[34, 6] <- disp("timetoltxcfail", "agetxc")

transition_probs[7, 7] <- paste0(fmt(timetotxfail_splinegamma_mean[1]), " (", fmt(timetotxfail_splinegamma_se[1]), ")")
transition_probs[8, 7] <- paste0(fmt(timetotxfail_splinegamma_mean[2]), " (", fmt(timetotxfail_splinegamma_se[2]), ")")
transition_probs[9, 7] <- paste0(fmt(timetotxfail_splinegamma_mean[3]), " (", fmt(timetotxfail_splinegamma_se[3]), ")")
transition_probs[10, 7] <- paste0(fmt(timetotxfail_splinegamma_mean[4]), " (", fmt(timetotxfail_splinegamma_se[4]), ")")
transition_probs[11, 7] <- paste0(fmt(timetotxfail_splinegamma_mean[5]), " (", fmt(timetotxfail_splinegamma_se[5]), ")")
transition_probs[12, 7] <- paste0(fmt(timetotxfail_splinegamma_mean[6]), " (", fmt(timetotxfail_splinegamma_se[6]), ")")
transition_probs[13, 7] <- paste0(fmt(timetotxfail_splinegamma_mean[7]), " (", fmt(timetotxfail_splinegamma_se[7]), ")")
transition_probs[14, 7] <- fmt(timetotxfail_splineknots[1])
transition_probs[15, 7] <- fmt(timetotxfail_splineknots[2])
transition_probs[16, 7] <- fmt(timetotxfail_splineknots[3])
transition_probs[17, 7] <- fmt(timetotxfail_splineknots[4])
transition_probs[18, 7] <- fmt(timetotxfail_splineknots[5])
transition_probs[19, 7] <- fmt(timetotxfail_splineknots[6])
transition_probs[20, 7] <- fmt(timetotxfail_splineknots[7])
transition_probs[21, 7] <- disp("timetotxfail", "prevtxcount")
transition_probs[22, 7] <- disp("timetotxfail", "bldgrpa")
transition_probs[23, 7] <- disp("timetotxfail", "bldgrpb")
transition_probs[24, 7] <- disp("timetotxfail", "bldgrpab")
transition_probs[25, 7] <- disp("timetotxfail", "female")
transition_probs[27, 7] <- disp("timetotxfail", "comorbs")
transition_probs[28, 7] <- disp("timetotxfail", "agetx")
transition_probs[29, 7] <- disp("timetotxfail", "donortypedbdscd")
transition_probs[30, 7] <- disp("timetotxfail", "donortypedbdecd")
transition_probs[31, 7] <- disp("timetotxfail", "donorfemale")
transition_probs[32, 7] <- disp("timetotxfail", "donorage")
transition_probs[33, 7] <- disp("timetotxfail", "donorkdpi")

transition_probs[3, 8] <- paste0(fmt(timetotxcfail_mu_mean), " (", fmt(timetotxcfail_mu_se), ")")
transition_probs[4, 8] <- paste0(fmt(timetotxcfail_sigma_mean), " (", fmt(timetotxcfail_sigma_se), ")")
transition_probs[5, 8] <- paste0(fmt(timetotxcfail_Q_mean), " (", fmt(timetotxcfail_Q_se), ")")
transition_probs[21, 8] <- disp("timetotxcfail", "prevtxcount")
transition_probs[22, 8] <- disp("timetotxcfail", "bldgrpa")
transition_probs[23, 8] <- disp("timetotxcfail", "bldgrpb")
transition_probs[24, 8] <- disp("timetotxcfail", "bldgrpab")
transition_probs[25, 8] <- disp("timetotxcfail", "female")
transition_probs[27, 8] <- disp("timetotxcfail", "comorbs")
transition_probs[28, 8] <- disp("timetotxcfail", "agetx")
transition_probs[29, 8] <- disp("timetotxcfail", "donortypedbdscd")
transition_probs[30, 8] <- disp("timetotxcfail", "donortypedbdecd")
transition_probs[31, 8] <- disp("timetotxcfail", "donorfemale")
transition_probs[32, 8] <- disp("timetotxcfail", "donorage")
transition_probs[33, 8] <- disp("timetotxcfail", "donorkdpi")
transition_probs[34, 8] <- disp("timetotxcfail", "agetxc")

transition_probs[is.na(transition_probs)] <- "-"


## Life tables
lifetables <- mortality_baseline %>%
  left_join(mortality_wtlst) %>% rename(wtlst = annualrate) %>%
  left_join(mortality_offlstc) %>% rename_with(~ gsub("annualrate", "offlstc", .x), starts_with("annualrate")) %>%
  left_join(mortality_tx) %>% rename(tx = annualrate) %>%
  left_join(mortality_txc) %>% rename_with(~ gsub("annualrate", "txc", .x), starts_with("annualrate"))


## Cancer incidence
cancertables <- cancer_baseline %>%
  left_join(cancer_wtlst) %>% rename(dialysis = annualrate) %>%
  arrange(age, female)


## Save a copy of all tables
tablesloc <- file.path(paper3loc, "Tables and figures")

write.csv(patient_chars, file = file.path(tablesloc, "patient_chars.csv"))
write.csv(donor_chars, file = file.path(tablesloc, "donor_chars.csv"))
write.csv(transition_probs, file = file.path(tablesloc, "transition_probs.csv"))
write.csv(lifetables, file = file.path(tablesloc, "lifetables.csv"))
write.csv(cancertables, file = file.path(tablesloc, "cancertables.csv"))
write.csv(cancer_relativesurvival_summary, file = file.path(tablesloc, "cancersurvival.csv"))



# Summarise 'average' patient and donor characteristics ----
load(file.path(inputsloc,'statistical_inputs.RData'))

## Waitlist patients ----
## prevtxcount
f_prevtxcount <- 
  coef_prevtxcount_int_mean

prevtxcount_median <- qpois(0.5, lambda = exp(f_prevtxcount))

## blood group
f_bldgrpa <-
  coef_bldgrp_bldgrpa_int_mean
expf_bldgrpa <- exp(f_bldgrpa)

f_bldgrpb <-
  coef_bldgrp_bldgrpb_int_mean
expf_bldgrpb <- exp(f_bldgrpb)

f_bldgrpab <-
  coef_bldgrp_bldgrpab_int_mean
expf_bldgrpab <- exp(f_bldgrpab)

expf_sum <- expf_bldgrpa + expf_bldgrpb + expf_bldgrpab

p_bldgrpo <- as.numeric(unlist(1/(1+expf_sum)))
p_bldgrpa <- as.numeric(unlist(expf_bldgrpa/(1+expf_sum)))
p_bldgrpb <- as.numeric(unlist(expf_bldgrpb/(1+expf_sum)))
p_bldgrpab <- as.numeric(unlist(expf_bldgrpab/(1+expf_sum)))

bldgrp_mostlikely <- c("O", "A", "B", "AB")[which.max(c(p_bldgrpo, p_bldgrpa, p_bldgrpb, p_bldgrpab))]

## female
logistic <- function(x){exp(x)/(1+exp(x))}
f_female <- 
  coef_female_int_mean
p_female <- logistic(f_female)

sex_mostlikely <- c("Female", "Male")[which.max(c(p_female, 1-p_female))]

## age at waitlisting
f_agewtlst <- 
  coef_agewtlst_int_mean

agewtlst_mean <- f_agewtlst

## comorbidities
f_comorbs <- 
  coef_comorbs_int_mean + 
  coef_comorbs_agewtlst_mean * agewtlst_mean

comorbs_median <- qpois(0.5,lambda=exp(f_comorbs))



## Transplant recipients ----
## Number of previous transplants
cohort2 %>% tabyl(prevtxcount)

model_prevtxcount <- glm(data = cohort2,
                         family = poisson,
                         formula = prevtxcount ~ 1)

coef_prevtxcount_int_mean2 <- model_prevtxcount$coefficients["(Intercept)"] %>% as.numeric()

## Bloodgroup
model_bloodgroup <- multinom(data = cohort2,
                             family = binomial,
                             formula = bloodgroup ~ prevtxcount)

coef_bldgrp_bldgrpa_int_mean2 <- summary(model_bloodgroup)$coefficients['A','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpa_prevtxcount_mean2 <- summary(model_bloodgroup)$coefficients['A','prevtxcount'] %>% as.numeric()

coef_bldgrp_bldgrpb_int_mean2 <- summary(model_bloodgroup)$coefficients['B','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpb_prevtxcount_mean2 <- summary(model_bloodgroup)$coefficients['B','prevtxcount'] %>% as.numeric()

coef_bldgrp_bldgrpab_int_mean2 <- summary(model_bloodgroup)$coefficients['AB','(Intercept)'] %>% as.numeric()
coef_bldgrp_bldgrpab_prevtxcount_mean2 <- summary(model_bloodgroup)$coefficients['AB','prevtxcount'] %>% as.numeric()

## Sex (female)
model_female <- glm(data=cohort2,
                    family=binomial,
                    formula= female ~ bloodgroup + prevtxcount)

coef_female_int_mean2 <- model_female$coefficients["(Intercept)"] %>% as.numeric()
coef_female_bldgrpa_mean2 <- model_female$coefficients['bloodgroupA'] %>% as.numeric()
coef_female_bldgrpb_mean2 <- model_female$coefficients['bloodgroupB'] %>% as.numeric()
coef_female_bldgrpab_mean2 <- model_female$coefficients['bloodgroupAB'] %>% as.numeric()
coef_female_prevtxcount_mean2 <- model_female$coefficients['prevtxcount'] %>% as.numeric()

## Age at transplant
model_agetx <- glm(data=cohort2,
                      family=gaussian,
                      formula= ageattransplant ~ bloodgroup + female + prevtxcount)

coef_agetx_int_mean2 <- model_agetx$coefficients["(Intercept)"] %>% as.numeric()
coef_agetx_bldgrpa_mean2 <- model_agetx$coefficients['bloodgroupA'] %>% as.numeric()
coef_agetx_bldgrpb_mean2 <- model_agetx$coefficients['bloodgroupB'] %>% as.numeric()
coef_agetx_bldgrpab_mean2 <- model_agetx$coefficients['bloodgroupAB'] %>% as.numeric()
coef_agetx_female_mean2 <- model_agetx$coefficients['female'] %>% as.numeric()
coef_agetx_prevtxcount_mean2 <- model_agetx$coefficients['prevtxcount'] %>% as.numeric()
rmse_agetx2 <- sqrt(sum(model_agetx$residuals^2) / model_agetx$df.null)



## Comorbidities
model_comorbs <- glm(data=cohort2,
                     family=poisson,
                     formula= comorbs ~ bloodgroup + female + ageattransplant + prevtxcount)

coef_comorbs_int_mean2 <- model_comorbs$coefficients["(Intercept)"] %>% as.numeric()
coef_comorbs_bldgrpa_mean2 <- model_comorbs$coefficients['bloodgroupA'] %>% as.numeric()
coef_comorbs_bldgrpb_mean2 <- model_comorbs$coefficients['bloodgroupB'] %>% as.numeric()
coef_comorbs_bldgrpab_mean2 <- model_comorbs$coefficients['bloodgroupAB'] %>% as.numeric()
coef_comorbs_female_mean2 <- model_comorbs$coefficients['female'] %>% as.numeric()
coef_comorbs_agetx_mean2 <- model_comorbs$coefficients['ageattransplant'] %>% as.numeric()
coef_comorbs_prevtxcount_mean2 <- model_comorbs$coefficients['prevtxcount'] %>% as.numeric()


## prevtxcount
f_prevtxcount <- 
  coef_prevtxcount_int_mean2

prevtxcount_median <- qpois(0.5, lambda = exp(f_prevtxcount))

## blood group
f_bldgrpa <-
  coef_bldgrp_bldgrpa_int_mean2
expf_bldgrpa <- exp(f_bldgrpa)

f_bldgrpb <-
  coef_bldgrp_bldgrpb_int_mean2
expf_bldgrpb <- exp(f_bldgrpb)

f_bldgrpab <-
  coef_bldgrp_bldgrpab_int_mean2
expf_bldgrpab <- exp(f_bldgrpab)

expf_sum <- expf_bldgrpa + expf_bldgrpb + expf_bldgrpab

p_bldgrpo <- as.numeric(unlist(1/(1+expf_sum)))
p_bldgrpa <- as.numeric(unlist(expf_bldgrpa/(1+expf_sum)))
p_bldgrpb <- as.numeric(unlist(expf_bldgrpb/(1+expf_sum)))
p_bldgrpab <- as.numeric(unlist(expf_bldgrpab/(1+expf_sum)))

bldgrp_mostlikely <- c("O", "A", "B", "AB")[which.max(c(p_bldgrpo, p_bldgrpa, p_bldgrpb, p_bldgrpab))]

## female
logistic <- function(x){exp(x)/(1+exp(x))}
f_female <- 
  coef_female_int_mean2
p_female <- logistic(f_female)

sex_mostlikely <- c("Female", "Male")[which.max(c(p_female, 1-p_female))]

## age at transplant
f_agetx <- 
  coef_agetx_int_mean2

agetx_mean <- f_agetx

## comorbidities
f_comorbs <- 
  coef_comorbs_int_mean2 + 
  coef_comorbs_agetx_mean2 * agetx_mean

comorbs_median <- qpois(0.5,lambda=exp(f_comorbs))


## donor type
f_dbdscd <- 
  coef_donortype_dbdscd_int_mean + 
  coef_donortype_dbdscd_agetx_mean * agetx_mean + 
  coef_donortype_dbdscd_comorbs_mean * comorbs_median
expf_dbdscd <- exp(f_dbdscd)

f_dbdecd <- 
  coef_donortype_dbdecd_int_mean + 
  coef_donortype_dbdecd_agetx_mean * agetx_mean + 
  coef_donortype_dbdecd_comorbs_mean * comorbs_median
expf_dbdecd <- exp(f_dbdecd)

expf_sum <- expf_dbdscd + expf_dbdecd

p_dbdscd <- as.numeric(unlist(expf_dbdscd/(1+expf_sum)))
p_dbdecd <- as.numeric(unlist(expf_dbdecd/(1+expf_sum)))
p_dcd <- as.numeric(unlist(1/(1+expf_sum)))

donortype_mostlikely <- c("DCD", "DBDSCD", "DBDECD")[which.max(c(p_dcd, p_dbdscd, p_dbdecd))]

## donor sex
f_donorfemale <- 
  coef_donorfemale_int_mean + 
  coef_donorfemale_agetx_mean * agetx_mean +
  coef_donorfemale_comorbs_mean * comorbs_median +
  coef_donorfemale_donortypedbdscd_mean * 1
p_donorfemale <- logistic(f_donorfemale)

donorsex_mostlikely <- c("Female", "Male")[which.max(c(p_donorfemale, 1-p_donorfemale))]

## donor age
f_donorage <- 
  coef_donorage_int_mean + 
  coef_donorage_agetx_mean * agetx_mean +
  coef_donorage_comorbs_mean * comorbs_median + 
  coef_donorage_donortypedbdscd_mean * 1 

donorage_mean <- f_donorage

## KDPI
f_donorkdpi <- 
  coef_donorkdpi_int_mean + 
  coef_donorkdpi_agetx_mean * agetx_mean +
  coef_donorkdpi_comorbs_mean * comorbs_median + 
  coef_donorkdpi_donortypedbdscd_mean * 1 + 
  coef_donorkdpi_donorage_mean * donorage_mean

donorkdpi_mean <- exp(f_donorkdpi) / (1 + exp(f_donorkdpi))


## Probability of deceased donor transplant
f_lambda <- 
  log(timetodtx_scale_mean) + 
  coef_timetodtx_agewtlst_mean * agewtlst_mean + 
  coef_timetodtx_comorbs_mean * comorbs_median
lambda <- exp(f_lambda)^(-1/timetodtx_shape_mean)


## Calculate the cumulative probability of transplant at each cycle
p_dtx_1year <- pweibull(q = 365.25,
                        shape = timetodtx_shape_mean,
                        scale = lambda)


## Probability of living donor transplant
log_cumhaz <-
  log(pgenf(q = 365.25,
            mu = timetoltx_mu_mean, 
            sigma = timetoltx_sigma_mean, 
            Q = timetoltx_Q_mean, 
            P = timetoltx_P_mean)) +
  coef_timetoltx_agewtlst_mean * agewtlst_mean +
  coef_timetoltx_comorbs_mean * comorbs_median

p_ltx_1year <- exp(log_cumhaz)


## Probability of cancer after deceased donor transplant
f_lambda <- 
  log(timetotxcancer_scale_mean) + 
  coef_timetotxcancer_agetx_mean * (agewtlst_mean + 0) +
  coef_timetotxcancer_comorbs_mean * comorbs_median +
  coef_timetotxcancer_donortypedbdscd_mean * 1 + 
  coef_timetotxcancer_donorage_mean * donorage_mean +
  coef_timetotxcancer_donorkdpi_mean * donorkdpi_mean
lambda <- exp(f_lambda)^(-1/timetotxcancer_shape_mean)

p_txcancer_1year <- pweibull(q = 365.25,
                             shape = timetotxcancer_shape_mean,
                             scale = lambda)


## Probability of transplant failure without cancer
log_cumhaz <-
  log(psurvspline(q=365.25,
                  gamma = timetotxfail_splinegamma_mean,
                  knots = timetotxfail_splineknots)) +
  coef_timetotxfail_agetx_mean * (agewtlst_mean + 0) + 
  coef_timetotxfail_comorbs_mean * comorbs_median +
  coef_timetotxfail_donortypedbdscd_mean * 1 + 
  coef_timetotxfail_donorage_mean * donorage_mean +
  coef_timetotxfail_donorkdpi_mean * donorkdpi_mean

p_txfail_1year <- exp(log_cumhaz)



## Probability of transplant failure after living donor transplant
log_cumhaz <-
  log(pgengamma(q = 365.25, 
                mu = timetotxcfail_mu_mean, 
                sigma = timetotxcfail_sigma_mean, 
                Q = timetotxcfail_Q_mean)) +
  coef_timetotxcfail_agetx_mean * (agewtlst_mean + 0) + 
  coef_timetotxcfail_agetxc_mean * (agewtlst_mean + 0) + 
  coef_timetotxcfail_comorbs_mean * comorbs_median +
  coef_timetotxcfail_donortypedbdscd_mean * 1 + 
  coef_timetotxcfail_donorage_mean * donorage_mean +
  coef_timetotxcfail_donorkdpi_mean * donorkdpi_mean

p_txcfail_1year <- exp(log_cumhaz)

