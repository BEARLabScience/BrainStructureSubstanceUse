library(readxl)
library(tidyverse)
library(tidyr)
library(readr)
library(writexl)
library(stringr)

base_dir = c("/scratch/g/dbaranger/brainstr_su")
hcp_brain <- read.csv(paste(base_dir,"/datasets/unrestricted_hcp_freesurfer.csv",sep=""))
base_dat <- read_csv(paste0(base_dir,"/datasets/RESTRICTED_baranger2_8_1_2025_10_17_20.csv"))
drugs <- read_xlsx(paste0(base_dir,"/datasets/S1200_SSAGA_Raw_Released_Distribution_Sept2017.xlsx"))
behavior <- read_csv("HCP_analyses/data_sets/Originals/hcp_behavioral_data.csv")
dictionary <- read_csv( "HCP_analyses/data_sets/HCP_S1200_DataDictionary_Oct_30_2023.csv")

### mean_Thck, SA, and Primary Regions ###########################################################

SA = hcp_brain %>% dplyr::select(ends_with("_Area"))  # get surface area variables
TK =  hcp_brain %>% dplyr::select(ends_with("_Thck")) # get cortical thickness variables

total_SA = apply(SA,1,sum) # sums across each row of 'SA', computes the total cortical surface area

colnames(SA) = stringr::str_replace(colnames(SA),pattern = "_Area",replacement = "")
colnames(TK) = stringr::str_replace(colnames(TK),pattern = "_Thck",replacement = "")

all(colnames(SA) == colnames(TK)) # confirm columns are in the same order

mean_Thck = apply(SA*TK,1,sum) / total_SA # computes mean cortical thickness by weighting the average by the size of each region

###############
# pull regions from review https://onlinelibrary.wiley.com/doi/full/10.1111/adb.13327

keep = hcp_brain %>% dplyr::select(contains(c("Subject","Hippo_Vol","Amygdala_Vol","Cerebellum_Cort_Vol",
                                              "Caudalmiddlefrontal_Thck","Rostralmiddlefrontal_Thck","Superiorfrontal_Thck",
                                              "Inferiortemporal_Thck","Middletemporal_Thck","Insula_Thck",
                                              "Precuneus_Thck","Frontalpole_Thck",
                                              "FS_BrainSeg_Vol_No_Vent" # this is a covariate in all the analyses. everything else is a y-variable
)))

keep = keep %>% dplyr::select(!contains(c("ThckStd","Vent_Surf"))) # get rid of things we don't want

colnames(keep)[which(colnames(keep) == "FS_L_Cerebellum_Cort_Vol.1")] = "FS_R_Cerebellum_Cort_Vol" # correct wrong name

keep$total_SA = total_SA
keep$mean_Thck = mean_Thck

hcp_brain_use = keep

base_dat <- left_join(base_dat, keep, by = "Subject")

base_dat <- base_dat %>%
  filter(Subject %in% keep$Subject)

####### sib type variables ############################

base_dat <- base_dat %>%
  mutate(
    MZ = case_when(
      ZygosityGT == "MZ" ~ 1,
      is.na(ZygosityGT) & ZygositySR == "MZ" ~ 1,
      is.na(ZygosityGT) & is.na(ZygositySR) ~ 0,
      TRUE ~ 0),
    DZ = case_when(
      ZygosityGT == "DZ" ~ 1,
      is.na(ZygosityGT) & ZygositySR == "NotMZ" ~ 1,
      is.na(ZygosityGT) & ZygositySR == "NotTwin" ~ 0,
      is.na(ZygosityGT) & is.na(ZygositySR) ~ 0,
      TRUE ~ 0))

# recode twins with missing twin or falsely reported twin as non-twin 

base_dat <- base_dat %>%
  group_by(Family_ID) %>%
  mutate(
    MZ = if (sum(MZ != 0) == 1) 0 else MZ,
    DZ = if (sum(DZ != 0) == 1) 0 else DZ
  ) %>%
  ungroup()%>%
  mutate(DZ = if_else(Subject %in% c("174437", "256540"), 0, DZ),
         MZ = if_else(Subject %in% c("179245", "849264"), 0, MZ))
    
#half sibs 

half2 <- base_dat %>%
  group_by(Family_ID) %>%
  filter(str_count(Family_ID, "_") == 2, n() == 3) %>% 
  group_by(Family_ID, Father_ID, Mother_ID) %>%
  mutate(Half = as.integer(n() < 2)) %>%  
  ungroup() %>%
  dplyr::select(Subject, Half)

base_dat <- base_dat %>%
  left_join(half2, by = "Subject")

base_dat <- base_dat %>%
  group_by(Family_ID) %>%
  mutate(
    Half = case_when(
      str_count(Family_ID, "_") == 1 ~ 0,
      str_count(Family_ID, "_") == 3 & (MZ == 1 | DZ == 1) ~ 0,
      str_count(Family_ID, "_") == 3 & (MZ == 0 & DZ == 0) ~ 1,
      TRUE ~ Half)) %>%
  ungroup()%>%
  mutate(Half = if_else(Family_ID == "56096_85916_99972", 1, Half)) #anomaly family

######### audit_c recoding #######################################

audit_c <- drugs%>%
  dplyr::select(audit_1 = AL4e3,
         audit_2 = AL4e4,
         audit_3 = AL4e1,
         Subject = PUBLIC_ID...1)%>%
  mutate(
    audit_1 = case_when(
      audit_1 %in% c("Never") ~ 0,
      audit_1 %in% c("1 to 2 days per year", "3-5 days per year", "1 day per month") ~ 1,
      audit_1 %in% c("2 days per month", "3 days per month", "1 day per week") ~ 2,
      audit_1 %in% c("2 days per week", "3 days per week") ~ 3,
      audit_1 %in% c("4 days a week", "5-6 days a week", "Every day") ~ 4,
      TRUE ~ as.numeric(NA)),
    audit_2 = case_when(
          audit_2 %in% c(0, 1, 2) ~ 0,
          audit_2 %in% c(3, 4) ~ 1,
          audit_2 %in% c(5, 6) ~ 2,
          audit_2 %in% c(7, 8, 9) ~ 3,
          audit_2 >= 10 ~ 4,
          TRUE ~ as.numeric(NA)),
   audit_3 = case_when(
          audit_3 %in% c("Never") ~ 0,
          audit_3 %in% c("1 to 2 days per year", "3-5 days per year", "6-11 days per year") ~ 1,
          audit_3 %in% c("1 day per month", "2 days per month", "3 days per month") ~ 2,
          audit_3 %in% c("1 day per week", "2 days per week", "3 days per week", "4 days a week") ~ 3,
          audit_3 %in% c("5-6 days a week", "Every day") ~ 4,
          TRUE ~ as.numeric(NA)))

audit_c <- audit_c%>%
  mutate(audit_c = rowSums(across(c(audit_1, audit_2, audit_3)), na.rm = FALSE))

base_dat <- left_join(base_dat, audit_c, by = "Subject")

######### binary lifetime use ################################

base_dat <- base_dat%>%
  mutate(thc_user = ifelse(SSAGA_Mj_Use == 1, 1, 0),
         illic_user = ifelse(SSAGA_Times_Used_Illicits == 0, 0, 1),
         tobac_user = ifelse(SSAGA_TB_Smoking_History == 0, 0, 1))

###### drug use onset ##################################

onset <- drugs%>%
  dplyr::select(Subject = PUBLIC_ID...1, onset_tobac = TB1c_ao1, onset_alc = AL1AgeOn, onset_thc = MJ2AgeOn,
         DR1bAgeO, DR1bAge2, DR1bAge3, DR1bAge4, DR1bAge5, DR1bAge6, DR1bAge7, DR1bAge8, DR1bAge9, DR1bAg10, 
         DR1bAg11, DR1bAg12, DR1bAg13, DR1bAg14, DR1bAg15, DR1bAg16, DR1bAg17)%>%
  mutate(onset_illic = do.call(pmin, c(across(starts_with("DR1bAG")), na.rm = TRUE)))

base_dat <- left_join(base_dat, onset %>% dplyr::select(Subject, onset_tobac, onset_alc, onset_illic, onset_thc), by = "Subject")

# remove anomalies (age alc onset = 98 & 99)

base_dat <- base_dat%>%
  mutate(onset_alc = ifelse(onset_alc %in% c(98, 99), NA, onset_alc))

########## heavy use ######################################

base_dat <- base_dat%>%
  mutate(thc_heavy = ifelse(SSAGA_Mj_Times_Used == 5, 1, 0))

heavy <- drugs%>%
  dplyr::select(DR2B_, DR2B_2, DR2B_3, DR2B_4, DR2B_5, Subject = PUBLIC_ID...1, TB3)%>%
  mutate(illic_max = pmax(DR2B_, DR2B_2, DR2B_3, DR2B_4, DR2B_5, na.rm = TRUE))%>%
  mutate(tobac_heavy = ifelse(TB3 == "YES", 1, 0))

base_dat <- left_join(base_dat, heavy %>% dplyr::select(Subject, illic_max, tobac_heavy), by = "Subject")

############### mediation vars ###########################

missing_items <- setdiff(dictionary$columnHeader, names(base_dat))

existing <- intersect(missing_items, names(behavior))

existing <- existing[-c(1, 2, 4, 5:86, 484:548)] #fmri, completion vars, MEG, NEORAW

base_dat <- base_dat %>%
  left_join(behavior %>% dplyr::select(Subject, all_of(existing)), by = "Subject")

base_dat <- base_dat %>%
  dplyr::select(-ends_with(c("Ratio", "Holes")))

########### within-between family variables ###############################################

base_dat <- base_dat%>%
  group_by(Family_ID)%>%
  mutate(familyMean_alcOnset = mean(onset_alc),
         family_meanAudit = mean(audit_c),
         family_meanTHC = mean(thc_user),
         family_meanThck = mean(mean_Thck), 
         family_meanAnger = mean(AngAffect_Unadj),
         familybirth = mean(Menstrual_UsingBirthControl))%>%
  ungroup(Family_ID)

base_dat <- base_dat%>%
  mutate(withinFam_alcOnset = onset_alc-familyMean_alcOnset,
         withinFamAudit = audit_c-family_meanAudit,
         withinFam_thc = thc_user-family_meanTHC,
         withinFam_thck = mean_Thck-family_meanThck,
         withinFam_anger = AngAffect_Unadj-family_meanAnger,
         withinFam_birth = Menstrual_UsingBirthControl- familybirth)

famid <- base_dat%>%
  distinct(Family_ID)%>%
  mutate(hhid = 1:length(Family_ID))

base_dat <- left_join(base_dat, famid, by = "Family_ID")

base_dat <- base_dat%>%
  group_by(hhid)%>%
  mutate(single = ifelse(n()>=2, 0, 1))%>%
  ungroup()

######### numeric gender, final order #################

base_dat <- base_dat%>%
  rename(gender = Gender)%>%
  mutate(Gender = if_else(gender == "M", 1, 2))%>%
  dplyr::select(Subject, Family_ID, MZ, DZ, Half, Age_in_Yrs, Gender, everything())
  
  write_xlsx(base_dat, "brainstr_su/datasets/base_dat.xlsx")
