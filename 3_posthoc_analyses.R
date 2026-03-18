library(readxl)
library(tidyverse)
library(tidyr)
library(readr)
library(lme4)
library(lmerTest)
library(dbplyr)
library(broom.mixed)
library(e1071)
library(writexl)
library(ggplot2)
library(dplyr)
library(effectsize)
library(mediation)

base_dat <- read_xlsx("brainstr_su/datasets/base_dat.xlsx")

winsorize = function(x,q=3){
  
  mean_x = mean(x,na.rm = T)
  sd_x = sd(x,na.rm = T)
  top_q = mean_x + q*sd_x
  bottom_q = mean_x - q*sd_x
  
  x[x>top_q] = top_q
  x[x<bottom_q] = bottom_q
  
  return(x)
  
}

############# mean_Thck ~ biological drug tests and other 2ndary vars  ########################

analysis.framework <-  data.frame(
  x.vars = c("Breathalyzer_Over_08", "Cocaine","THC", "Opiates", "Amphetamines", 
             "MethAmphetamine", "Oxycontin", "SSAGA_Mj_Times_Used", "SSAGA_TB_Smoking_History"), 
  co.vars = c("", "illic_user", "thc_user",  "illic_user", "illic_user",
              "illic_user", "illic_user", "thc_user", "tobac_user"),
  y.vars = "mean_Thck")

for (m in 1:nrow(analysis.framework)) {
  
  print(m)
  x.var <- analysis.framework$x.vars[m]
  co.var <- analysis.framework$co.vars[m]
  
  column.to.keep = c(which(colnames(base_dat) == analysis.framework$x.vars[m]))
  
  base_dat$x.var =  base_dat[[ column.to.keep ]]
  
  if (co.var != "") {  base_dat$co.var =  base_dat[,c(which(colnames(base_dat) == analysis.framework$co.vars[m]))]
  }else{base_dat$co.var = 1 }
  
  covars <- c("x.var", "co.var", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent",
              "Gender", "Age1", "Age2", "MZ", "DZ", "Half")
  
  var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
                "SSAGA_Income", "SSAGA_Educ","x.var", "co.var", "FS_BrainSeg_Vol_No_Vent", "mean_Thck")
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", 
                  "x.var", "co.var", "FS_BrainSeg_Vol_No_Vent", "mean_Thck")
  var.mutate <- c('x.var', "SSAGA_Income", "SSAGA_Educ", "mean_Thck")
  
  dat.analyze = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()
  
  # normalization skew, scale, center
  
  dat.analyze <- dat.analyze %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze$Age1 = poly(dat.analyze$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze$Age2 = poly(dat.analyze$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  if (co.var != "") {
    formula_str <- paste0("mean_Thck ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  } else {
    formula_str <- paste0("mean_Thck ~ ",paste(setdiff(covars, "co.var"), collapse = " + "), " + (1 | Family_ID)")
  }

  m1 = lmer(as.formula(formula_str),data = dat.analyze)
  
  m1.ci <- confint (m1, parm = "x.var")
  
  dat.out = summary(m1)$coefficients[2,] %>% t()%>% as.data.frame()
  dat.out <- cbind(dat.out, t(m1.ci[1, ]))
  dat.out$xvar = x.var
  dat.out$y.var <-  "mean_Thck"
  dat.out$covar = co.var
  
  if(m ==1){dat.final = matrix(data = NA,nrow = nrow(analysis.framework),ncol = ncol(dat.out)) %>% as.data.frame()}
  
  
  dat.final[m,] = dat.out
  
}

colnames(dat.final) = colnames(dat.out)
dat.final$pfdr = p.adjust(p=dat.final$`Pr(>|t|)`, method = "fdr" )

 write_xlsx(dat.final, "1_meanThck_secondaryDrugVars.xlsx")

################## all brain ~ all drugs (before mean_Thck covar) ############################

y.vars = grep("(Thck$|Vol$|Area$)", colnames(base_dat), value = TRUE)
y.vars <- y.vars[!y.vars %in% c("family_meanThck", "FS_L_WM_Hypointens_Vol",
                                "FS_WM_Hypointens_Vol", "FS_R_WM_Hypointens_Vol",
                                "FS_L_Non-WM_Hypointens_Vol", "FS_R_Non-WM_Hypointens_Vol")]
y.vars = y.vars[!grepl("(WM_Vol|GM_Vol)$", y.vars)]

analysis.framework2 <-  data.frame(
  x.vars = c("audit_c", "SSAGA_Mj_Ab_Dep", "SSAGA_Alc_D4_Dp_Dx", "onset_tobac", "onset_alc", 
             "onset_illic", "onset_thc", "thc_heavy", "illic_max", "tobac_heavy",
             "thc_user", "illic_user", "tobac_user", "SSAGA_Times_Used_Illicits",
             "SSAGA_Mj_Times_Used", "Breathalyzer_Over_08",
             "Cocaine", "THC", "Opiates", "Amphetamines", "MethAmphetamine", "Oxycontin"), 
  co.vars = c("","thc_user", "audit_c", "tobac_user", "audit_c", "illic_user", "thc_user", "thc_user", "illic_user", 
              "tobac_user", "", "", "", "illic_user", "thc_user", "", "", "", "", "", "", ""))

analysis.framework2 = merge(y.vars,analysis.framework2)
colnames(analysis.framework2)[1] = "y.vars"

for (m in 1:nrow(analysis.framework2)) {
  
  print(m)
  x.var <- analysis.framework2$x.vars[m]
  co.var <- analysis.framework2$co.vars[m]
  y.var <- analysis.framework2$y.vars[m]
  
  column.to.keep = c(which(colnames(base_dat) == analysis.framework2$x.vars[m]))
  yvar.to.keep <- c(which(colnames(base_dat) == analysis.framework2$y.vars[m]))
  
  base_dat$x.var =  base_dat[[ column.to.keep ]]
  base_dat$y.var = base_dat[[ yvar.to.keep ]]

  if (co.var != "") {
    base_dat$co.var <- base_dat[[co.var]] 
  } else {
    base_dat$co.var <- 1
  }
  
  covars = c("x.var", "co.var", "SSAGA_Income", "SSAGA_Educ", "Gender", "Age1", "Age2", "MZ", "DZ",
              "Half", "FS_BrainSeg_Vol_No_Vent")
  
  var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
               "y.var", "SSAGA_Income", "SSAGA_Educ","x.var","co.var", "FS_BrainSeg_Vol_No_Vent")
  
  var.names <-  c("Gender", "MZ","DZ", "FS_BrainSeg_Vol_No_Vent", "Half","Age_in_Yrs", 
                  "SSAGA_Income", "SSAGA_Educ", "x.var", "y.var")
  var.mutate <- c("y.var", "x.var", "SSAGA_Income","SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent")
  
  dat.analyze2 = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()

  dat.analyze2 <- dat.analyze2 %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze2$Age1 = poly(dat.analyze2$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze2$Age2 = poly(dat.analyze2$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  if (co.var != "") {
    formula_str <- paste0("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  } else {
    formula_str <- paste0("y.var ~ ",paste(setdiff(covars, "co.var"), collapse = " + "), " + (1 | Family_ID)")
  }
  
  m2 = lmer(as.formula(formula_str),data = dat.analyze2)
  
  m1.ci <- confint (m2, parm = "x.var")
  
  dat.out2 = summary(m2)$coefficients["x.var",] %>% t()%>% as.data.frame()
  dat.out2 <- cbind(dat.out2, t(m1.ci["x.var", ]))
  dat.out2$xvar = x.var
  dat.out2$y.var <-  y.var
  dat.out2$co.var <- co.var
  
  if(m ==1){dat.final2 = matrix(data = NA,nrow = nrow(analysis.framework2),ncol = ncol(dat.out2)) %>% as.data.frame()}
  
  
  dat.final2[m,] = dat.out2
  
}

colnames(dat.final2) = colnames(dat.out2)

allbrain_alldrugs = dat.final2

# XVAR BY MODALITY PFDR

allbrain_alldrugs <- allbrain_alldrugs %>%
  mutate(mod = case_when(
    str_ends(y.var, "_Area") ~ "area",
    str_ends(y.var, "_SA") ~ "area",
    str_ends(y.var, "_Vol") ~ "vol",
    str_ends(y.var, "_Thck") ~ "thck",
    TRUE ~ NA_character_),
    type = case_when(
      xvar %in% c("audit_c", "SSAGA_Alc_D4_Dp_Dx", "onset_alc", "Breathalyzer_Over_08") ~ "alc",
      xvar %in% c("SSAGA_Mj_Ab_Dep", "onset_thc", "THC", "thc_heavy", "thc_user", "SSAGA_Mj_Times_Used") ~ "thc",
      xvar %in% c("onset_tobac", "tobac_heavy", "tobac_user") ~ "tobac",
      xvar %in% c("onset_illic", "illic_max", "illic_user", "SSAGA_Times_Used_Illicits", "Cocaine", "Opiates", 
                  "Amphetamines", "MethAmphetamine", "Oxycontin") ~ "illic"))%>%
  mutate(type = factor(type, levels = c("alc", "thc", "tobac", "illic")))


loop2 = expand.grid(xvar = unique(allbrain_alldrugs$xvar),
                    mod = unique(allbrain_alldrugs$mod))
loop2$group_id = c(1:dim(loop2)[1])

allbrain_alldrugs = merge(allbrain_alldrugs,loop2)

allpfdr2 <- data.frame()

for (m in 1:max(loop2$group_id) ) {
  #m=1
  print(m)
  
  dat.in <- allbrain_alldrugs %>%
    filter(group_id == m)
  
  dat.in$pfdr = p.adjust(p=dat.in$`Pr(>|t|)`, method = "fdr" )
  allpfdr2 <- rbind(allpfdr2, dat.in)
}

allbrain_alldrugs <- merge(allbrain_alldrugs,allpfdr2)

# TYPE PFDR 

type <- data.frame(type=unique(allbrain_alldrugs$type),group2 = c(1:length(unique(allbrain_alldrugs$type))))

allbrain_alldrugs <- merge(allbrain_alldrugs,type)

allpfdr2 <- data.frame()

for (m in 1:max(unique(allbrain_alldrugs$group2)) ) {
  #m=1
  print(m)
  
  dat.in <- allbrain_alldrugs %>%
    filter(group2 == m)
  
  dat.in$pfdr_substance = p.adjust(p=dat.in$`Pr(>|t|)`, method = "fdr" )
  allpfdr2 <- rbind(allpfdr2, dat.in)
}

allbrain_alldrugs = merge(allbrain_alldrugs,allpfdr2)

# ALL PFDR

allbrain_alldrugs$pfdr_all = p.adjust(p=allbrain_alldrugs$`Pr(>|t|)`, method = "fdr" )

write_xlsx(allbrain_alldrugs, "2_allbrain_alldrugs_noCtrlThck.xlsx") 

################## all brain ~ all drugs (after mean_Thck covar) ############################

y.vars = grep("(Thck$|Vol$|Area$)", colnames(base_dat), value = TRUE)
y.vars <- y.vars[!y.vars %in% c("family_meanThck", "mean_Thck", "FS_L_WM_Hypointens_Vol",
                                "FS_WM_Hypointens_Vol", "FS_R_WM_Hypointens_Vol",
                                "FS_L_Non-WM_Hypointens_Vol", "FS_R_Non-WM_Hypointens_Vol")]
y.vars = y.vars[!grepl("(WM_Vol|GM_Vol)$", y.vars)]

analysis.framework3 <-  data.frame(
  x.vars = c("audit_c", "SSAGA_Mj_Ab_Dep", "SSAGA_Alc_D4_Dp_Dx", "onset_tobac", "onset_alc", 
             "onset_illic", "onset_thc", "thc_heavy", "illic_max", "tobac_heavy",
             "thc_user", "illic_user", "tobac_user", "SSAGA_Times_Used_Illicits",
             "SSAGA_Mj_Times_Used", "Breathalyzer_Over_08",
             "Cocaine", "THC", "Opiates", "Amphetamines", "MethAmphetamine", "Oxycontin"), 
  co.vars = c("","thc_user", "audit_c", "tobac_user", "audit_c", "illic_user", "thc_user", "thc_user", "illic_user", 
              "tobac_user", "", "", "", "illic_user", "thc_user", "", "", "", "", "", "", ""))

analysis.framework3 = merge(y.vars,analysis.framework3)
colnames(analysis.framework3)[1] = "y.vars"

for (m in 1:nrow(analysis.framework3)) {

  print(m)
  x.var <- analysis.framework3$x.vars[m]
  co.var <- analysis.framework3$co.vars[m]
  y.var <- analysis.framework3$y.vars[m]
  
  column.to.keep = c(which(colnames(base_dat) == analysis.framework3$x.vars[m]))
  yvar.to.keep <- c(which(colnames(base_dat) == analysis.framework3$y.vars[m]))
  
  base_dat$x.var =  base_dat[[ column.to.keep ]]
  base_dat$y.var = base_dat[[ yvar.to.keep ]]
  
  if (co.var != "") {
    base_dat$co.var <- base_dat[[co.var]] 
  } else {
    base_dat$co.var <- 1
  }
  
  covars = c("x.var", "co.var", "mean_Thck", "SSAGA_Income", "SSAGA_Educ", "Gender", "Age1", "Age2", "MZ", "DZ",
             "Half", "FS_BrainSeg_Vol_No_Vent")
  
  var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", "mean_Thck", "FS_BrainSeg_Vol_No_Vent",
               "y.var", "SSAGA_Income", "SSAGA_Educ","x.var","co.var")
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "FS_BrainSeg_Vol_No_Vent", 
                  "SSAGA_Income", "SSAGA_Educ", "x.var", "y.var", "mean_Thck")
  var.mutate <- c("y.var", "x.var", "mean_Thck", "SSAGA_Income","SSAGA_Educ")
  
  dat.analyze3 = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()
  
  dat.analyze3 <- dat.analyze3 %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze3$Age1 = poly(dat.analyze3$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze3$Age2 = poly(dat.analyze3$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  if (co.var != "") {
    formula_str <- paste0("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  } else {
    formula_str <- paste0("y.var ~ ",paste(setdiff(covars, "co.var"), collapse = " + "), " + (1 | Family_ID)")
  }
  
  m3 = lmer(as.formula(formula_str),data = dat.analyze3)
  
  m3.ci <- confint (m3, parm = "x.var")
  
  dat.out3 = summary(m3)$coefficients["x.var",] %>% t()%>% as.data.frame()
  dat.out3 <- cbind(dat.out3, t(m3.ci["x.var", ]))
  dat.out3$xvar = x.var
  dat.out3$y.var <-  y.var
  dat.out3$co.var <- co.var
  
  if(m ==1){dat.final3 = matrix(data = NA,nrow = nrow(analysis.framework3),ncol = ncol(dat.out3)) %>% as.data.frame()}
  
  dat.final3[m,] = dat.out3
  
}

colnames(dat.final3) = colnames(dat.out3)

allbrain_alldrugs = dat.final3

# XVAR BY MODALITY PFDR

allbrain_alldrugs <- allbrain_alldrugs %>%
  mutate(mod = case_when(
    str_ends(y.var, "_Area") ~ "area",
    str_ends(y.var, "_SA") ~ "area",
    str_ends(y.var, "_Vol") ~ "vol",
    str_ends(y.var, "_Thck") ~ "thck",
    TRUE ~ NA_character_),
    type = case_when(
      xvar %in% c("audit_c", "SSAGA_Alc_D4_Dp_Dx", "onset_alc", "Breathalyzer_Over_08") ~ "alc",
      xvar %in% c("SSAGA_Mj_Ab_Dep", "onset_thc", "THC", "thc_heavy", "thc_user", "SSAGA_Mj_Times_Used") ~ "thc",
      xvar %in% c("onset_tobac", "tobac_heavy", "tobac_user") ~ "tobac",
      xvar %in% c("onset_illic", "illic_max", "illic_user", "SSAGA_Times_Used_Illicits", "Cocaine", "Opiates", 
                  "Amphetamines", "MethAmphetamine", "Oxycontin") ~ "illic"))%>%
  mutate(type = factor(type, levels = c("alc", "thc", "tobac", "illic")))


loop2 = expand.grid(xvar = unique(allbrain_alldrugs$xvar),
                    mod = unique(allbrain_alldrugs$mod))
loop2$group_id = c(1:dim(loop2)[1])

allbrain_alldrugs = merge(allbrain_alldrugs,loop2)

allpfdr2 <- data.frame()

for (m in 1:max(loop2$group_id) ) {
  #m=1
  print(m)
  
  dat.in <- allbrain_alldrugs %>%
    filter(group_id == m)
  
  dat.in$pfdr = p.adjust(p=dat.in$`Pr(>|t|)`, method = "fdr" )
  allpfdr2 <- rbind(allpfdr2, dat.in)
}

allbrain_alldrugs = merge(allbrain_alldrugs,allpfdr2)

# TYPE PFDR 

type <- data.frame(type=unique(allbrain_alldrugs$type),group2 = c(1:length(unique(allbrain_alldrugs$type))))

allbrain_alldrugs <- merge(allbrain_alldrugs,type)

allpfdr2 <- data.frame()

for (m in 1:max(unique(allbrain_alldrugs$group2)) ) {
  #m=1
  print(m)
  
  dat.in <- allbrain_alldrugs %>%
    filter(group2 == m)
  
  dat.in$pfdr_substance = p.adjust(p=dat.in$`Pr(>|t|)`, method = "fdr" )
  allpfdr2 <- rbind(allpfdr2, dat.in)
}

allbrain_alldrugs = merge(allbrain_alldrugs,allpfdr2)

# ALL PFDR

allbrain_alldrugs$pfdr_all = p.adjust(p=allbrain_alldrugs$`Pr(>|t|)`, method = "fdr" )
write_xlsx(allbrain_alldrugs, "3_allbrain_alldrugs_ctrlThck.xlsx")

#################### original brain roi ~ all drugs (after mean thck covar) #########

y.vars = colnames(base_dat[,206:229])

analysis.framework4 <-  data.frame(
  x.vars = c("audit_c", "SSAGA_Mj_Ab_Dep", "SSAGA_Alc_D4_Dp_Dx", "onset_tobac", "onset_alc", 
             "onset_illic", "onset_thc", "thc_heavy", "illic_max", "tobac_heavy",
             "thc_user", "illic_user", "tobac_user", "SSAGA_Times_Used_Illicits",
             "SSAGA_Mj_Times_Used", "Breathalyzer_Over_08",
             "Cocaine", "THC", "Opiates", "Amphetamines", "MethAmphetamine", "Oxycontin"), 
  co.vars = c("","thc_user", "audit_c", "tobac_user", "audit_c", "illic_user", "thc_user", "thc_user", "illic_user", 
              "tobac_user", "", "", "", "illic_user", "thc_user", "", "", "", "", "", "", ""))

analysis.framework4 = merge(y.vars,analysis.framework4)
colnames(analysis.framework4)[1] = "y.vars"

analysis.framework4 <- analysis.framework4%>%
  filter(!y.vars == "FS_BrainSeg_Vol_No_Vent")

for (m in 1:nrow(analysis.framework4)) {
  
  print(m)
  x.var <- analysis.framework4$x.vars[m]
  co.var <- analysis.framework4$co.vars[m]
  y.var <- analysis.framework4$y.vars[m]
  
  column.to.keep = c(which(colnames(base_dat) == analysis.framework4$x.vars[m]))
  yvar.to.keep <- c(which(colnames(base_dat) == analysis.framework4$y.vars[m]))
  
  base_dat$x.var =  base_dat[[ column.to.keep ]]
  base_dat$y.var = base_dat[[ yvar.to.keep ]]
  
  if (co.var != "") {
    base_dat$co.var <- base_dat[[co.var]] 
  } else {
    base_dat$co.var <- 1
  }
  
  covars = c("x.var", "co.var", "mean_Thck", "SSAGA_Income", "SSAGA_Educ", "Gender", "Age1", "Age2", "MZ", "DZ",
             "Half", "FS_BrainSeg_Vol_No_Vent")
  
  var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", "mean_Thck", 
               "y.var", "SSAGA_Income", "SSAGA_Educ","x.var","co.var", "FS_BrainSeg_Vol_No_Vent")
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "FS_BrainSeg_Vol_No_Vent",
                  "SSAGA_Income", "SSAGA_Educ", "x.var", "y.var", "mean_Thck")
  var.mutate <- c("y.var", "x.var", "mean_Thck", "SSAGA_Income","SSAGA_Educ")
  
  dat.analyze4 = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()
  
  dat.analyze4 <- dat.analyze4 %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze4$Age1 = poly(dat.analyze4$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze4$Age2 = poly(dat.analyze4$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  if (co.var != "") {
    formula_str <- paste0("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  } else {
    formula_str <- paste0("y.var ~ ",paste(setdiff(covars, "co.var"), collapse = " + "), " + (1 | Family_ID)")
  }
  
  m4 = lmer(as.formula(formula_str),data = dat.analyze4)
  
  m4.ci <- confint (m4, parm = "x.var")
  
  dat.out4 = summary(m4)$coefficients["x.var",] %>% t()%>% as.data.frame()
  dat.out4 <- cbind(dat.out4, t(m4.ci["x.var", ]))
  dat.out4$xvar = x.var
  dat.out4$y.var <-  y.var
  dat.out4$co.var <- co.var
  
  if(m ==1){dat.final4 = matrix(data = NA,nrow = nrow(analysis.framework4),ncol = ncol(dat.out4)) %>% as.data.frame()}
  
  dat.final4[m,] = dat.out4
  
}

colnames(dat.final4) = colnames(dat.out4)

allbrain_alldrugs = dat.final4

# XVAR BY MODALITY PFDR

allbrain_alldrugs <- allbrain_alldrugs %>%
  mutate(mod = case_when(
    str_ends(y.var, "_Area") ~ "area",
    str_ends(y.var, "_SA") ~ "area",
    str_ends(y.var, "_Vol") ~ "vol",
    str_ends(y.var, "_Thck") ~ "thck",
    TRUE ~ NA_character_),
    type = case_when(
      xvar %in% c("audit_c", "SSAGA_Alc_D4_Dp_Dx", "onset_alc", "Breathalyzer_Over_08") ~ "alc",
      xvar %in% c("SSAGA_Mj_Ab_Dep", "onset_thc", "THC", "thc_heavy", "thc_user", "SSAGA_Mj_Times_Used") ~ "thc",
      xvar %in% c("onset_tobac", "tobac_heavy", "tobac_user") ~ "tobac",
      xvar %in% c("onset_illic", "illic_max", "illic_user", "SSAGA_Times_Used_Illicits", "Cocaine", "Opiates", 
                  "Amphetamines", "MethAmphetamine", "Oxycontin") ~ "illic"))%>%
  mutate(type = factor(type, levels = c("alc", "thc", "tobac", "illic")))


loop2 = expand.grid(xvar = unique(allbrain_alldrugs$xvar),
                    mod = unique(allbrain_alldrugs$mod))
loop2$group_id = c(1:dim(loop2)[1])

allbrain_alldrugs = merge(allbrain_alldrugs,loop2)

allpfdr2 <- data.frame()

for (m in 1:max(loop2$group_id) ) {
  #m=1
  print(m)
  
  dat.in <- allbrain_alldrugs %>%
    filter(group_id == m)
  
  dat.in$pfdr = p.adjust(p=dat.in$`Pr(>|t|)`, method = "fdr" )
  allpfdr2 <- rbind(allpfdr2, dat.in)
}

allbrain_alldrugs = merge(allbrain_alldrugs,allpfdr2)

# TYPE PFDR 

type <- data.frame(type=unique(allbrain_alldrugs$type),group2 = c(1:length(unique(allbrain_alldrugs$type))))

allbrain_alldrugs <- merge(allbrain_alldrugs,type)

allpfdr2 <- data.frame()

for (m in 1:max(unique(allbrain_alldrugs$group2)) ) {
  #m=1
  print(m)
  
  dat.in <- allbrain_alldrugs %>%
    filter(group2 == m)
  
  dat.in$pfdr_substance = p.adjust(p=dat.in$`Pr(>|t|)`, method = "fdr" )
  allpfdr2 <- rbind(allpfdr2, dat.in)
}

allbrain_alldrugs = merge(allbrain_alldrugs,allpfdr2)

# ALL PFDR

allbrain_alldrugs$pfdr_all = p.adjust(p=allbrain_alldrugs$`Pr(>|t|)`, method = "fdr" )

allbrain_alldrugs <- allbrain_alldrugs%>%
  dplyr::select(type, xvar, mod, co.var, y.var, everything())

write_xlsx(allbrain_alldrugs, "4_originalbrain_alldrugs_ctrlThck.xlsx")
