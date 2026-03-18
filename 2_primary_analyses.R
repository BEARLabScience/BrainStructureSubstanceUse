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
library(solarius)
library(openxlsx)
library(stringr)
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

########## SHARED VERSES UNIQUE EFFECTS #####################
#########	(primary brain ROI) ~ mAUDIT-C before mean_Thck covariate ##########################

analysis.framework <- data.frame(
y.vars=colnames(base_dat)[c(206:227, 229:230)])

for (m in 1:nrow(analysis.framework)) {
  
  print(m)
  y.var <- analysis.framework$y.vars[m]

  yvar.to.keep <- c(which(colnames(base_dat) == analysis.framework$y.vars[m]))
  
  base_dat$y.var = base_dat[[ yvar.to.keep ]]
  
  covars = c("audit_c", "SSAGA_Income", "SSAGA_Educ", "Gender",
               "Age1", "Age2", "MZ", "DZ", "Half", "FS_BrainSeg_Vol_No_Vent")
  
  var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
               "y.var", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent", "audit_c")
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", "audit_c", "y.var", "FS_BrainSeg_Vol_No_Vent")
  var.mutate <- c("audit_c", "y.var", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent")
  
  dat.analyze = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()
  
  # normalization skew, scale, center
  
  dat.analyze <- dat.analyze %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze$Age1 = poly(dat.analyze$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze$Age2 = poly(dat.analyze$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  ##################
  
  if (str_ends(y.var, "_Thck")) {
    formula_str <- paste0("y.var ~ ",paste(setdiff(covars, "FS_BrainSeg_Vol_No_Vent"), collapse = " + "), " + (1 | Family_ID)")
  } else {
    formula_str <- paste0("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  }
  
  m1 = lmer(as.formula(formula_str),data = dat.analyze)
  
  m1.ci <- confint (m1, parm = "audit_c")
  
  dat.out = summary(m1)$coefficients[2,] %>% t()%>% as.data.frame()
  dat.out <- cbind(dat.out, t(m1.ci[1, ]))
  dat.out$xvar = "audit_c"
  dat.out$y.var <-  y.var
  
  if(m ==1){dat.final = matrix(data = NA,nrow = nrow(analysis.framework),ncol = ncol(dat.out)) %>% as.data.frame()}
  
  dat.final[m,] = dat.out
  
}

colnames(dat.final) = colnames(dat.out)
dat.final$pfdr = p.adjust(p=dat.final$`Pr(>|t|)`, method = "fdr")
  # write_xlsx(dat.final, "1_region_audit_noThckCtrl.xlsx")

#########	(primary brain ROI) ~ mAUDIT-C after mean_Thck covariate ##########################

analysis.framework2 <- data.frame(
  y.vars=colnames(base_dat)[c(206:227, 229:230)])

for (m in 1:nrow(analysis.framework2)) {
  
  print(m)
  y.var <- analysis.framework2$y.vars[m]
  
  yvar.to.keep <- c(which(colnames(base_dat) == analysis.framework2$y.vars[m]))
  
  base_dat$y.var = base_dat[[ yvar.to.keep ]]
  
  covars = c("audit_c", "SSAGA_Income", "SSAGA_Educ", "Gender", "mean_Thck",
             "Age1", "Age2", "MZ", "DZ", "Half", "FS_BrainSeg_Vol_No_Vent")
  
  var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", "mean_Thck",
               "y.var", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent", "audit_c")
  
  var.names <-  c("mean_Thck", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", 
                  "SSAGA_Educ", "audit_c", "y.var", "FS_BrainSeg_Vol_No_Vent")
  var.mutate <- c("mean_Thck", "audit_c", "y.var", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent")
  
  dat.analyze2 = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()
  
  # normalization skew, scale, center
  
  dat.analyze2 <- dat.analyze2 %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze2$Age1 = poly(dat.analyze2$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze2$Age2 = poly(dat.analyze2$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  ##################
  
  if (str_ends(y.var, "_Thck")) {
    formula_str <- paste0("y.var ~ ",paste(setdiff(covars, "FS_BrainSeg_Vol_No_Vent"), collapse = " + "), " + (1 | Family_ID)")
  } else {
    formula_str <- paste0("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  }
  
  m2 = lmer(as.formula(formula_str),data = dat.analyze2)
  
  m2.ci <- confint (m2, parm = "audit_c")
  
  dat.out2 = summary(m2)$coefficients[2,] %>% t()%>% as.data.frame()
  dat.out2 <- cbind(dat.out2, t(m2.ci[1, ]))
  dat.out2$xvar = "audit_c"
  dat.out2$y.var <-  y.var
  
  if(m ==1){dat.final2 = matrix(data = NA,nrow = nrow(analysis.framework2),ncol = ncol(dat.out2)) %>% as.data.frame()}
  
  
  dat.final2[m,] = dat.out2
  
}

colnames(dat.final2) = colnames(dat.out2)
dat.final2$pfdr = p.adjust(p=dat.final2$`Pr(>|t|)`, method = "fdr")
  # write_xlsx(dat.final2, "2_region_audit_ThckCtrl.xlsx")

############ mean_Thck ~ drug use vars (shared) ###########################################

analysis.framework3 <-  data.frame(
    x.vars = c("audit_c", "SSAGA_Mj_Ab_Dep", "SSAGA_Alc_D4_Dp_Dx", "onset_tobac", "onset_alc", 
               "onset_illic", "onset_thc", "thc_heavy", "illic_max", "tobac_heavy",
               "thc_user", "illic_user", "tobac_user"), 
    co.vars = c("","thc_user", "audit_c", "tobac_user", "audit_c", "illic_user", "thc_user", "thc_user", "illic_user", 
                "tobac_user", "", "", ""))
  
for (m in 1:nrow(analysis.framework3)) {
  
  print(m)
  x.var <- analysis.framework3$x.vars[m]
  co.var <- analysis.framework3$co.vars[m]
  
  column.to.keep = c(which(colnames(base_dat) == analysis.framework3$x.vars[m]))
  
  base_dat$x.var <- base_dat[[ column.to.keep ]]
  
  if (co.var != "") {
    base_dat$co.var <- base_dat[[ which(colnames(base_dat) == analysis.framework3$co.vars[m]) ]]
  } else {
    base_dat$co.var <- 1
  }
  
  covars = c("x.var", "co.var", "SSAGA_Income", "SSAGA_Educ", "Gender", 
             "Age1", "Age2", "MZ", "DZ", "Half", "FS_BrainSeg_Vol_No_Vent")
  
  var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
               "SSAGA_Income", "SSAGA_Educ", "x.var" , "co.var", "mean_Thck", "FS_BrainSeg_Vol_No_Vent")
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", 
                  "x.var", "mean_Thck", "FS_BrainSeg_Vol_No_Vent")
  var.mutate <- c("x.var", "mean_Thck", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent")
  
  dat.analyze3 = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()
  
  # normalization skew, scale, center
  
  dat.analyze3 <- dat.analyze3 %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze3$Age1 = poly(dat.analyze3$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze3$Age2 = poly(dat.analyze3$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  ##################
  
  if (co.var != "") {
    formula_str <- paste0("mean_Thck ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  } else {
    formula_str <- paste0("mean_Thck ~", paste(setdiff(covars, "co.var"), collapse = " + "), "+ (1 | Family_ID)")
  }
  
  m3 = lmer(as.formula(formula_str),data = dat.analyze3)
  
  m3.ci <- confint (m3, parm = "x.var")
  
  dat.out3 = summary(m3)$coefficients[2,] %>% t()%>% as.data.frame()
  dat.out3 <- cbind(dat.out3, t(m3.ci[1, ]))
  dat.out3$xvar = x.var
  dat.out3$y.var <-  "mean_Thck"
  dat.out3$co.var = co.var
  
  if(m ==1){dat.final3 = matrix(data = NA,nrow = nrow(analysis.framework3),ncol = ncol(dat.out3)) %>% as.data.frame()}
  
  
  dat.final3[m,] = dat.out3
  
}
  
  colnames(dat.final3) = colnames(dat.out3)
  dat.final3$pfdr = p.adjust(p=dat.final3$`Pr(>|t|)`, method = "fdr")
   # write_xlsx(dat.final3, "3_meanThck_shared.xlsx")
  

############ mean_Thck ~ drug use vars (unique) ###########################################

x.vars = c("audit_c", "tobac_user", "onset_alc", "thc_user")

var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
             "SSAGA_Income", "SSAGA_Educ", x.vars ,"mean_Thck", "FS_BrainSeg_Vol_No_Vent")

var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", 
                x.vars, "FS_BrainSeg_Vol_No_Vent", "mean_Thck")
var.mutate <- c( x.vars, "mean_Thck", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent")

dat.analyze4 = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()

# normalization skew, scale, center

dat.analyze4 <- dat.analyze4 %>%
  mutate(across(all_of(var.mutate), winsorize)) %>%
  mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
  mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

dat.analyze4$Age1 = poly(dat.analyze4$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
dat.analyze4$Age2 = poly(dat.analyze4$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)


formula_str <- paste0(
  "mean_Thck ~ ",
  paste(x.vars, collapse = " + "),
  " + SSAGA_Income + SSAGA_Educ + Gender + Age1 + Age2 + MZ + DZ + Half + FS_BrainSeg_Vol_No_Vent + (1 | Family_ID)"
)

m4 = lmer(as.formula(formula_str),data = dat.analyze4)

dat.out4 = summary(m4)$coefficients[2:5,] %>% as.data.frame()%>%tibble::rownames_to_column(var = "xvar")
dat.out4$y.var <-  "mean_Thck"

dat.out4 <- dat.out4%>%
  dplyr::select(xvar, y.var, everything())%>%
  arrange(`Pr(>|t|)`)

 # write_xlsx(dat.out4, "4_meanThck_unique.xlsx")

########### anova (mean thck ~ all, mean thck ~ audit/thc) ###########

x.vars = c("audit_c", "SSAGA_Mj_Ab_Dep", "SSAGA_Alc_D4_Dp_Dx", "onset_tobac", "onset_alc", 
           "onset_illic", "onset_thc", "thc_heavy", "illic_max", "tobac_heavy",
           "thc_user", "illic_user", "tobac_user")

var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
             "SSAGA_Income", "SSAGA_Educ", x.vars ,"mean_Thck", "FS_BrainSeg_Vol_No_Vent")

var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", 
                x.vars, "FS_BrainSeg_Vol_No_Vent", "mean_Thck")
var.mutate <- c( x.vars, "mean_Thck", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent")

dat.analyze4a = base_dat %>%  dplyr::select(all_of(var.keep))

dat.analyze4a[is.na(dat.analyze4a)] <- 0

# normalization skew, scale, center

dat.analyze4a <- dat.analyze4a %>%
  mutate(across(all_of(var.mutate), winsorize)) %>%
  mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
  mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

dat.analyze4a$Age1 = poly(dat.analyze4a$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
dat.analyze4a$Age2 = poly(dat.analyze4a$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)


formula_str4a <- paste0(
  "mean_Thck ~ ",
  paste(x.vars, collapse = " + "),
  " + SSAGA_Income + SSAGA_Educ + Gender + Age1 + Age2 + MZ + DZ + Half + FS_BrainSeg_Vol_No_Vent + (1 | Family_ID)"
)

m4a = lmer(as.formula(formula_str4a),data = dat.analyze4a)

x.vars = c("audit_c", "thc_user")

var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
             "SSAGA_Income", "SSAGA_Educ", x.vars ,"mean_Thck", "FS_BrainSeg_Vol_No_Vent")

var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", 
                x.vars, "FS_BrainSeg_Vol_No_Vent", "mean_Thck")
var.mutate <- c( x.vars, "mean_Thck", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent")

dat.analyze4a = base_dat %>%  dplyr::select(all_of(var.keep))

dat.analyze4a[is.na(dat.analyze4a)] <- 0

# normalization skew, scale, center

dat.analyze4a <- dat.analyze4a %>%
  mutate(across(all_of(var.mutate), winsorize)) %>%
  mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
  mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

dat.analyze4a$Age1 = poly(dat.analyze4a$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
dat.analyze4a$Age2 = poly(dat.analyze4a$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)


formula_str4b <- paste0(
  "mean_Thck ~ ",
  paste(x.vars, collapse = " + "),
  " + SSAGA_Income + SSAGA_Educ + Gender + Age1 + Age2 + MZ + DZ + Half + FS_BrainSeg_Vol_No_Vent + (1 | Family_ID)"
)

m4b = lmer(as.formula(formula_str4b),data = dat.analyze4a)

m4anova <- anova(m4a, m4b)

# write_xlsx(m4anova, "4a_anova.xlsx")

##### GENETIC PREDISPOSITION VERSUS ENVIRONMENTAL EXPOSURE ######
  
################# mean_Thck ~ within- & between- SU vars ################################
  
analysis.framework5 <- data.frame(
    x.vars = c("withinFamAudit", "family_meanAudit", "family_meanTHC", "withinFam_thc"),
    co.vars = c("family_meanAudit", "withinFamAudit", "withinFam_thc","family_meanTHC"))
  
  analysis.framework5 <- analysis.framework5 %>%
    tidyr::expand_grid(
      y.vars = "mean_Thck",
      sample = c("whole", "Half != 1", "MZ == 1 | DZ == 1", "MZ == 1")
    ) %>%
    arrange(x.vars, co.vars)
  
  results <- data.frame()
  
  for (m in 1:nrow(analysis.framework5)) {
    
    sample <- analysis.framework5$sample[m]
    x.var <- analysis.framework5$x.vars[m]   
    y.var <- analysis.framework5$y.vars[m]
    co.vars <- analysis.framework5$co.vars[m]
    
    column.to.keep <- which(colnames(base_dat) == x.var)
    yvar.to.keep <- which(colnames(base_dat) == y.var)
    covars.to.keep <- which(colnames(base_dat) == co.vars)
    
    base_dat$x.var <- base_dat[[ column.to.keep ]]
    base_dat$y.var <- base_dat[[ yvar.to.keep ]]
    base_dat$co.vars <- base_dat[[ covars.to.keep ]]
    
    covars <- c("x.var", "co.vars", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent",
                "Gender", "Age1", "Age2", "MZ", "DZ", "Half")
    
    var.keep <- c("Subject", "Gender", "MZ", "DZ", "Age_in_Yrs", "Family_ID", "Half", "FS_BrainSeg_Vol_No_Vent",
                  "SSAGA_Income", "SSAGA_Educ", "x.var", "y.var", "co.vars", "single")
    
    var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", 
                    "x.var","y.var", "co.vars", "FS_BrainSeg_Vol_No_Vent")
    var.mutate <- c("y.var", "SSAGA_Income", "SSAGA_Educ", "x.var",  "co.vars")
    
    dat.analyze5 <- base_dat %>%
      dplyr::select(all_of(var.keep)) %>%
      {if (sample == "whole") . 
        else if (sample == "Half != 1") filter(., Half != 1, single != 1)
        else if (sample == "MZ == 1 | DZ == 1") filter(., MZ == 1 | DZ == 1)
        else if (sample == "MZ == 1") filter(., MZ == 1)
      } %>%
      na.omit()
    
    dat.analyze5 <- dat.analyze5 %>%
      mutate(across(all_of(var.mutate), winsorize)) %>%
      mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
      mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
    
    dat.analyze5$Age1 = poly(dat.analyze5$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
    dat.analyze5$Age2 = poly(dat.analyze5$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
    
    if (sample == "whole") {
      covars <- covars
    } else if (sample == "Half != 1") {
      covars <- setdiff(covars, "Half")
    } else if (sample == "MZ == 1 | DZ == 1") {
      covars <- setdiff(covars, c("Half", "DZ"))
    } else if (sample == "MZ == 1") {
      covars <- setdiff(covars, c("Half", "DZ", "MZ"))
    }
    
    formula_str <- paste("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
    
    m5 = lmer(as.formula(formula_str),data = dat.analyze5)
    
    summary(m5)
    
    m5.ci <- confint (m5, parm = "x.var")
    
    dat.out5 = summary(m5)$coefficients[2,] %>% t()%>% as.data.frame()
    dat.out5 <- cbind(dat.out5, t(m5.ci[1, ]))
    
    dat.out5$x.var <-  analysis.framework5$x.vars[m]
    dat.out5$y.var <-  analysis.framework5$y.vars[m]
    dat.out5$co.var <- analysis.framework5$co.vars[m]
    dat.out5$sample <- analysis.framework5$sample[m]
    dat.out5$n <- n_distinct(dat.analyze5$Subject)
    
    results <- rbind(results, dat.out5)
    
  }
  
  results <- results%>%
    dplyr::select(y.var, x.var, co.var, sample, everything())
results$pfdr = p.adjust(p=results$`Pr(>|t|)`, method = "fdr")

 # write_xlsx(results, "5_meanThck_wtnbtwn.xlsx")  

###################### mean_Thck ~ drug use vars (unqiue) + drg use control ###########

analysis.framework5a <- data.frame(
  x.vars = c("withinFamAudit", "family_meanTHC", "withinFam_thc"),
  co.vars = c("family_meanAudit", "withinFam_thc","family_meanTHC"),
  y.vars = "mean_Thck")%>%
  tidyr::expand_grid(
    sample = c("whole", "Half != 1"))

analysis.framework5a <- analysis.framework5a%>%
  mutate(co.var1 = ifelse(x.vars %in% "withinFamAudit",
                          "thc_user", "audit_c"))

results5a <- data.frame()

for (m in 1:nrow(analysis.framework5a)) {
  
  sample <- analysis.framework5a$sample[m]
  x.var <- analysis.framework5a$x.vars[m]   
  y.var <- analysis.framework5a$y.vars[m]
  co.vars <- analysis.framework5a$co.vars[m]
  co.var1 <- analysis.framework5a$co.var1[m]
  
  column.to.keep <- which(colnames(base_dat) == x.var)
  yvar.to.keep <- which(colnames(base_dat) == y.var)
  covars.to.keep <- which(colnames(base_dat) == co.vars)
  covar1.to.keep <- which(colnames(base_dat) == co.var1)
  
  base_dat$x.var <- base_dat[[ column.to.keep ]]
  base_dat$y.var <- base_dat[[ yvar.to.keep ]]
  base_dat$co.vars <- base_dat[[ covars.to.keep ]]
  base_dat$co.var1 <- base_dat[[ covar1.to.keep ]]
  
  covars <- c("x.var", "co.vars", "co.var1", "SSAGA_Income", "SSAGA_Educ", 
              "Gender", "Age1", "Age2", "MZ", "DZ", "Half", "FS_BrainSeg_Vol_No_Vent")
  
  var.keep <- c("Subject", "Gender", "MZ", "DZ", "Age_in_Yrs", "Family_ID", "Half", "FS_BrainSeg_Vol_No_Vent",
                "SSAGA_Income", "SSAGA_Educ", "x.var", "y.var", "co.var1","co.vars", "single")
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", 
                  "SSAGA_Educ", "x.var","y.var", "co.vars", "co.var1", "FS_BrainSeg_Vol_No_Vent")
  var.mutate <- c("y.var", "SSAGA_Income", "SSAGA_Educ", "x.var", "co.var1", "co.vars")
  
  dat.analyze5a <- base_dat %>%
    dplyr::select(all_of(var.keep)) %>%
    {if (sample == "whole") . 
      else if (sample == "Half != 1") filter(., Half != 1, single != 1)
    } %>%
    na.omit()
  
  dat.analyze5a <- dat.analyze5a %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze5a$Age1 = poly(dat.analyze5a$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze5a$Age2 = poly(dat.analyze5a$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  if (sample == "whole") {
    covars <- covars
  } else if (sample == "Half != 1") {
    covars <- setdiff(covars, "Half")
  }
  
  formula_str <- paste("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  
  m5a = lmer(as.formula(formula_str),data = dat.analyze5a)
  
  summary(m5a)
  
  m5a.ci <- confint (m5a, parm = "x.var")
  
  dat.out5a = summary(m5a)$coefficients[2,] %>% t()%>% as.data.frame()
  dat.out5a <- cbind(dat.out5a, t(m5a.ci[1, ]))
  
  dat.out5a$x.var <-  analysis.framework5a$x.vars[m]
  dat.out5a$y.var <-  analysis.framework5a$y.vars[m]
  dat.out5a$co.var <- analysis.framework5a$co.vars[m]
  dat.out5a$drug.co.var1 <- analysis.framework5a$co.var1[m]
  dat.out5a$sample <- analysis.framework5a$sample[m]
  dat.out5a$n <- n_distinct(dat.analyze5a$Subject)
  
  results5a <- rbind(results5a, dat.out5a)
  
}

results5a <- results5a%>%
  dplyr::select(y.var, x.var, co.var, drug.co.var1, sample, everything())
results5a$pfdr = p.adjust(p=results5a$`Pr(>|t|)`, method = "fdr")

# write_xlsx(results5a, "5a_meanThck_unique_drugCtrl.xlsx")

################# within & between mean_Thck ~ SU vars ################################

analysis.framework6 <- data.frame(
  x.vars = c("family_meanThck", "withinFam_thck"),
  co.vars = c("withinFam_thck", "family_meanThck"))

analysis.framework6 <- analysis.framework6 %>%
  tidyr::expand_grid(
    y.vars = c("audit_c", "thc_user"),
    sample = c("whole", "Half != 1", "MZ == 1 | DZ == 1", "MZ == 1")
  ) %>%
  arrange(x.vars, co.vars)

results2 <- data.frame()

for (m in 1:nrow(analysis.framework6)) {
  
  sample <- analysis.framework6$sample[m]
  x.var <- analysis.framework6$x.vars[m]   
  y.var <- analysis.framework6$y.vars[m]
  co.vars <- analysis.framework6$co.vars[m]
  
  column.to.keep <- which(colnames(base_dat) == x.var)
  yvar.to.keep <- which(colnames(base_dat) == y.var)
  covars.to.keep <- which(colnames(base_dat) == co.vars)
  
  base_dat$x.var <- base_dat[[ column.to.keep ]]
  base_dat$y.var <- base_dat[[ yvar.to.keep ]]
  base_dat$co.vars <- base_dat[[ covars.to.keep ]]
  
  covars <- c("x.var", "co.vars", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent",
              "Gender", "Age1", "Age2", "MZ", "DZ", "Half")
  
  var.keep <- c("Subject", "Gender", "MZ", "DZ", "Age_in_Yrs", "Family_ID", "Half", "FS_BrainSeg_Vol_No_Vent", 
                "SSAGA_Income", "SSAGA_Educ", "x.var", "y.var", "co.vars", "single")
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", 
                  "x.var","y.var", "co.vars", "FS_BrainSeg_Vol_No_Vent")
  var.mutate <- c("y.var", "SSAGA_Income", "SSAGA_Educ", "x.var",  "co.vars", "FS_BrainSeg_Vol_No_Vent")
  
  dat.analyze6 <- base_dat %>%
    dplyr::select(all_of(var.keep)) %>%
    {if (sample == "whole") . 
      else if (sample == "Half != 1") filter(., Half != 1, single != 1)
      else if (sample == "MZ == 1 | DZ == 1") filter(., MZ == 1 | DZ == 1)
      else if (sample == "MZ == 1") filter(., MZ == 1)
    } %>%
    na.omit()
  
  dat.analyze6 <- dat.analyze6 %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze6$Age1 = poly(dat.analyze6$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze6$Age2 = poly(dat.analyze6$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  if (sample == "whole") {
    covars <- covars
  } else if (sample == "Half != 1") {
    covars <- setdiff(covars, "Half")
  } else if (sample == "MZ == 1 | DZ == 1") {
    covars <- setdiff(covars, c("Half", "DZ"))
  } else if (sample == "MZ == 1") {
    covars <- setdiff(covars, c("Half", "DZ", "MZ"))
  }
  
  formula_str <- paste("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  
  m6 = lmer(as.formula(formula_str),data = dat.analyze6)
  
  summary(m6)
  
  m6.ci <- confint (m6, parm = "x.var")
  
  dat.out6 = summary(m6)$coefficients[2,] %>% t()%>% as.data.frame()
  dat.out6 <- cbind(dat.out6, t(m6.ci[1, ]))
  
  dat.out6$x.var <-  analysis.framework6$x.vars[m]
  dat.out6$y.var <-  analysis.framework6$y.vars[m]
  dat.out6$co.var <- analysis.framework6$co.vars[m]
  dat.out6$sample <- analysis.framework6$sample[m]
  dat.out6$n <- n_distinct(dat.analyze6$Subject)
  
  results2 <- rbind(results2, dat.out6)
  
}

results2 <- results2%>%
  dplyr::select(y.var, x.var, co.var, sample, everything())
results2$pfdr = p.adjust(p=results2$`Pr(>|t|)`, method = "fdr")

  # write_xlsx(results2, "6_substanceUse_wtnbtwnThck.xlsx")  

############# SOLAR-Eclipse genetic vs environmental analyses #######

base_dat$Age1 = poly(base_dat$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
base_dat$Age2 = poly(base_dat$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)

analysis.framework7 <-  data.frame(
  x.var = c("audit_c", "thc_user"), 
  y.var = c("mean_Thck"))

dat_final7 <- data.frame()

for (m in 1:nrow(analysis.framework7))  {
  
  print(m)
  x.var <- analysis.framework7$x.var[m]
  y.var <- analysis.framework7$y.var[m]
  
  ############## pedigree
  
  dat.7 <- data.frame(
    famid = base_dat$Family_ID,
    id = base_dat$Subject,
    fa = base_dat$Father_ID,
    mo = base_dat$Mother_ID,
    mz = base_dat$MZ,
    dz = base_dat$DZ,
    half = base_dat$Half,
    sex = base_dat$Gender,
    Age = base_dat$Age_in_Yrs, 
    Age1 = base_dat$Age1, 
    Age2 = base_dat$Age2,
    half = base_dat$Half,
    sid = base_dat$Subject)
  
  dat.7[x.var] <- base_dat[x.var]
  dat.7[y.var] <- base_dat[y.var]
  
  #remove singletons
  dat.7 <- dat.7 %>%
    group_by(famid) %>%
    filter(n() > 1) %>%
    ungroup()
  
  #remove half sibs
  dat.7 <- dat.7 %>%
    filter(half != 1)
  
  #mztwin column
  mz_twins <- dat.7 %>% filter(mz == 1)
  
  unique_fams <- mz_twins%>%
    distinct(famid)%>%
    mutate(mztwin = 1:n())
  
  dat.7 <- dat.7%>%
    left_join(unique_fams,by = "famid")
  
  dat.7 <- dat.7 %>%
    mutate(mztwin = ifelse(mz == 0, 0, mztwin))
  
  #hhid column 
  unique_famID <- dat.7 %>%
    distinct(famid) %>%   
    mutate(hhid = 1:n())
  
  dat.7 <- dat.7%>% merge(unique_famID, by = "famid")
  
  dat.7 <- dat.7 %>% 
    group_by(famid, id) %>% 
    slice(1) %>%  
    ungroup()
  
  any(duplicated(dat.7 %>% dplyr::select(famid, id)))
  
  dat.7 <- as.data.frame(dat.7)
  
  dat.7 <- dat.7 %>%
    mutate(famid = as.numeric(factor(famid)))
  
  dat.7 <- dat.7%>%
    arrange(mztwin)%>%
    arrange(hhid)%>%
    mutate(id=1:length(id))
  
  ############## transformations
  
  var.names <-  c("mz", "Age",  x.var, y.var)
  var.mutate <- c(x.var, y.var)
  
  dat.analyze = dat.7 
  
  winsorize = function(x,q=3){
    
    mean_x = mean(x,na.rm = T)
    sd_x = sd(x,na.rm = T)
    top_q = mean_x + q*sd_x
    bottom_q = mean_x - q*sd_x
    
    x[x>top_q] = top_q
    x[x<bottom_q] = bottom_q
    
    return(x)
    
  }
  
  dat.analyze <- dat.analyze %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  ################## MODS
  
  form1 <- as.formula(paste0(x.var, " ~ Age1 + Age2 + sex"))
  form2 <- as.formula(paste0(y.var, " ~ Age1 + Age2 + sex"))
  form3 <- as.formula(paste0(x.var, " + ", y.var, " ~ Age1 + Age2 + sex"))
  
  m1 <- solarPolygenic(
    form1,
    data = dat.analyze,
    covtest = TRUE,
    household = FALSE,
    dir = paste0("/scratch/g/dbaranger/HCP_analyses/solar/loop_test/", x.var, "_res"))
  
  m1.2 <- solarPolygenic(
    form2,
    data = dat.analyze,
    covtest = TRUE,
    household = FALSE,
    dir = paste0("/scratch/g/dbaranger/HCP_analyses/solar/loop_test/", y.var, "_res"))
  
  m2 <-  solarPolygenic(
    form3,
    data = dat.analyze,
    covtest = FALSE,
    household = FALSE,
    dir = paste0("/scratch/g/dbaranger/HCP_analyses/solar/loop_test/", y.var, "_", x.var, "_res"),
    polygenic.options = "-testrhop -testrhog -testrhoe")
  
  ################ EXTRACT DATA 
  
  #heritability 
  
  h2r_out <- m1$vcf[1,]
  colnames(h2r_out)[colnames(h2r_out) == "Var"] <- "Estimate"
  colnames(h2r_out) <- paste("xh2r", colnames(h2r_out), sep = "_")
  e2_out <- m1$vcf[2,]
  colnames(e2_out)[colnames(e2_out) == "Var"] <- "Estimate"
  colnames(e2_out) <- paste("xe2", colnames(e2_out), sep = "_")
  hert <- cbind(h2r_out, e2_out)
  
  h2r_out <- m1.2$vcf[1,]
  colnames(h2r_out)[colnames(h2r_out) == "Var"] <- "Estimate"
  colnames(h2r_out) <- paste("yh2r", colnames(h2r_out), sep = "_")
  e2_out <- m1.2$vcf[2,]
  colnames(e2_out)[colnames(e2_out) == "Var"] <- "Estimate"
  colnames(e2_out) <- paste("ye2", colnames(e2_out), sep = "_")
  hert2 <- cbind(h2r_out, e2_out)
  
  hert_out <- cbind(hert, hert2)
  
  #rhoe and rhog
  rhog_out <- m2$vcf[m2$vcf$varcomp == "rhog", ]
  rhog_out$pvalZ <- m2$lf[4, 5]
  colnames(rhog_out) <- paste("rhog", colnames(rhog_out), sep = "_")
  
  rhoe_out <- m2$vcf[m2$vcf$varcomp == "rhoe", ]
  rhoe_out$pvalZ <- m2$lf[3, 5]
  colnames(rhoe_out) <- paste("rhoe", colnames(rhoe_out), sep = "_")
  
  combined <- cbind(rhog_out, rhoe_out)
  combined$x.var = x.var
  combined$y.var = y.var
  combined <- combined %>% dplyr::select(x.var, y.var, everything())
  
  new <- cbind(combined, hert_out)
  
  dat_final7 <- rbind(dat_final7, new)
  
}

dat_final7 <- dat_final7 %>%
  dplyr::select(-matches("_varcomp$"), -matches("e2_pval"))

  # write_xlsx(dat_final7, "7_solar_analyses.xlsx")

# with alternative predictor as a control 

analysis.framework7a <-  data.frame(
  x.var = "thc_user", 
  co.var = "audit_c",
  y.var = "mean_Thck")

dat_final7a <- data.frame()

for (m in 1:nrow(analysis.framework7a))  {
  
  print(m)
  x.var <- analysis.framework7a$x.var[m]
  y.var <- analysis.framework7a$y.var[m]
  co.var <- analysis.framework7a$co.var[m]
  
  ############## pedigree
  
  dat.7 <- data.frame(
    famid = base_dat$Family_ID,
    id = base_dat$Subject,
    fa = base_dat$Father_ID,
    mo = base_dat$Mother_ID,
    mz = base_dat$MZ,
    dz = base_dat$DZ,
    half = base_dat$Half,
    sex = base_dat$Gender,
    Age = base_dat$Age_in_Yrs, 
    Age1 = base_dat$Age1, 
    Age2 = base_dat$Age2,
    half = base_dat$Half,
    sid = base_dat$Subject)
  
  dat.7[x.var] <- base_dat[x.var]
  dat.7[y.var] <- base_dat[y.var]
  dat.7[co.var] <- base_dat[co.var]
  
  #remove singletons
  dat.7 <- dat.7 %>%
    group_by(famid) %>%
    filter(n() > 1) %>%
    ungroup()
  
  #remove half sibs
  dat.7 <- dat.7 %>%
    filter(half != 1)
  
  #mztwin column
  mz_twins <- dat.7 %>% filter(mz == 1)
  
  unique_fams <- mz_twins%>%
    distinct(famid)%>%
    mutate(mztwin = 1:n())
  
  dat.7 <- dat.7%>%
    left_join(unique_fams,by = "famid")
  
  dat.7 <- dat.7 %>%
    mutate(mztwin = ifelse(mz == 0, 0, mztwin))
  
  #hhid column 
  unique_famID <- dat.7 %>%
    distinct(famid) %>%   
    mutate(hhid = 1:n())
  
  dat.7 <- dat.7%>% merge(unique_famID, by = "famid")
  
  dat.7 <- dat.7 %>% 
    group_by(famid, id) %>% 
    slice(1) %>%  
    ungroup()
  
  any(duplicated(dat.7 %>% dplyr::select(famid, id)))
  
  dat.7 <- as.data.frame(dat.7)
  
  dat.7 <- dat.7 %>%
    mutate(famid = as.numeric(factor(famid)))
  
  dat.7 <- dat.7%>%
    arrange(mztwin)%>%
    arrange(hhid)%>%
    mutate(id=1:length(id))
  
  ############## transformations
  
  var.names <-  c("mz", "Age",  x.var, y.var, co.var)
  var.mutate <- c(x.var, y.var, co.var)
  
  dat.analyze = dat.7 
  
  winsorize = function(x,q=3){
    
    mean_x = mean(x,na.rm = T)
    sd_x = sd(x,na.rm = T)
    top_q = mean_x + q*sd_x
    bottom_q = mean_x - q*sd_x
    
    x[x>top_q] = top_q
    x[x<bottom_q] = bottom_q
    
    return(x)
    
  }
  
  dat.analyze <- dat.analyze %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  ################## MODS
  
  form1 <- as.formula(paste0(x.var, " ~ Age1 + Age2 + sex"))
  form2 <- as.formula(paste0(y.var, " ~ Age1 + Age2 + sex"))
  form2.1 <- as.formula(paste0(co.var, " ~ Age1 + Age2 + sex"))
  form3 <- as.formula(paste0(x.var, " + ", y.var, " ~", co.var, "+ Age1 + Age2 + sex"))
  
  m1 <- solarPolygenic(
    form1,
    data = dat.analyze,
    covtest = TRUE,
    household = FALSE,
    dir = paste0("/scratch/g/dbaranger/HCP_analyses/solar/loop_test/", x.var, "_res"))
  
  m1.2 <- solarPolygenic(
    form2,
    data = dat.analyze,
    covtest = TRUE,
    household = FALSE,
    dir = paste0("/scratch/g/dbaranger/HCP_analyses/solar/loop_test/", y.var, "_res"))
  
  m1.5 <- solarPolygenic(
    form2.1,
    data = dat.analyze,
    covtest = TRUE,
    household = FALSE,
    dir = paste0("/scratch/g/dbaranger/HCP_analyses/solar/loop_test/", co.var, "_res"))
  
  m2 <-  solarPolygenic(
    form3,
    data = dat.analyze,
    covtest = FALSE,
    household = FALSE,
    dir = paste0("/scratch/g/dbaranger/HCP_analyses/solar/loop_test/", y.var, "_", x.var, "_", co.var, "_res"),
    polygenic.options = "-testrhop -testrhog -testrhoe")
  
  ################ EXTRACT DATA 
  
  #heritability 
  
  h2r_out <- m1$vcf[1,]
  colnames(h2r_out)[colnames(h2r_out) == "Var"] <- "Estimate"
  colnames(h2r_out) <- paste("xh2r", colnames(h2r_out), sep = "_")
  e2_out <- m1$vcf[2,]
  colnames(e2_out)[colnames(e2_out) == "Var"] <- "Estimate"
  colnames(e2_out) <- paste("xe2", colnames(e2_out), sep = "_")
  hert <- cbind(h2r_out, e2_out)
  
  h2r_out <- m1.2$vcf[1,]
  colnames(h2r_out)[colnames(h2r_out) == "Var"] <- "Estimate"
  colnames(h2r_out) <- paste("yh2r", colnames(h2r_out), sep = "_")
  e2_out <- m1.2$vcf[2,]
  colnames(e2_out)[colnames(e2_out) == "Var"] <- "Estimate"
  colnames(e2_out) <- paste("ye2", colnames(e2_out), sep = "_")
  hert2 <- cbind(h2r_out, e2_out)
  
  hert_out <- cbind(hert, hert2)
  
  #rhoe and rhog
  rhog_out <- m2$vcf[m2$vcf$varcomp == "rhog", ]
  rhog_out$pvalZ <- m2$lf[4, 5]
  colnames(rhog_out) <- paste("rhog", colnames(rhog_out), sep = "_")
  
  rhoe_out <- m2$vcf[m2$vcf$varcomp == "rhoe", ]
  rhoe_out$pvalZ <- m2$lf[3, 5]
  colnames(rhoe_out) <- paste("rhoe", colnames(rhoe_out), sep = "_")
  
  combined <- cbind(rhog_out, rhoe_out)
  combined$x.var = x.var
  combined$y.var = y.var
  combined$co.var = co.var
  combined <- combined %>% dplyr::select(x.var, y.var, co.var, everything())
  
  new <- cbind(combined, hert_out)
  
  dat_final7a <- rbind(dat_final7a, new)
  
}

dat_final7a <- dat_final7a %>%
  dplyr::select(-matches("_varcomp$"), -matches("e2_pval"))

write_xlsx(dat_final7a, "7a_solar_analyses_drugCovar.xlsx")

############ MEDIATIONS & INTERACTIONS ###################

############# mediation- step 1, associations with mean_Thck ###########################

analysis.framework8 <- expand_grid(
  x.vars = c(colnames(base_dat[33:117]), colnames(base_dat[246:349]), colnames(base_dat[521:647])),
  y.vars = "mean_Thck")

results8 <- data.frame()
errors <- c()

for (m in 1:nrow(analysis.framework8)) {
  
  x.var <- analysis.framework8$x.vars[m]   
  y.var <- analysis.framework8$y.vars[m]
  
  tryCatch({
    
    column.to.keep <- which(colnames(base_dat) == x.var)
    yvar.to.keep <- which(colnames(base_dat) == y.var)
    
    base_dat$x.var <- base_dat[[column.to.keep]]
    base_dat$y.var <- base_dat[[yvar.to.keep]]
    
    covars <- c("x.var", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent",
                "Gender", "Age1", "Age2", "MZ", "DZ", "Half")
    
    var.keep <- c("Subject", "Gender", "MZ", "DZ", "Age_in_Yrs", "Family_ID", "Half",
                  "SSAGA_Income", "SSAGA_Educ", "x.var", "y.var", "FS_BrainSeg_Vol_No_Vent")
    
    var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income", "SSAGA_Educ", 
                    "x.var","y.var", "FS_BrainSeg_Vol_No_Vent")
    var.mutate <- c("y.var", "SSAGA_Income", "SSAGA_Educ", "x.var")
    
    dat.analyze8 <- base_dat %>%
      dplyr::select(all_of(var.keep)) %>%
      na.omit() 
    
    dat.analyze8 <- dat.analyze8 %>%
      mutate(across(all_of(var.mutate), winsorize)) %>%
      mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
      mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
    
    dat.analyze8$Age1 = poly(dat.analyze8$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
    dat.analyze8$Age2 = poly(dat.analyze8$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
    
    formula_str <- paste("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
    
    m8 = lmer(as.formula(formula_str), data = dat.analyze8)
    m8.ci <- confint(m8, parm = "x.var")
    
    dat.out8 = summary(m8)$coefficients[2,] %>% t() %>% as.data.frame()
    dat.out8 <- cbind(dat.out8, t(m8.ci[1, ]))
    
    dat.out8$x.var <- x.var
    dat.out8$y.var <- y.var
    dat.out8$n <- n_distinct(dat.analyze8$Subject)
    
    results8 <- rbind(results8, dat.out8)
    
  }, error = function(e) {
    errors <<- c(errors, paste(x.var, y.var, sep = " ~ "))
  })
  
}

errors  # catches vars that are incompatible with analysis e.g., character values 
results8 <- results8%>%
  dplyr::select(y.var, x.var, everything())
results8$pfdr = p.adjust(p=results8$`Pr(>|t|)`, method = "fdr" )

# write_xlsx(results8, "8_potentialMediators_meanThck.xlsx")

############## mediation- step 2, mean_Thck associations + audit_c and thc_user ########

significant <- read_xlsx("brainstr_su/results/primary/8_potentialMediators_meanThck.xlsx")%>%
  filter(`Pr(>|t|)`<0.05)

analysis.framework9 <-  expand_grid(
  x.vars = c(unique(significant$x.var)), 
  y.vars = c("audit_c", "thc_user"))

for (m in 1:nrow(analysis.framework9)) {
  
  print(m)
  x.var <- analysis.framework9$x.vars[m]
  y.var <- analysis.framework9$y.vars[m]
  
  column.to.keep = c(which(colnames(base_dat) == analysis.framework9$x.vars[m]))
  yvar.to.keep <- c(which(colnames(base_dat) == analysis.framework9$y.vars[m]))
  
  base_dat$x.var =  base_dat[[ column.to.keep]]
  base_dat$y.var = base_dat[[ yvar.to.keep ]]
  
  covars <- c("x.var", "SSAGA_Income", "SSAGA_Educ",  "FS_BrainSeg_Vol_No_Vent",
              "Gender", "Age1", "Age2", "MZ", "DZ", "Half")
  
  var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
               "y.var", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent", "x.var")
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income",  "FS_BrainSeg_Vol_No_Vent",
                  "SSAGA_Educ", "x.var", "y.var")
  var.mutate <- c("y.var", "x.var", "SSAGA_Income", "SSAGA_Educ")
  
  dat.analyze9 = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()
  
  dat.analyze9 <- dat.analyze9 %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze9$Age1 = poly(dat.analyze9$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze9$Age2 = poly(dat.analyze9$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  formula_str <- paste("y.var ~", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  
  m9 = lmer(as.formula(formula_str),data = dat.analyze9)
  
  m9.ci <- confint (m9, parm = "x.var")
  
  dat.out9 = summary(m9)$coefficients[2,] %>% t()%>% as.data.frame()
  dat.out9 <- cbind(dat.out9, t(m9.ci[1, ]))
  dat.out9$xvar = x.var
  dat.out9$y.var <-  y.var
  
  if(m ==1){dat.final9 = matrix(data = NA,nrow = nrow(analysis.framework9),ncol = ncol(dat.out9)) %>% as.data.frame()}
  
  dat.final9[m,] = dat.out9
  
}

colnames(dat.final9) = colnames(dat.out9)
dat.final9$pfdr = p.adjust(p=dat.final9$`Pr(>|t|)`, method = "fdr" )

dat.final9 <- dat.final9 %>%
  dplyr::select(y.var, xvar, dplyr::everything()) %>%
  arrange(`Pr(>|t|)`) %>%
  filter(!str_ends(as.character(xvar), "_T"))

  # write_xlsx(dat.final9, "9_potentialMediators_allVars.xlsx")

############### mediation- step 3, mediation test ##############################

base_dat <- base_dat%>%
  group_by(Family_ID)%>%
  mutate(family_meanrule = mean(ASR_Rule_Raw))%>%
  ungroup(Family_ID)

base_dat <- base_dat%>%
  mutate(withinFam_rule = ASR_Rule_Raw-family_meanrule)

detach("package:lmerTest", unload = TRUE)

# ASR RULE BREAKING 

var.keep11 = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", 
              "SSAGA_Income", "SSAGA_Educ", "withinFamAudit", "withinFam_thck", "withinFam_rule")

var.names11 <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", 
                 "SSAGA_Income", "SSAGA_Educ", "withinFamAudit",
                 "withinFam_thck", "withinFam_rule")
var.mutate11 <- c("SSAGA_Income", "SSAGA_Educ", "withinFamAudit",
                 "withinFam_thck", "withinFam_rule")

dat.analyze11 = base_dat %>%  dplyr::select(all_of(var.keep11)) %>% na.omit()%>%
  filter(Half != 1)

dat.analyze11 <- dat.analyze11 %>%
  mutate(across(all_of(var.mutate11), winsorize)) %>%
  mutate(across(all_of(var.mutate11), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
  mutate(across(all_of(var.names11), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

dat.analyze11$Age1 = poly(dat.analyze11$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
dat.analyze11$Age2 = poly(dat.analyze11$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)

med.fit11 = lmer(withinFam_rule ~ withinFamAudit + SSAGA_Income + SSAGA_Educ + Gender + Age1 + 
                  Age2 + MZ + DZ + (1 | Family_ID), data = dat.analyze11)

summary(med.fit11)

out.fit11 = lmer(withinFam_thck ~ withinFam_rule + withinFamAudit + SSAGA_Income + SSAGA_Educ + Gender + Age1 + 
                  Age2 + MZ + DZ + (1 | Family_ID), data = dat.analyze11)

summary(out.fit11)

med.out11 <- mediate(
  model.m = med.fit11, 
  model.y = out.fit11, 
  treat   = "withinFamAudit",      
  mediator = "withinFam_rule",             
  sims=10000)

out.sum.any11 = c(med.out11$d.avg, med.out11$d.avg.ci[1],med.out11$d.avg.ci[2],med.out11$d.avg.p,
                 med.out11$z.avg, med.out11$z.avg.ci[1],med.out11$z.avg.ci[2],med.out11$z.avg.p,
                 med.out11$tau.coef,med.out11$tau.ci[1],med.out11$tau.ci[2],med.out11$tau.p,
                 med.out11$n.avg, med.out11$n.avg.ci[1],med.out11$n.avg.ci[2],med.out11$n.avg.p) %>% t() %>% as.data.frame()
col.st11 = expand.grid(c =c("Est","L","U","p") ,b = c("ACME","ADE","Tot","Prop"))
colnames(out.sum.any11) = apply(col.st11,1,function(X){paste(X,collapse = "_",sep="")})
out.sum.any11

out.sum.any11$xvar = "audit_c"
out.sum.any11$yvar = "mean_Thck"
out.sum.any11$mediator = "ASR_Rule_Raw"

out.sum.any11 <- out.sum.any11%>%
  dplyr::select(yvar, mediator, xvar, everything())

  # write_xlsx(out.sum.any11, "10_mediation.xlsx")

################# interactions (drug, age, sex, birth control) #######################

# reopen all packages 

library(lme4)
library(lmerTest)

base_dat <- base_dat %>%
  mutate(birthc = case_when(
    is.na(Menstrual_UsingBirthControl) ~ 0,
    TRUE ~ 1))

base_dat$Age1 = poly(base_dat$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
base_dat$Age2 = poly(base_dat$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)

analysis.framework12 <- data.frame(
  x.var.one = c("thc_user", "Gender", "Gender", "birthc", "birthc"),
  x.var.two = c("audit_c", "audit_c", "thc_user", "Age1", "Age2"),
  y.vars = "mean_Thck")

results12 <- data.frame()

for (m in 1:nrow(analysis.framework12)) {
  
  x.var.one <- analysis.framework12$x.var.one[m]   
  x.var.two <- analysis.framework12$x.var.two[m]
  y.var <- analysis.framework12$y.vars[m]
  
  xone.to.keep <- which(colnames(base_dat) == x.var.one)
  yvar.to.keep <- which(colnames(base_dat) == y.var)
  xtwo.to.keep <- which(colnames(base_dat) == x.var.two)
  
  base_dat$x.var <- base_dat[[ xone.to.keep ]]
  base_dat$y.var <- base_dat[[ yvar.to.keep ]]
  base_dat$x.var.two <- base_dat[[ xtwo.to.keep ]]
  
  covars <- c("SSAGA_Income", "SSAGA_Educ",  "FS_BrainSeg_Vol_No_Vent",
              "Gender", "Age1", "Age2", "MZ", "DZ", "Half")
  
  var.keep <- c("Subject", "Gender", "MZ", "DZ", "Age_in_Yrs", "Family_ID", "Half", "FS_BrainSeg_Vol_No_Vent",
                "SSAGA_Income", "SSAGA_Educ", x.var.one, y.var, x.var.two)
  
  var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "SSAGA_Income",  "FS_BrainSeg_Vol_No_Vent",
                  "SSAGA_Educ", x.var.one, y.var, x.var.two)
  var.mutate <- c(y.var, "SSAGA_Income", "SSAGA_Educ", x.var.one, y.var, x.var.two)
  
  dat.analyze12 <- base_dat %>%
    dplyr::select(all_of(var.keep))%>%
    distinct()%>%
    drop_na()
  
  dat.analyze12 <- dat.analyze12 %>%
    mutate(across(all_of(var.mutate), winsorize)) %>%
    mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
    mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))
  
  dat.analyze12$Age1 = poly(dat.analyze12$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
  dat.analyze12$Age2 = poly(dat.analyze12$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)
  
  formula_str <- paste(y.var, "~", x.var.one, "*", x.var.two, "+", paste(covars, collapse = " + "), "+ (1 | Family_ID)")
  
  m12 = lmer(as.formula(formula_str),data = dat.analyze12)
  
  summary(m12)
  
  m12.ci <- confint(m12)
  interacti <- paste(x.var.one, ":", x.var.two, sep = "")
  
  dat.out12 = summary(m12)$coefficients[interacti, ] %>% t()%>% as.data.frame()
  dat.out12 <- cbind(dat.out12, t(m12.ci[interacti, ]))
  
  dat.out12$x.var.one <-  analysis.framework12$x.var.one[m]
  dat.out12$x.var.two <-  analysis.framework12$x.var.two[m]
  dat.out12$y.var <-  analysis.framework12$y.vars[m]
  dat.out12$n <- n_distinct(dat.analyze12$Subject)
  
  results12 <- rbind(results12, dat.out12)
  
}

results12$pfdr <-  p.adjust(p=results12$`Pr(>|t|)`, method = "fdr" )

   # write_xlsx(results12, "11_interactions.xlsx")

#######################

base_dat <- base_dat%>%
  mutate(THC = if_else(THC == TRUE, 1, 0))

var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age_in_Yrs", "Family_ID", "mean_Thck", 
             "SSAGA_Income", "SSAGA_Educ", "thc_user", "FS_BrainSeg_Vol_No_Vent", "THC")

var.names <-  c("Gender", "MZ","DZ", "Half","Age_in_Yrs", "THC", "SSAGA_Income", "SSAGA_Educ", "mean_Thck", "FS_BrainSeg_Vol_No_Vent", "thc_user")
var.mutate <- c("mean_Thck", "SSAGA_Income", "SSAGA_Educ", "FS_BrainSeg_Vol_No_Vent","thc_user", "THC")

dat.analyze = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()

# normalization skew, scale, center

dat.analyze <- dat.analyze %>%
  mutate(across(all_of(var.mutate), winsorize)) %>%
  mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
  mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

dat.analyze$Age1 = poly(dat.analyze$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
dat.analyze$Age2 = poly(dat.analyze$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)

##################

m1 = lmerTest::lmer(mean_Thck ~ thc_user + SSAGA_Income + SSAGA_Educ + Gender + Age1 + 
                      Age2 + MZ + DZ + Half + THC + (1 | Family_ID), data = dat.analyze)

summary(m1)
