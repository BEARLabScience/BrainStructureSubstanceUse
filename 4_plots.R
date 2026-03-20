library(readxl)
library(tidyverse)
library(tidyr)
library(readr)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(ggpointdensity)
library(patchwork)
library(grid)
library(readxl)
library(janitor)
library(knitr)
library(forestplot)
library(ggtext)
library(ggh4x)
library(grid)
library(viridisLite)
library(viridis)
library(scales)
library(paletteer)
library(colorspace)
library(systemfonts)
library(showtext)
library(UpSetR)
library(ComplexUpset)
library(grid)

base_dat <- read_xlsx("Daniella/brainstr_su/datasets/base_dat.xlsx")

############# PRIMARY MATERIALS PLOTS ##################

####### SU endorsement ###############

druguse_recoded <- base_dat %>%
  mutate(
    Tobacco_use = factor(case_when(
      SSAGA_TB_Smoking_History == 0 ~ "Never",
      SSAGA_TB_Smoking_History == 1 ~ "1-19",
      SSAGA_TB_Smoking_History == 2 ~ "20-99",
      SSAGA_TB_Smoking_History == 3 ~ "Regular Smoker",
      TRUE ~ NA_character_
    ), levels = c("Never", "1-19", "20-99", "Regular Smoker")),
    Illicits_use = factor(case_when(
      SSAGA_Times_Used_Illicits == 0 ~ "Never",
      SSAGA_Times_Used_Illicits == 1 ~ "1-2",
      SSAGA_Times_Used_Illicits == 2 ~ "3-10",
      SSAGA_Times_Used_Illicits == 3 ~ "11-25",
      SSAGA_Times_Used_Illicits == 4 ~ "26-100",
      SSAGA_Times_Used_Illicits == 5 ~ ">100",
      TRUE ~ NA_character_
    ), levels = c("Never","1-2","3-10","11-25","26-100",">100")),
    Marijuana_use = factor(case_when(
      SSAGA_Mj_Times_Used == 0 ~ "Never",
      SSAGA_Mj_Times_Used == 1 ~ "1-5",
      SSAGA_Mj_Times_Used == 2 ~ "6-10",
      SSAGA_Mj_Times_Used == 3 ~ "11-50",
      SSAGA_Mj_Times_Used == 4 ~ "51-1000",
      SSAGA_Mj_Times_Used == 5 ~ ">1000",
      TRUE ~ NA_character_
    ), levels = c("Never","1-5","6-10","11-50","51-1000",">1000"))) %>%
  filter(!is.na(Tobacco_use) & !is.na(Illicits_use) & !is.na(Marijuana_use))

tobacco_counts <- druguse_recoded %>%
  count(Tobacco_use)

illicits_counts <- druguse_recoded %>%
  count(Illicits_use)

marijuana_counts <- druguse_recoded %>%
  count(Marijuana_use)

auditc_counts <- druguse_recoded %>%
  count(audit_c)%>%
  filter(!is.na(audit_c))%>%
  mutate(sev = case_when(
    audit_c == 0 ~ "Low", 
    audit_c == 1 ~ "Low", 
    audit_c == 2 ~ "Low", 
    audit_c == 3 ~ "Low", 
    audit_c == 4 ~ "Moderate", 
    audit_c == 5 ~ "Moderate", 
    audit_c == 6 ~ "High", 
    audit_c == 7 ~ "High", 
    TRUE ~ "Severe"),
    sev = factor(
      sev,
      levels = c("Low", "Moderate", "High", "Severe"),
      ordered = TRUE))

max_count <- max(c(tobacco_counts$n, illicits_counts$n, marijuana_counts$n, auditc_counts$n))

p8 <- ggplot(tobacco_counts, aes(x = Tobacco_use, y = n, fill = Tobacco_use)) +
  geom_col() +
  scale_fill_manual(values = colorRampPalette(c("#bdd7e7","#08306b"))(nrow(tobacco_counts))) +
  labs(title = "Times Used Tobacco", x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0, max_count)) +
  theme_classic(base_size = 6) +
  theme(plot.title = element_text(size = 7),
        legend.position = "none")
p8

p9 <- ggplot(illicits_counts, aes(x = Illicits_use, y = n, fill = Illicits_use)) +
  geom_col() +
  scale_fill_manual(values = colorRampPalette(c("#fde0dd","#c51b8a"))(nrow(illicits_counts))) +
  labs(title = "Times Used Illicit Drugs", x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0, max_count)) +
  theme_classic(base_size = 6) +
  theme(legend.position = "none",
        plot.title = element_text(size = 7))
p9

p10 <- ggplot(marijuana_counts, aes(x = Marijuana_use, y = n, fill = Marijuana_use)) +
  geom_col() +
  scale_fill_manual(values = colorRampPalette(c("#A3D492","#3f5938"))(nrow(marijuana_counts)))+
  labs(title = "Times Used Marijuana", x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0, max_count)) +
  theme_classic(base_size = 6) +
  theme(legend.position = "none",
        plot.title = element_text(size = 7))

p10

## note: p11 is created here but used in the "upset plots- poly use & heavy use" portion 
p11 <- ggplot(auditc_counts, aes(x = audit_c, y = n, fill = sev)) +
  labs(x = "mAUDIT-C: Hazardous Alcohol Use", x = NULL, y = NULL, tag = "A") +
  geom_col(alpha = 0.85, width = 0.9) +
  scale_fill_manual(values = c("Low" = "#33A65CFF", "Moderate" = "#F8B620FF", "High" = "#1BA3C6FF", "Severe" = "violetred3"))+
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12))+
  facet_grid(. ~ sev, space = "free", scales = "free_x", switch = "x") +
  theme_classic(base_size = 6) +
  theme(legend.position = "none",
        axis.line.y = element_line(linewidth = 0.15, color = "black"),
        axis.line.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x= element_text(size = 7),
        strip.placement = "inside",
        strip.text = element_text(hjust = 0.5),
        strip.background = element_rect(linewidth = 0.25, color = "black"),
        panel.spacing =  unit(0, "lines"),
        plot.tag = element_text(size = 10, face = "bold", hjust = -1, vjust = 1.5))+
  geom_text(aes(label = n, y = n + 3), size = 1.75,
            position = position_dodge(0.5), vjust = 0)+
  scale_y_continuous(expand = expansion(mult = 0, add = 1))+
  expand_limits(y = c(0, 155))

p11

pb <- (p10 + p8 + p9 + 
         plot_layout(ncol = 1, axes = "keep", heights = c(1, 1, 1)) +
         plot_annotation(tag_levels = "A")) &
  theme(
    plot.tag = element_text(size = 10, color = "black", face = "bold"),
    axis.line = element_line(linewidth = 0.25),
    axis.ticks = element_line(linewidth = 0.25),
    plot.margin = margin(0, 0, 0, 0))

pb

# ggsave("Daniella/images/eFigure3.png", pb, dpi = 600, units = "mm", width = 90, height = 100)
# ggsave("Daniella/images/eFigure3.jpeg", pb, dpi = 600, units = "mm", width = 90, height = 100)
# ggsave("Daniella/images/eFigure3.pdf", pb, units = "mm", width = 90, height = 100)

############### upset plots- poly use  ##########

alc <- read_xlsx("HCP_raw/S1200_SSAGA_Raw_Released_Distribution_Sept2017.xlsx")%>%
  select(Subject = "PUBLIC_ID...1", AL1)%>%
  mutate(alc_user = ifelse (AL1 == "YES", 1, 0 ))%>%
  select(-(AL1))

base_dat <- left_join(base_dat, alc, by = "Subject")

# polyuse plot 

poly_dat <- base_dat %>%
  mutate(
    illic_user = ifelse(is.na(illic_user), 0, illic_user),
    tobac_user = ifelse(is.na(tobac_user), 0, tobac_user),
    thc_user   = ifelse(is.na(thc_user), 0, thc_user),
    alc_user   = ifelse(is.na(alc_user), 0, alc_user),
    illic_user = illic_user == 1,
    tobac_user = tobac_user == 1,
    thc_user   = thc_user == 1,
    alc_user = alc_user == 1)%>%
  rename(Alcohol = alc_user, 
         Marijuana = thc_user, 
         Tobacco = tobac_user,
         `Illicit Drugs` = illic_user)

poly_dat$degree <- rowSums(poly_dat[, c("Illicit Drugs","Tobacco","Alcohol","Marijuana")])

intersection <- upset(
  poly_dat,
  wrap = TRUE,
  name = "Lifetime Drug Use Patterns",
  intersect = c("Illicit Drugs", "Tobacco", "Alcohol", "Marijuana"),
  sort_intersections_by = "degree",
  sort_intersections = "ascending",
  stripes = upset_stripes(colors = "grey95", geom=geom_segment(size=3)),
  height_ratio = 0.35,
  themes=upset_modify_themes(
    list('intersections_matrix'=theme(
      axis.text.y=element_text(color = "black", size = 6),
      axis.title.x=element_text(color = "black", size = 7),
      panel.grid = element_blank(),
      'Intersection size' =theme(axis.text.y=element_text(size = 7))))),
  base_annotations = list(
    "Intersection size" = intersection_size(
      counts = TRUE,
      width = 0.7,
      bar_number_threshold = 1,
      text = list(vjust = -0.5, size = 1.75)) +
      coord_cartesian(ylim = c(0, 350)) + 
      theme_void() +
      theme(
        axis.title.y = element_text(vjust = -20, size = 7),
        axis.line = element_line(color = "white")) +
      ylab("Number of Users by Pattern")
  ),
  set_sizes = (
    upset_set_size() +
      expand_limits(y=1300)+
      geom_bar (fill = "grey75", width = 0.6) +
      geom_text(aes(label=..count..), hjust=1.2, stat='count', size = 1.75) +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 7, color = "black"))+
      ylab("Number of Users by Drug")
  ),
  matrix = intersection_matrix(
    geom = geom_point(shape = 19, size = 2),
    segment = geom_segment(
      linetype = "solid", linewidth = 0.75),
    outline_color = list(
      active = NA,     
      inactive = NA)
  ),
  
  ## colors##########
  queries=list(
    upset_query(
      intersect=c('Marijuana', 'Tobacco'),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Alcohol', 'Tobacco'),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Alcohol', 'Illicit Drugs'),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Alcohol', 'Marijuana'),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Alcohol', 'Marijuana', 'Tobacco'),
      fill="#1f77b4",
      color = "#1f77b4",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Alcohol', 'Illicit Drugs', 'Tobacco'),
      fill="#1f77b4",
      color = "#1f77b4",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Alcohol', 'Marijuana', 'Illicit Drugs'),
      fill="#1f77b4",
      color = "#1f77b4",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Alcohol', 'Marijuana', 'Tobacco', 'Illicit Drugs'),
      fill= "#E03426FF",
      color = "#E03426FF",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Alcohol'),
      fill= "#F28E2BFF",
      color = "#F28E2BFF",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Marijuana'),
      fill= "#F28E2BFF",
      color = "#F28E2BFF",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c('Tobacco'),
      fill= "#F28E2BFF",
      color = "#F28E2BFF",
      only_components=c('intersections_matrix', 'Intersection size')) 
    ######
  ))+
  theme(
    panel.spacing = unit(80, "mm"),
    plot.tag = element_text(size = 10, face = "bold", hjust = -1, vjust = 1.5),
    plot.tag.position = c(0,1))+
  labs(tag = "B")

intersection

ppoly <- (free(p11) + free(intersection) + 
            plot_layout(ncol = 2, widths = c(1, 2)))

ppoly

ggsave("Daniella/images/Figure1.png", ppoly, dpi = 600, units = "mm", width = 180, height = 70)
ggsave("Daniella/images/Figure1.pdf", ppoly, units = "mm", width = 180, height = 70)
ggsave("Daniella/images/Figure1.jpeg", ppoly, dpi = 500, units = "mm", width = 180, height = 70)

#### mean thck ~ audit residual scatter ###############

library(ggeffects) 

winsorize = function(x,q=3){
  
  mean_x = mean(x,na.rm = T)
  sd_x = sd(x,na.rm = T)
  top_q = mean_x + q*sd_x
  bottom_q = mean_x - q*sd_x
  
  x[x>top_q] = top_q
  x[x<bottom_q] = bottom_q
  
  return(x)
  
}

base_dat$Age1 = poly(base_dat$Age_in_Yrs,2)[,1] %>% scale(center = T,scale = T)
base_dat$Age2 = poly(base_dat$Age_in_Yrs,2)[,2] %>% scale(center = T,scale = T)

var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age1", "Age2", "Family_ID", 
             "SSAGA_Income", "SSAGA_Educ","audit_c", "mean_Thck")

var.names <-  c("Gender", "MZ","DZ", "Half", "audit_c", "mean_Thck", "SSAGA_Educ", "SSAGA_Income")
var.mutate <- c("SSAGA_Income", "SSAGA_Educ", "audit_c", "mean_Thck")

dat.analyze = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()

dat.analyze <- dat.analyze %>%
  mutate(across(all_of(var.mutate), winsorize)) %>%
  mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
  mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

m1 <- lmerTest::lmer(mean_Thck ~ 
                       audit_c +
                       SSAGA_Income + 
                       SSAGA_Educ + 
                       Age1 + 
                       Age2 + 
                       Gender +
                       MZ + 
                       DZ + 
                       Half +
                       (1 | Family_ID), data = dat.analyze)

dat.analyze$residuals <- residuals(m1)

res <- dat.analyze %>%
  select(Subject, residuals)

base_dat <- left_join(base_dat, res, by = "Subject")  

var.keep = c("Subject", "Gender", "MZ","DZ", "Half","Age1", "Age2", "Family_ID", 
             "SSAGA_Income", "SSAGA_Educ","audit_c", "mean_Thck")

var.names <-  c("Gender", "MZ","DZ", "Half", "mean_Thck", "SSAGA_Educ", "SSAGA_Income")
var.mutate <- c("SSAGA_Income", "SSAGA_Educ", "mean_Thck")

dat.analyze = base_dat %>%  dplyr::select(all_of(var.keep)) %>% na.omit()

dat.analyze <- dat.analyze %>%
  mutate(across(all_of(var.mutate), winsorize)) %>%
  mutate(across(all_of(var.mutate), ~ if (abs(psych::skew(.x)) > 1){log(.x + 2)}else{.x})) %>%
  mutate(across(all_of(var.names), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

m1 <- lmerTest::lmer(mean_Thck ~ 
                       audit_c +
                       SSAGA_Income + 
                       SSAGA_Educ + 
                       Age1 + 
                       Age2 + 
                       Gender +
                       MZ + 
                       DZ + 
                       Half +
                       (1 | Family_ID), data = dat.analyze)

dat <- predict_response(m1, terms = "audit_c", type = "fixed")

dat <- dat%>%
  rename(audit_c = x, residuals = predicted)

scatter <- ggplot(base_dat, aes(x = audit_c, y = residuals)) +
  geom_pointdensity(size = 3)+
  geom_smooth(data = dat, color = "deeppink4", method = "lm", se = FALSE)+
  scale_color_viridis(option = "G", direction = -1)+
  theme_classic(base_size = 7)+
  labs(x = "Hazardous Alcohol Use", y = "Standardized Adjusted Global Thickness")+
  scale_x_continuous(breaks = seq(0, 12, by = 2))+
  scale_y_continuous(breaks = seq(-2, 2, by = 1), limits = c(-2, 2))+
  theme(
    legend.position = "none",
    axis.text = element_text(size =6))

scatter

ggsave("Daniella/images/Figure2.png", scatter, dpi = 500, units = "mm", width = 90, height = 70)
ggsave("Daniella/images/Figure2.pdf", scatter, units = "mm", width = 90, height = 70)
ggsave("Daniella/images/Figure2.jpeg", scatter, dpi = 500, units = "mm", width = 90, height = 70)

# ggsave("scatter.jpeg", scatter, dpi = 500, units = "cm")

##################### mean_Thck ~ SU Vars ###################

plot2 <- read_xlsx("Daniella/brainstr_su/results/primary/3_meanThck_shared.xlsx")%>%
  mutate(stroke = ifelse(pfdr < 0.05, 1, 0.5),
         alpha = ifelse(pfdr < 0.05, 1, 0.5),
         size = ifelse(pfdr < 0.05, 0.75, 0.5),
         sig = ifelse(pfdr < 0.05, "yes", "no"))

plot2$xvar <- plot2$xvar %>%
  dplyr::recode_factor(
    "audit_c"            = "mAUDIT-C",
    "onset_alc"          = "Alcohol Use Age of Onset",
    "SSAGA_Alc_D4_Dp_Dx" = "Alcohol Dependence",
    "thc_user"           = "Marijuana Use",
    "onset_thc"          = "Marijuana Use Age of Onset",
    "thc_heavy"          = "Heavy Marijuana Use",
    "SSAGA_Mj_Ab_Dep"    = "Marijuana Dependence",
    "tobac_user"         = "Tobacco Use",
    "onset_tobac"        = "Tobacco Use Age of Onset",
    "tobac_heavy"        = "Heavy Tobacco Use",
    "illic_user"         = "Illicit Drug Use",
    "onset_illic"        = "Illicit Drug Use Age of Onset",
    "illic_max"          = "Maximum Illicit Drug Use") %>%
  forcats::fct_rev()

p2 <- ggplot(plot2, aes(x = Estimate, y = xvar)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  geom_point(aes(alpha = alpha, stroke = 1), fill = "#5ebba0", color = "#5ebba0",
             size = 1.5, shape = 21, show.legend = FALSE) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0, color = "#5ebba0", 
                 alpha = plot2$alpha, size = plot2$size, show.legend = FALSE)+
  theme_classic(base_size = 7)+
  labs(x = "Standardized Regression Estimate (95% CI) \nAssociation With Global Brain Thickness", y = "")+
  theme(
    axis.text.x = element_text(color = "black", size = 6))+
  geom_point(data = subset(plot2, xvar %in% c("mAUDIT-C", 
                                                 "Marijuana Use")),
             aes(x = `2.5 %` - 0.01, y = xvar),  
             shape = 8, size = 1, color = "black")
p2

# ggsave("Daniella/images/Figuret3.png", p2, dpi = 500, units = "mm", width = 90, height = 60)
# ggsave("Daniella/images/Figure3.pdf", p2, units = "mm", width = 90, height = 60)
# ggsave("Daniella/images/Figure3.jpeg", p2, dpi = 500, units = "mm", width = 90, height = 60)

############## mean_thck ~ wtn/btwn fam SU #########################

plot3 <- read_xlsx("Daniella/brainstr_su/results/primary/5_meanThck_wtnbtwn.xlsx")%>%
  mutate(
    drug = case_when(
      str_detect(x.var, regex("audit", ignore_case = TRUE)) ~ "mAUDIT-C",
      str_detect(x.var, regex("thc", ignore_case = TRUE)) ~ "Marijuana Use",
      TRUE ~ NA_character_),
      group = case_when(
        str_detect(x.var, "within") ~ "Within-Family",
        str_detect(x.var, regex("mean", ignore_case = TRUE)) ~ "Between-Family"),
      my_color = case_when(
        sample == "whole" ~ "#F5CB63FF",
        sample =="Half != 1" ~ "#F7A84AFF",
        sample == "MZ == 1 | DZ == 1" ~ "#EF731EFF",
        sample =="MZ == 1" ~ "#9E3A26FF"),
      drug_group_sample = interaction(drug, group, sample, sep = "_"),
      order = factor(sample, levels = c(
          "MZ == 1", "MZ == 1 | DZ == 1", "Half != 1", "whole")))%>%
  mutate(
     group = factor(group, levels = c("Within-Family", "Between-Family")),
     drug = factor(drug, levels = c("mAUDIT-C", "Marijuana Use")),
     stroke = ifelse(pfdr < 0.05, 1, 0.5),
     alpha = ifelse(pfdr < 0.05, 1, 0.5),
     size = ifelse(pfdr < 0.05, 0.75, 0.5),
     sig = ifelse(pfdr < 0.05, "yes", "no"))

p3 <- ggplot(plot3, aes(x = Estimate, y = order, group = drug_group_sample)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = 4.6, color= "grey80", linewidth = 1) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`, color = order), 
                 height = 0, size = plot3$size, alpha = plot3$alpha) +
  geom_point(aes(stroke = stroke, fill = order, color = order),
             size = 1.5, shape = 21, alpha = plot3$alpha) +
  scale_color_manual(name = "", values = setNames(plot3$my_color, plot3$order),
                     labels = c("MZ == 1" = "Monozygotic Twins", "MZ == 1 | DZ == 1" = "Monozygotic & Dizygotic Twins", 
                                "Half != 1" = "Full Siblings", "whole" = "Entire Sample")) +
  scale_fill_manual(name = "", values = setNames(plot3$my_color, plot3$order),
                    labels = c("MZ == 1" = "Monozygotic Twins", "MZ == 1 | DZ == 1" = "Monozygotic & Dizygotic Twins", 
                               "Half != 1" = "Full Siblings", "whole" = "Entire Sample")) +
  facet_grid(drug ~ group, scales = "fixed", switch = "y",
             labeller = as_labeller(c(
               "mAUDIT-C" = "mAUDIT-C",
               "Marijuana Use" = "MJ Use",
               "Within-Family" = "Within-Family",
               "Between-Family" = "Between-Family"
             )))+
  theme_classic(base_size = 7) +
  guides(color = guide_legend(ncol = 4, reverse = TRUE),
         fill  = guide_legend(reverse = TRUE))+
  xlab("Standardized Regression Estimate (95% CI) Association With Brain Thickness")+
  theme(
    axis.line.y = element_line(color = "grey80"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 6),
    strip.placement = "left",
    strip.background = element_rect(colour = "white", fill = "white"),
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(0, 0, 0, 0, unit = "cm"))+
  xlim(-0.3, 0.3)

p3

################### SU vars ~ wtn/btwn mean_Thck ##############

plot4 <- read_xlsx("Daniella/brainstr_su/results/primary/6_substanceUse_wtnbtwnThck.xlsx")%>%
  mutate(
    stroke = ifelse(pfdr < 0.05, 1, 0.5),
    alpha = ifelse(pfdr < 0.05, 1, 0.5),
    size = ifelse(pfdr < 0.05, 0.75, 0.5),
    sig = ifelse(pfdr < 0.05, "yes", "no"),
    y.var = case_when(
      y.var == "thc_user" ~ "Marijuana Use",
      y.var == "audit_c" ~ "mAUDIT-C"),
    group = case_when(
      x.var == "family_meanThck" ~ "Between-Family",
      x.var == "withinFam_thck" ~ "Within-Family"),
    y.var = factor(y.var, levels = c("mAUDIT-C", "Marijuana Use")),
    group = factor(group, levels = c("Within-Family", "Between-Family")),
    my_color = case_when(
      sample == "whole" ~ "#E5CAEA",
      sample =="Half != 1" ~ "#C490CF",
      sample == "MZ == 1 | DZ == 1" ~ "#90519C",
      sample =="MZ == 1" ~ "#491F50"),
    drug_group_sample = interaction(y.var, group, sample, sep = "_"),
    order = factor(sample, levels = c(
      "MZ == 1", "MZ == 1 | DZ == 1", "Half != 1", "whole")))

p4 <- ggplot(plot4, aes(x = Estimate, y = order, group = drug_group_sample)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = 4.6, color= "grey80", linewidth = 1) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`, color = order), 
                 height = 0, size = plot4$size, alpha = plot4$alpha) +
  geom_point(aes(stroke = stroke, fill = order, color = order),
             size = 1.5, shape = 21, alpha = plot4$alpha) +
  scale_color_manual(name = "", values = setNames(plot4$my_color, plot4$order),
                     labels = c("MZ == 1" = "Monozygotic Twins", "MZ == 1 | DZ == 1" = "Monozygotic & Dizygotic Twins", 
                                "Half != 1" = "Full Siblings", "whole" = "Entire Sample")) +
  scale_fill_manual(name = "", values = setNames(plot4$my_color, plot4$order),
                    labels = c("MZ == 1" = "Monozygotic Twins", "MZ == 1 | DZ == 1" = "Monozygotic & Dizygotic Twins", 
                               "Half != 1" = "Full Siblings", "whole" = "Entire Sample")) +
  facet_grid(y.var ~ group, scales = "fixed", switch = "y",
             labeller = as_labeller(c(
               "mAUDIT-C" = "mAUDIT-C",
               "Alcohol Age of Onset" = "Alc Onset",
               "Marijuana Use" = "MJ Use",
               "Within-Family" = "Within-Family Brain Thickness",
               "Between-Family" = "Between-Family Brain Thickness"
             )))+
  theme_classic(base_size = 7) +
  guides(color = guide_legend(ncol = 4, reverse = TRUE),
         fill  = guide_legend(reverse = TRUE))+
  xlab("Standardized Regression Estimate (95% CI) Association With Substance Use")+
  theme(
    axis.line.y = element_line(color = "grey80"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 6),
    strip.placement = "left",
    strip.background = element_rect(colour = "white", fill = "white"),
    strip.text.y.left = element_text(angle = 0, hjust = 1),
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(0, 0, 0, 0, unit = "cm"))+
  xlim(-0.3, 0.3)

p4

############# SOLAR-Eclipse Heritability & variance component corr###########

#heritability

peher1 <- read_xlsx("Daniella/brainstr_su/results/primary/7_solar_analyses.xlsx")%>%
  filter(!x.var == "onset_alc")%>%
  dplyr::select(x.var, xh2r_Estimate, xh2r_SE, xh2r_pval, xe2_Estimate, xe2_SE)%>%
  rename(x.var = x.var, 
         estimate = xh2r_Estimate, 
         se = xh2r_SE,
         pval = xh2r_pval,
         eestimate = xe2_Estimate,
         ese = xe2_SE)

peher2 <- read_xlsx("Daniella/brainstr_su/results/primary/7_solar_analyses.xlsx")%>%
  dplyr::select(y.var, yh2r_Estimate, yh2r_SE, yh2r_pval, ye2_Estimate, ye2_SE)

colnames(peher2) = colnames(peher1)  

plot5 <- rbind(peher1, peher2)%>%
  pivot_longer(
    cols = c(estimate, se, eestimate, ese),
    names_to = c("prefix", ".value"),
    names_pattern = "(e?)(estimate|se)") %>%
  mutate(type = case_when(
    prefix == "" ~ "Heritability",
    prefix == "e" ~ "Environmental Variance"),
    stroke = ifelse(pval<0.05, 1, 0.5),
    alpha = ifelse(pval<0.05, 1, 0.5),
    size = ifelse(pval<0.05, .75, 0.25),
    x.var = case_when(
      x.var == "mean_Thck" ~ "Brain Thickness",
      x.var =="audit_c" ~ "mAUDIT-C",
      x.var =="thc_user" ~ "MJ Use"),
    ci_lower_std = estimate - 1.96*se,
    ci_upper_std = estimate + 1.96*se,
    x.var = factor(x.var, levels = c(
      "Brain Thickness",
      "MJ Use",
      "mAUDIT-C"))) 

dodge <- position_dodge(width = 0.5)

p5 <- ggplot(plot5, aes(y = x.var, x = estimate, fill = type, color = type)) +
  geom_hline(yintercept = c(1.5, 2.5), color = "grey80", linewidth = 0.5, linetype = "solid") +
  geom_point(size = 1.5, shape = 21, position = dodge, stroke = plot5$stroke) +
  geom_errorbarh(aes(xmin = ci_lower_std, xmax = ci_upper_std),
                 size = plot5$size,  height = 0, position = dodge) +
  scale_fill_manual(
    values = c("Heritability" = "#c1447e", "Environmental Variance" = "#8bac54")) +
  scale_color_manual(
    values = c("Heritability" = "#c1447e", "Environmental Variance" = "#8bac54")) +
  labs(x = "Standardized Estimate (95% CI)", y = "", fill = "", color = "") +
  theme_classic(base_size = 7) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1))+
  theme(
    axis.text.x = element_text(color = "black", size = 6),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0, unit = "cm"))+
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))

p5

# variance component correlation

plot6 <- read_xlsx("Daniella/brainstr_su/results/primary/7_solar_analyses.xlsx")%>%
  mutate(x.var = case_when(
    x.var =="audit_c" ~ "mAUDIT-C",
    x.var =="thc_user" ~ "MJ Use",))%>%
  dplyr::select(x.var, rhog_Estimate, rhog_SE, rhog_pvalZ, rhoe_Estimate, rhoe_SE, rhoe_pvalZ) %>%
  pivot_longer(
    cols = c(rhog_Estimate, rhoe_Estimate, rhog_SE, rhoe_SE),
    names_to = c("type", ".value"),
    names_pattern = "(rhog|rhoe)_(Estimate|SE)")%>%
  mutate(sig = case_when(
    rhog_pvalZ < 0.05 & type == "rhog" ~ "y",
    rhoe_pvalZ < 0.05 & type == "rhoe" ~ "y",
    TRUE ~ "n"),
    stroke = ifelse(sig == "y", 1, 0.5),
    alpha = ifelse(sig == "y", 1, 0.5),
    size = ifelse(sig == "y", .75, 0.5),
    type = case_when(
      type == "rhog" ~ "Additive Genetics",
      type == "rhoe" ~ "Non-Shared Environment"),
    ci_lower_std = Estimate - 1.96*SE,
    ci_upper_std = Estimate + 1.96*SE,
    x.var = factor(x.var, levels = c(
      "MJ Use",
      "mAUDIT-C")))

p6 <- ggplot(plot6, aes(y = x.var, x = Estimate, fill = type, color = type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = 1.5, color = "grey80", linewidth = 0.5, linetype = "solid") +
  geom_point(stroke = plot6$stroke, alpha = plot6$alpha,
             size = 1.5, shape = 21, position = dodge) +
  geom_errorbarh(aes(xmin = ci_lower_std, xmax = ci_upper_std),
                 size = plot6$size, alpha = plot6$alpha, height = 0, position = dodge) +
  scale_fill_manual(
    values = c("Additive Genetics" = "#1f77b4", "Non-Shared Environment" = "#EF2929FF"),
    guide = guide_legend(reverse = TRUE)) +
  guides(color = guide_legend(ncol = 1, nrow = 2, reverse = TRUE),
         fill  = guide_legend(reverse = TRUE))+
  scale_color_manual(
    values = c("Additive Genetics" = "#1f77b4", "Non-Shared Environment" = "#EF2929FF"),
    guide = guide_legend(reverse = TRUE)) +
  labs(x = "Variance Component Correlation (95% CI)", y = "", fill = "", color = "") +
  theme_classic(base_size = 7) +
  theme(
    axis.text = element_text(color = "black", size = 6),
    axis.ticks.y = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0, unit = "cm"))+
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2))

p5
p6

######### combined wtn/btwn & solar analyses #############

p7 <- (p3) / (p4) / (p5 | p6)+
  plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size = 10, face = "bold"))

p7

ggsave("Daniella/images/Figure4.png", p7, dpi = 500, units = "mm", width = 180, height = 170)
ggsave("Daniella/images/Figure4.pdf", p7, units = "mm", width = 180, height = 170)
ggsave("Daniella/images/Figure4.jpeg", p7, dpi = 500, units = "mm", width = 180, height = 170)

########## SUPPLEMENTARY MATERIALS PLOTS #############

#### region ~ maudit-c #########################################

noctrl <- read_xlsx("Daniella/brainstr_su/results/primary/1_region_audit_noThckCtrl.xlsx")%>%
  filter(!y.var == "FS_BrainSeg_Vol_No_Vent")%>%
  mutate(Dataset = "Before Thickness Covariate",
         fdr = if_else(pfdr < 0.05, 1, 0 ))

ctrl <- read_xlsx("Daniella/brainstr_su/results/primary/2_region_audit_ThckCtrl.xlsx")%>%
  filter(!y.var == "FS_BrainSeg_Vol_No_Vent")%>%
  mutate(Dataset = "After Thickness Covariate",
         fdr = 0)

ctrl[24,] = noctrl [24,]
ctrl[24,11] = "After Thickness Covariate"
ctrl[24,12] = 0

plot1 <- rbind(ctrl, noctrl)

level_order <- unique(plot1$y.var)

labels <- c(
  "L Hippocampus", "R Hippocampus", "L Amygdala", "R Amygdala",
  "L Cerebellum", "R Cerebellum", "L Caudal Middle Frontal", "R Caudal Middle Frontal",
  "L Rostral Middle Frontal", "R Rostral Middle Frontal", "L Superior Frontal", "R Superior Frontal",
  "L Inferior Temporal", "R Inferior Temporal", "L Middle Temporal", "R Middle Temporal",
  "L Insula", "R Insula", "L Precuneus", "R Precuneus", "L Frontal Pole", "R Frontal Pole",
  "Surface Area", "Brain Thickness")

plot1 <- plot1%>%
  mutate(
    ci_lower = Estimate - 1.96 * `Std. Error`,
    ci_upper = Estimate + 1.96 * `Std. Error`,
    type = as.factor(case_when(
      y.var == "mean_Thck" ~ "Global",  
      str_detect(y.var, "Vol$")  ~ "Volume",
      str_detect(y.var, "Thck$") ~ "Thickness",
      str_detect(y.var, "SA$") ~ "Global",
      TRUE ~ as.character(NA))),
    y.var = factor(y.var, levels = level_order, labels = labels),
    y_numeric = as.numeric(factor(y.var, levels = rev(unique(y.var)))),
    y_pos = ifelse(Dataset == "After Thickness Covariate",
                   y_numeric + .7,
                   y_numeric - .7),
    stroke = ifelse(fdr ==1, 1, 0.5),
    alpha = ifelse(fdr ==1, 1, 0.5),
    size = ifelse(fdr ==1, .75, .5),
    color = case_when(
      y.var == "Brain Thickness" ~ "grey40",
      Dataset == "After Thickness Covariate" ~ "violetred3",
      Dataset == "Before Thickness Covariate" ~ "#1f77b4"),
    Dataset = factor(Dataset, levels = c("Before Thickness Covariate", "After Thickness Covariate")))%>%
  group_by(type) %>%
  arrange(y.var) %>%
  mutate(y_numeric = row_number()) %>%
  ungroup()%>%
  mutate(size = ifelse(y.var == "Brain Thickness", 0.75, size),
         alpha = ifelse(y.var == "Brain Thickness", 1, alpha))

plot1[47,17:19] = plot1[48,17:19]

p1 <- ggplot(plot1, aes(x = Estimate, y = y.var, color = color, fill = color)) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  geom_point(size = 1.5, shape = 21, alpha = plot1$alpha, fill = plot1$color,
             stroke = plot1$stroke, color = plot1$color) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0, alpha = plot1$alpha, 
                 size = plot1$size, color = plot1$color)+
  theme_classic(base_size = 7)+
  facet_grid(type ~ Dataset,
             labeller = "label_value",
             scales = "free_y", space = "free_y", switch = "y")+
  scale_x_continuous(breaks = seq(-0.1, 0.1, by = 0.1), limits = c(-0.2, 0.09))+
  theme(
    strip.placement = "left",
    panel.spacing = unit(0, "lines"),
    strip.background = element_rect(colour = "white", fill = "grey90"),
    axis.text.x = element_text(color = "black", size = 6),
    legend.position = "none",
  )+
  labs(x = "Standardized Regression Estimate (95% CI) Association With mAUDIT-C", 
       y = "", color = NULL, fill = NULL, color = "black")

p1

# ggsave("Daniella/images/eFigure4.png", p1, dpi = 500, units = "mm", width = 180, height = 100)
# ggsave("Daniella/images/eFigure4.pdf", p1, units = "mm", width = 180, height = 100)
# ggsave("Daniella/images/eFigure4.jpeg", p1, dpi = 500, units = "mm", width = 180, height = 100)

######## sample characteristics ###############

samp <- base_dat%>%
  dplyr::select(Subject, Gender, Age_in_Yrs, SSAGA_Income, SSAGA_Educ, DZ, MZ, Half, Family_ID, Race)

# gender

gend <- data.frame(
  Gender = c("Male", "Female"),
  value = c(507, 606))

pgend <- ggplot(gend, aes(x = Gender, y = value, fill = Gender)) +
  geom_col()+
  scale_fill_manual(values = c("violetred3", "#1f77b4")) +
  theme_classic(base_size = 6) + 
  labs (tag = "A", title = "Sex")+
  theme(
    legend.position = "none", 
    plot.tag = element_text(face = "bold", size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(color = "black"),
    plot.title = element_text(size = 7)
    )    
  
pgend

#Age 

age <- table(samp$Age_in_Yrs)%>%
  data.frame()%>%
  rename(Age = Var1, value = Freq)

page <- ggplot(age, aes(x = Age, y = value, fill = Age)) +
  geom_col()+
  scale_fill_manual(values = colorRampPalette(c("#fae587","#e27950"))(nrow(age))) +
  scale_x_discrete(breaks = levels(age$Age)[seq(1, length(levels(age$Age)), by = 2)]) +
  scale_y_continuous(breaks = seq(0, max(age$value), by = 25)) +
  theme_classic(base_size = 6) +       
  labs(tag = "C", title = "Age")+
  theme(
    legend.position = "none", 
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black"),
    plot.tag = element_text(face = "bold", size = 10),
    axis.title.x = element_blank(),                           
    plot.title = element_text(size = 7)) +
  ggtitle("Age")

page

# income

income <- data.frame(table(samp$SSAGA_Income))%>%
  rename(Income = Var1, value = Freq)%>%
  mutate(Income = recode(Income,
                         "1" = "<$10,000",
                         "2" = "$10K–19,999",
                         "3" = "$20K–29,999",
                         "4" = "$30K–39,999",
                         "5" = "$40K–49,999",
                         "6" = "$50K–74,999",
                         "7" = "$75K–99,999",
                         "8" = "≥$100,000"))

pinc <- ggplot(income, aes(x = Income, y = value, fill = Income)) +
  geom_col()+
  scale_fill_manual(values = colorRampPalette(c("#d0ccf4","#3f72bf"))(nrow(income)))+
  theme_classic(base_size = 6) + 
  labs(tag = "D", title = "Yearly Income")+
  theme(
    legend.position = "none", 
    text = element_text(color = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    plot.tag = element_text(face = "bold", size = 10),
    axis.title.x = element_blank(),                           
    plot.title = element_text(size = 7, hjust = 0))

pinc

#race

race <- data.frame(table(samp$Race)) %>%
  rename(Race = Var1, value = Freq) %>%
  mutate(Race = factor(Race,
                       levels = c(
                         "Am. Indian/Alaskan Nat.",
                         "Unknown or Not Reported",
                         "More than one",
                         "Asian/Nat. Hawaiian/Othr Pacific Is.",
                         "Black or African Am.",
                         "White"
                       ),
                       labels = c(
                         "Am./Alaskan Nat.",
                         "Unknown",
                         "Multiracial",
                         "Asian/Pacific Is.",
                         "Black/African Am.",
                         "White"
                       )))

prace <- ggplot(race, aes(x = Race, y = value, fill = Race)) +
  geom_col()+
  scale_fill_manual(values = c("#F8B620FF", "#7fb800",  "#1f77b4", "#F28E2BFF", "violetred3",  "#9f9cca"))+
  ylim(0, 900)+
  theme_classic(base_size = 6) + 
  geom_text(aes(label = value, y = value + 10), size = 1.75,
            position = position_dodge(0.9), vjust = 0) +
  labs(tag = "E", y = "", x = "")+
  theme(
    legend.position = "none", 
    text = element_text(color = "black"),
    axis.ticks.y = element_blank(),
    plot.tag = element_text(face = "bold", size = 10),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 7, hjust = 0)) +
  ggtitle("Racial Identity")

prace

#education 

educ <- data.frame(table(samp$SSAGA_Educ))%>%
  rename("Years Completed" = Var1, value = Freq)%>%
  mutate(`Years Completed` = case_when(
    `Years Completed` == 17 ~ "17+",
    TRUE ~ as.character(`Years Completed`)
  ))

pedu <- ggplot(educ, aes(x = `Years Completed`, y = value, fill = `Years Completed`)) +
  geom_col()+
  scale_fill_manual(values = colorRampPalette(c("#ffc6c4", "#ad466c"))(nrow(educ)))+
  theme_classic(base_size = 6) +
  labs(tag="B", x = "", y = "", title = "Education (Years Completed)")+
  theme(
    legend.position = "none", 
    plot.tag = element_text(face = "bold", size = 10),            
    plot.title = element_text(size = 7)) 

pedu

# sibstat

sibstat <- data.frame(
  sibling_status = c("Singleton", "Non-twin Sibling", "Half Sibling", "Dizygotic Twin", "Monozygotic Twin"),
  value = c(64, 543, 27, 173, 306))

sibstat$sibling_status <- factor(
  sibstat$sibling_status,
  levels = c("Singleton", "Non-twin Sibling", "Half Sibling", "Dizygotic Twin", "Monozygotic Twin"))


psib <- ggplot(sibstat, aes(x = `sibling_status`, y = value, fill = `sibling_status`)) +
  geom_col()+
  scale_fill_manual(values = c("#9f9cca", "#7fb800", "#1f77b4", "violetred3", "#e48f4e"))+
  theme_classic(base_size = 6) +       
  geom_text(aes(label = value, y = value + 10), size = 1.75,
            position = position_dodge(0.9), vjust = 0) +
  labs(tag="F", y = "", title = "Sibling Status", x = "")+
  theme(
    legend.position = "none", 
    plot.tag = element_text(face = "bold", size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),                  
    plot.title = element_text(size = 7))

psib

pa <- (pedu | page | pinc) / (pgend | prace | psib)+
  plot_layout(widths = c(2, 7, 5))

pa1 <- pgend + pedu + page + 
  plot_layout(widths = c(1, 2, 2), ncol = 3)

pa2 <- pinc + prace + psib +
  plot_layout(widths = c(1, 1, 1), ncol = 3)

pa <- pa1 / pa2

pa

ggsave("Daniella/images/eFigure1.png", pa, dpi = 500, units = "mm", width = 180, height = 100)
ggsave("Daniella/images/eFigure1.pdf", pa, units = "mm", width = 180, height = 100)
ggsave("Daniella/images/eFigure1.jpeg", pa, dpi = 500, units = "mm", width = 180, height = 100)


######### mean_thck ~ primary + 2ndary SU vars #########

plot8 <- read_xlsx("Daniella/brainstr_su/results/post-hoc/1_meanThck_secondaryDrugVars.xlsx")%>%
  rename(co.var = covar)%>%
  mutate(stroke = ifelse(pfdr < 0.05, 1, 0.5),
         alpha = ifelse(pfdr < 0.05, 1, 0.5),
         size = ifelse(pfdr < 0.05, .75, .5),
         sig = ifelse(pfdr < 0.05, "yes", "no"))

plot8$xvar <- plot8$xvar %>%
  dplyr::recode_factor(
    "Breathalyzer_Over_08"  = "Breathalyzer Test",
    "SSAGA_TB_Smoking_History" = "Times Used Tobacco",
    "THC" = "Marijuana Drug Test",
    "SSAGA_Mj_Times_Used"           = "Times Used Marijuana",
    "Oxycontin"          = "Oxycontin Drug Test",
    "Opiates"          = "Opiates Drug Test",
    "Amphetamines"    = "Amphetamines Drug Test",
    "MethAmphetamine"         = "Methamphetamine Drug Test",
    "Cocaine"        = "Cocaine Drug Test") %>%
  forcats::fct_rev()

plot8 <- rbind(plot8, plot2)

p8 <- ggplot(plot8, aes(x = Estimate, y = xvar)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80") +
  geom_hline(yintercept = 9.5, linetype = "solid", color = "black")+
  geom_point(aes(alpha = alpha, stroke = 1), fill = "#5ebba0", color = "#5ebba0",
             size = 1.5, shape = 21, show.legend = FALSE) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0, color = "#5ebba0", 
                 alpha = plot8$alpha, size = plot8$size, show.legend = FALSE)+
  theme_classic(base_size = 7)+
  labs(x = "Standardized Regression Estimate (95% CI) \nAssociation With Global Brain Thickness", y = "")+
  theme(
    axis.text.x = element_text(color = "black", size = 6))+
  geom_point(data = subset(plot8, xvar %in% c("mAUDIT-C", 
                                              "Marijuana Use")),
             aes(x = `2.5 %` - 0.01, y = xvar),  
             shape = 8, size = 1, color = "black")
p8

ggsave("Daniella/images/eFigure6.png", p8, dpi = 500, units = "mm", width = 90, height = 90)
ggsave("Daniella/images/eFigure6.pdf", p8, units = "mm", width = 90, height = 100)
ggsave("Daniella/images/eFigure6.jpeg", p8, dpi = 500, units = "mm", width = 90, height = 100)

########### unique mean_thck ~ wtn/btwn SU #################

plot9 <- read_xlsx("Daniella/brainstr_su/results/primary/4a_meanThck_unique_drugCtrl.xlsx")%>%
  mutate(
    Sample = factor(sample,
                    levels = c("whole", "Half != 1"), 
                    labels = c("Entire Sample", "Full Siblings")),
    y.axis = factor(x.var,
                    levels = c("withinFamAudit", "withinFam_thc",
                               "family_meanTHC"),
                    labels = c("Within-Family mAUDIT-C", "Within-Family MJ Use", 
                               "Between-Family MJ Use")),
    Covariates = factor(drug.co.var1, 
                        levels = c("audit_c",  "thc_user"),
                        labels = c("mAUDIT-C", "MJ Use")),
    alpha = if_else(pfdr < 0.05,1, 0.5))

dodge <- position_dodge(width = 0.5)

p9 <- ggplot(plot9, aes(y = y.axis, x = Estimate, color = Sample)) +
  geom_hline(yintercept = 1.3, linetype = "solid", color = "grey80") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80")+
  geom_point(aes(color = Sample, fill = Sample),  position = dodge, size = 1.5, shape = 21, stroke = 1) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), position = dodge, 
                 height = 0, size = 0.75)+
  scale_color_manual(
    values = c("Entire Sample" = "#1f77b4", "Full Siblings" = "violetred3"))+
  scale_fill_manual(
    values = c("Entire Sample" = "#1f77b4", "Full Siblings" = "violetred3"))+
  facet_grid(y.axis ~ Covariates, scales = "free_y", space = "free_y")+
  theme_classic(base_size = 7)+
  theme(   
    axis.text.x = element_text(color = "black", size = 6),
    legend.title = element_blank(),
    strip.text.y = element_blank(),
    panel.spacing = unit(1, "mm"),
    axis.title.y = element_blank(),
    legend.justification = "left",
    legend.position = "top", 
    legend.margin = margin(0, 0, 0, 0, unit = "cm"))+
  scale_y_discrete(expand = c(0, 0))+
  scale_x_continuous(breaks = c(-0.2, -0.1, 0), limits = c(-0.2, 0))+
  guides(color = guide_legend(ncol = 4))+
  xlab("Standardized Regression Estimate (95% CI) \n Association with Global Brain Thickness")

p9

ggsave("Daniella/images/eFigure8.png", p9, dpi = 500, units = "mm", width = 90, height = 60)
ggsave("Daniella/images/eFigure8.pdf", p9, units = "mm", width = 90, height = 60)
ggsave("Daniella/images/eFigure8.jpeg", p9, dpi = 500, units = "mm", width = 90, height = 60)

########## mediation test ###################################

plot10 <- read_xlsx("Daniella/brainstr_su/results/primary/8_potentialMediators_meanThck.xlsx")%>%
  filter(`Pr(>|t|)` < 0.05,
    !str_detect(as.character(x.var), "_T$|_AgeAdj"))%>%
  dplyr::select(y.var, x.var, Estimate, `Std. Error`, `Pr(>|t|)`, `2.5 %`, `97.5 %`)

plot10.2 <- read_xlsx("Daniella/brainstr_su/results/primary/9_potentialMediators_allVars.xlsx")%>%
  filter(!str_detect(as.character(xvar), "_T$|_AgeAdj"))%>%
  dplyr::select(y.var, xvar, Estimate, `Std. Error`, `Pr(>|t|)`, `2.5 %`, `97.5 %`)

colnames(plot10) = colnames(plot10.2)

plot10 <- rbind(plot10, plot10.2)

pal <- paletteer::paletteer_d("awtools::bpalette")

colors <- pal[((1:length(unique(plot10$xvar)) - 1) %% length(pal)) + 1]

plot10$xvar <- plot10$xvar%>%
  dplyr::recode_factor(
    "Menstrual_UsingBirthControl" = "Birth Control", 
    "EVA_Denom" = "Visual Acuity",
    "ASR_Rule_Raw" = "ASR Rule Breaking",
    "SSAGA_ChildhoodConduct" = "Childhood Conduct Issues",
     "PSQI_Other" = "Sleep Trouble",
    "PercHostil_Unadj" = "Percieved Hostility",
    "Sadness_Unadj" = "Sadness",
    "ER40NOE" = "Neutral Emo. Recognition",
      "VSPLOT_CRTE" = "Spatial Orientation", 
    "Relational_Task_Acc" = "Relational Task Accuracy",
      "CogCrystalComp_Unadj" = "NIH Crystallized Cognition", 
    "ReadEng_Unadj" = "Oral Reading Recognition",
      "Language_Task_Math_Avg_Difficulty_Level" = "Avg. Math & Lang. Difficulty",
     "WM_Task_0bk_Body_Acc_Target" = "WM- Target Accuracy",
     "WM_Task_0bk_Body_Acc_Nontarget" = "WM- Non-Target Accuracy",
      "WM_Task_0bk_Body_Median_RT_Target" = "WM- Median Reaction Time",
    "WM_Task_0bk_Body_Acc" = "WM- All Trials")
  
plot10 <- plot10%>%
mutate(y.var = factor(y.var, levels = c("mean_Thck", "audit_c",  "thc_user"),
                   labels = c("Global Brain Thickness", "mAUDIT-C",  "MJ Use")),
       alpha = ifelse(`Pr(>|t|)` < 0.05, 1, 0.5))

pal  <- paletteer::paletteer_d("awtools::bpalette")
lvl  <- levels(plot10$xvar)                              
cols <- rep(pal, length.out = length(lvl))               
cols <- setNames(cols, lvl)                             

p10 <- ggplot(plot10, aes(y = xvar, x = Estimate, color = xvar)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey80")+
  geom_point(aes(fill = xvar), size = 1.5, shape = 21, alpha = plot10$alpha) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), size = 0.75, height = 0, alpha = plot10$alpha)+
  theme_classic(base_size = 7)+
  xlim(-0.4, 0.4)+
  facet_grid(. ~ y.var, scales = "free_y", switch = "y")+
  xlab("Standardized Regression Estimate (95% CI) Associations")+
  theme(
    legend.position = "none",
    strip.placement = "outside",
    axis.title.y = element_blank(),
    axis.text = element_text(color = "black"),
    strip.text.x = element_text(size =6),
    panel.border = element_rect(color = "grey80", fill= NA),
    strip.text = element_text(color = "black"),
    strip.background = element_rect(fill = "white", color = NA)) +
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)

p10

ggsave("Daniella/images/eFigure7.png", p10, dpi = 500, units = "mm", width = 180, height = 90)
ggsave("Daniella/images/eFigure7.pdf", p10, units = "mm", width = 180, height = 90)
ggsave("Daniella/images/eFigure7.jpeg", p10, dpi = 500, units = "mm", width = 180, height = 90)

######### all brain~ all drugs (no mean_thck covar) ################

plot11 <- read_xlsx("Daniella/brainstr_su/results/post-hoc/2_allbrain_alldrugs_noCtrlThck.xlsx") %>%
  mutate(drug_type_order = case_when(
    xvar %in% c("audit_c", "SSAGA_Alc_D4_Dp_Dx", "onset_alc", 
                "Breathalyzer_Over_08") ~ 1,
    xvar %in% c("SSAGA_Mj_Ab_Dep", "onset_thc", "thc_heavy", "thc_user", 
                "SSAGA_Mj_Times_Used", "THC") ~ 2,
    xvar %in% c("onset_tobac", "tobac_heavy", "tobac_user") ~ 3,
    xvar %in% c("onset_illic", "illic_max", "illic_user", "SSAGA_Times_Used_Illicits", 
                "Cocaine", "Opiates", "Amphetamines", "MethAmphetamine", "Oxycontin") ~ 4,
    TRUE ~ 5),
    drugtype = case_when(
      drug_type_order == 1 ~ "Alcohol",
      drug_type_order == 2 ~ "Marijuana",
      drug_type_order == 3 ~ "Tobacco",
      drug_type_order == 4 ~ "Illicit Drugs"))%>%
  arrange(drug_type_order, xvar, group_id)%>%
  mutate(ypos_factor = factor(paste(xvar, y.var), 
                              levels = unique(paste(xvar, y.var))),
         stroke = case_when(pfdr_substance < 0.05 ~ 1,
                            pfdr < 0.05 ~ 1, 
                            TRUE ~ 0))

pvals <- c(1, 0.05, 0.0005, 0.000005)
breaks <- -log10(pvals)

get_shades <- function(vars, base_color) {
  setNames(colorRampPalette(c(lighten(base_color, 0.4), base_color))(length(vars)), vars)
}

xvar_colors <- c(
  get_shades(plot11 %>% filter(drugtype == "Alcohol") %>% pull(xvar) %>% unique(), "#f6511d"),
  get_shades(plot11 %>% filter(drugtype == "Marijuana") %>% pull(xvar) %>% unique(), "#7fb800"),
  get_shades(plot11 %>% filter(drugtype == "Tobacco") %>% pull(xvar) %>% unique(), "#ffb400"),
  get_shades(plot11 %>% filter(drugtype == "Illicit Drugs") %>% pull(xvar) %>% unique(), "#00a6ed")
) %>% unlist()

p11 <- ggplot(plot11, aes(x = -log10(`Pr(>|t|)`), y = ypos_factor, fill = xvar)) +
  geom_point(size = 1.5, shape = 21, alpha = 1, stroke = plot11$stroke) + 
  theme_classic(base_size = 7) +
  scale_x_continuous(
    breaks = breaks,
    labels = pvals,
    expand = expansion(mult = c(0.01, 0.05)))+
  scale_fill_manual(values = xvar_colors) +
  facet_wrap(~ drugtype, scales = "free_y", space = "free_y", ncol = 1, strip.position = "left")+
  xlab(expression(italic(p)~"(uncorrected, log scale)"))+
  theme(
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 6),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 360),
    strip.background = element_rect(color = NA, fill = NA),
    panel.spacing = unit(0, "lines"),
    legend.position = "none"
  )+
  scale_y_discrete(expand = expansion(add = c(50, 50)))

p11
######## all brain ~ all drugs + meanthck########################

plot12 <- read_xlsx("Daniella/brainstr_su/results/post-hoc/3_allbrain_alldrugs_ctrlThck.xlsx") %>%
  mutate(drug_type_order = case_when(
    xvar %in% c("audit_c", "SSAGA_Alc_D4_Dp_Dx", "onset_alc", 
                "Breathalyzer_Over_05", "Breathalyzer_Over_08") ~ 1,
    xvar %in% c("SSAGA_Mj_Ab_Dep", "onset_thc", "thc_heavy", "thc_user", 
                "SSAGA_Mj_Times_Used", "THC") ~ 2,
    xvar %in% c("onset_tobac", "tobac_heavy", "tobac_user") ~ 3,
    xvar %in% c("onset_illic", "illic_max", "illic_user", "SSAGA_Times_Used_Illicits", 
                "Cocaine", "Opiates", "Amphetamines", "MethAmphetamine", "Oxycontin") ~ 4,
    TRUE ~ 5),
    drugtype = case_when(
      drug_type_order == 1 ~ "Alcohol",
      drug_type_order == 2 ~ "Marijuana",
      drug_type_order == 3 ~ "Tobacco",
      drug_type_order == 4 ~ "Illicit Drugs"))%>%
  arrange(drug_type_order, xvar, group_id)%>%
  mutate(ypos_factor = factor(paste(xvar, y.var), 
                              levels = unique(paste(xvar, y.var))),
         stroke = ifelse(pfdr < 0.05, 1, 0))

p12 <- ggplot(plot12, aes(x = -log10(`Pr(>|t|)`), y = ypos_factor, fill = xvar)) +
  geom_point(size = 1.5, shape = 21, stroke = plot12$stroke) + 
  theme_classic(base_size = 7) +
  scale_x_continuous(
    breaks = breaks,
    labels = pvals,
    expand = expansion(mult = c(0.01, 0.05))) +
  scale_fill_manual(values = xvar_colors) +
  facet_wrap(~ drugtype, scales = "free_y", space = "free_y", ncol = 1, strip.position = "left")+
  xlab(expression(italic(p)~"(uncorrected, log scale)"))+
  theme(
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 6),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 360),
    strip.background = element_rect(color = NA, fill = NA),
    panel.spacing = unit(0, "lines"),
    legend.position = "none"
  )+
  scale_y_discrete(expand = expansion(add = c(50, 50)))

p12

############# original regions ~ all drugs ###################

plot13 <- read_xlsx("Daniella/brainstr_su/results/post-hoc/4_originalbrain_alldrugs_ctrlThck.xlsx") %>%
  mutate(drug_type_order = case_when(
    xvar %in% c("audit_c", "SSAGA_Alc_D4_Dp_Dx", "onset_alc", 
                "Breathalyzer_Over_05", "Breathalyzer_Over_08") ~ 1,
    xvar %in% c("SSAGA_Mj_Ab_Dep", "onset_thc", "thc_heavy", "thc_user", 
                "SSAGA_Mj_Times_Used", "THC") ~ 2,
    xvar %in% c("onset_tobac", "tobac_heavy", "tobac_user") ~ 3,
    xvar %in% c("onset_illic", "illic_max", "illic_user", "SSAGA_Times_Used_Illicits", 
                "Cocaine", "Opiates", "Amphetamines", "MethAmphetamine", "Oxycontin") ~ 4,
    TRUE ~ 5),
    drugtype = case_when(
      drug_type_order == 1 ~ "Alcohol",
      drug_type_order == 2 ~ "Marijuana",
      drug_type_order == 3 ~ "Tobacco",
      drug_type_order == 4 ~ "Illicit Drugs"))%>%
  arrange(drug_type_order, xvar, group_id) %>%
  mutate(ypos_factor = factor(paste(xvar, y.var), 
                              levels = unique(paste(xvar, y.var))),
         stroke = ifelse(pfdr < 0.05, 1, 0))

p13 <- ggplot(plot13, aes(x = -log10(`Pr(>|t|)`), y = ypos_factor, fill = xvar)) +
  geom_point(size = 1.5, shape = 21, stroke = plot13$stroke) + 
  theme_classic(base_size = 7)+
  scale_x_continuous(
    breaks = breaks,
    labels = pvals,
    expand = expansion(mult = c(0.01, 0.05)),
    limits = c(0, 5.4) 
  ) +
  scale_fill_manual(values = xvar_colors) +
  facet_wrap(~ drugtype, scales = "free_y", space = "free_y", ncol = 1, strip.position = "left")+
  xlab(expression(italic(p)~"(uncorrected, log scale)"))+
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 6),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 360),
    strip.background = element_rect(color = NA, fill = NA),
    panel.spacing = unit(0, "lines"),
    legend.position = "none"
  )+
  scale_y_discrete(expand = expansion(add = c(5, 5)))
p13

####### brain ~ drugs comparison ###################

p14 <- (p11 | p12 | p13)+
  plot_annotation(tag_levels = 'A')&
  theme(plot.tag = element_text(size = 10, face = "bold"))
p14
  
ggsave("Daniella/images/eFigure5.png", p14, dpi = 500, units = "mm", width = 180, height = 60)
ggsave("Daniella/images/eFigure5.pdf", p14, units = "mm", width = 180, height = 60)
ggsave("Daniella/images/eFigure5.jpeg", p14, dpi = 500, units = "mm", width = 180, height = 60)



##### heavy use Upset Plot ######################

heavy_dat <- poly_dat %>%
  mutate(
    auditmax = ifelse(audit_c > 7, 1, 0),
    illic_heavy = ifelse(illic_max > 4, 1, 0))

heavy_dat <- heavy_dat %>%
  mutate(
    tobac_heavy = ifelse(is.na(tobac_heavy), 0, tobac_heavy),
    illic_heavy = ifelse(is.na(illic_heavy), 0, illic_heavy),
    auditmax = ifelse(is.na(auditmax), 0, auditmax),
    thc_heavy   = ifelse(is.na(thc_heavy), 0, thc_heavy),
    tobac_heavy = tobac_heavy == 1,
    auditmax = auditmax == 1,
    illic_heavy   = illic_heavy == 1,
    thc_heavy = thc_heavy == 1)%>%
  rename(`*Severe Alcohol Use` = auditmax,
         `Heavy Marijuana Use` = thc_heavy, 
         `Heavy Tobacco Use` = tobac_heavy,
         `Heavy Illicit Drug Use` = illic_heavy)


heavy_use <- upset(
  heavy_dat,
  wrap = TRUE,
  name = "Hazardous Drug Use Patterns",
  intersect = c("Heavy Illicit Drug Use","Heavy Tobacco Use","*Severe Alcohol Use","Heavy Marijuana Use"),
  sort_intersections_by = "degree",
  sort_intersections = "ascending",
  stripes = upset_stripes(colors = "grey95", geom=geom_segment(size=3)),
  height_ratio = 0.35,
  themes=upset_modify_themes(
    list('intersections_matrix'=theme(
      axis.text.y=element_text(color = "black", size = 6),
      axis.title=element_text(color = "black", size = 7),
      panel.grid = element_blank()),
      'Intersection size' =theme(axis.text.y=element_text(size = 7)))),
  base_annotations = list(
    "Intersection size" = intersection_size(
      counts = TRUE,
      width = 0.9,
      bar_number_threshold = 1,
      text = list(vjust = -0.5, size = 1.75)) +
      coord_cartesian(ylim = c(0, 800)) + 
      theme_void() +
      theme(
        axis.title.y = element_text(vjust = -40, size = 7),
        axis.line = element_line(color = "white")) +
      ylab("Number of Users by Pattern")
  ),
  set_sizes = (
    upset_set_size() +
      expand_limits(y=315)+
      geom_bar (fill = "grey75", width = 0.6) +
      geom_text(aes(label=..count..), hjust=1.2, stat='count', size = 1.75) +
      theme(
        strip.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_text(size = 7, color = "black"))+
      ylab("Number of Hazardous Users by Drug ")
  ),
  matrix = intersection_matrix(
    geom = geom_point(shape = 19, size = 2),
    segment = geom_segment(
      linetype = "solid", linewidth = 0.75),
    outline_color = list(
      active = NA,     
      inactive = NA)
  ),
  
  ### colors ####
  queries=list(
    upset_query(
      intersect=c("Heavy Tobacco Use", "Heavy Marijuana Use"),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Illicit Drug Use", "Heavy Tobacco Use"),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Illicit Drug Use", "Heavy Marijuana Use"),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Illicit Drug Use", "*Severe Alcohol Use"),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    
    upset_query(
      intersect=c("Heavy Tobacco Use", "*Severe Alcohol Use"),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Marijuana Use", "*Severe Alcohol Use"),
      fill= "#7fb800",
      color = "#7fb800",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Tobacco Use", "*Severe Alcohol Use", "Heavy Marijuana Use"),
      fill="#1f77b4",
      color = "#1f77b4",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Marijuana Use", "Heavy Illicit Drug Use", "Heavy Tobacco Use"),
      fill="#1f77b4",
      color = "#1f77b4",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Tobacco Use", "Heavy Illicit Drug Use", "*Severe Alcohol Use"),
      fill="#1f77b4",
      color = "#1f77b4",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Marijuana Use", "Heavy Illicit Drug Use", "*Severe Alcohol Use"),
      fill="#1f77b4",
      color = "#1f77b4",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Marijuana Use", "Heavy Illicit Drug Use", "*Severe Alcohol Use", "Heavy Tobacco Use"),
      fill= "#c51b8a",
      color = "#c51b8a",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Marijuana Use"),
      fill= "#ffb400",
      color = "#ffb400",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Illicit Drug Use"),
      fill= "#ffb400",
      color = "#ffb400",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("*Severe Alcohol Use"),
      fill= "#ffb400",
      color = "#ffb400",
      only_components=c('intersections_matrix', 'Intersection size')),
    upset_query(
      intersect=c("Heavy Tobacco Use"),
      fill= "#ffb400",
      color = "#ffb400",
      only_components=c('intersections_matrix', 'Intersection size')) 
    
  ))+
  
  ######
theme(
  plot.margin = unit(c(0, 0, 0, 0), "mm"),
  plot.tag = element_text(size = 10, face = "bold", hjust = -1, vjust = 1.5),
  plot.tag.position = c(0,1)
)

heavy_use

ggsave("Daniella/images/eFigure2.png", heavy_use, dpi = 500, units = "mm", width = 180, height = 60)
ggsave("Daniella/images/eFigure2.pdf", heavy_use, units = "mm", width = 180, height = 60)
ggsave("Daniella/images/eFigure2.jpeg", heavy_use, dpi = 500, units = "mm", width = 180, height = 60)

