# BrainStructureSubstanceUse
Contains   code for the manuscript "Brain Structure and Substance Use: Disentangling Risk, Exposure, and Drug-Specific Effects", doi: xxx.

Uses restricted and unrestricted data of the HCP 1200 release.

In each script, analyses/calculations are separated by hashes and include identical headings subheadings listed here. 

## 1_dataframe_setup.R
1.	mean_Thck, SA, and Primary Regions
2.	pull regions from review https://onlinelibrary.wiley.com/doi/full/10.1111/adb.13327
3.	 sib type variables
4.	audit_c recoding
5.	binary lifetime use
6.	drug use onset
7.	heavy use
8.	mediation vars
9.	within-between family variables
10.	numeric gender, final order

## 2_primary_analyses.R
1.	SHARED VERSES UNIQUE EFFECTS
   - a.	(primary brain ROI) ~ mAUDIT-C before mean_Thck covariate
   - b.	(primary brain ROI) ~ mAUDIT-C after mean_Thck covariate
   - c.	Mean_Thck ~ drug use vars (shared)
   - d.	Mean_Thck ~ drug use vars (unique)
   - e.	Anova (mean thck ~ all, mean thck ~ audit/thc)
2.	GENETIC PREDISPOSITION VERSUS ENVIRONMENTAL EXPOSURE
  - a.	Mean_Thck ~ within- & between-SU vars
  - b.	Mean_thck drug use vars (unique) + drg use control
  - c.	within & between mean_thck ~ SU vars
  - d.	SOLAR-Eclipse genetic vs environmental analyses
3.	MEDIATIONS & INTERACTIONS
- a.	Mediation- step 1, associations with mean_Thck
- b.	Mediation- step 2, mean_Thck associations + audic_c and thc_user
- c.	Mediation- step 3, mediation test
- d.	Interactions (drug, age, sex, birth control)

## 3_posthoc_analyses.R
1.	mean_Thck~ biological drug tests and other 2ndary vars 
2.	allbrain~alldrugs (before mean_Thck covar)
3.	allbrain~alldrugs (after mean_Thck covar)
4.	original brain roi ~ all drugs (after mean_Thck covar)

## 4_plots.R
1.	PRIMARY MATERIALS PLOTS
- a.	SU endorsement
- b.	region ~ maudit c
- c.	mean_Thck ~ SU vars
- d.	mean_Thck ~ wtn/btwn fam SU
- e.	SU vars ~ wtn/btwn mean_Thck
- f.	SOLAR-Eclipse heritability & variance component corr
- g.	combined wtn/btwn & solar analyses 
2.	SUPPLEMENTARY MATERIALS PLOTS
- a.	sample characteristics
- b.	mean_thck ~ primary + 2ndary SU vars
- c.	unique mean_thck ~ wtn/btwn SU
- d.	mediation test
- e.	all brain ~ all drugs (no mean_thck covar)
- f.	all brain ~ all drugs + meanthck
- g.	original regions ~ all drugs
- h.	brain ~ drugs comparison
- i.	upset plots- poly use
- j.	heavy use Upset Plot
- k.	mean_thck ~ audit residual scatter 


