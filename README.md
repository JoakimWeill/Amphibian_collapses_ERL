# Package for "Amphibian Collapses Increased Malaria Incidence in Central America" 
by Michael R. Springborn, Joakim A. Weill, Karen R. Lips, Roberto Ibanez, and Aniruddha Ghosh


## General ploting options

file_copy_here("Code/general_options.R")
file_copy_here("Code/draw_crosshatch.R")

## Generating outputs

# Main text

#  Fig 1: aggregated malaria, lines 
Scripts/plot_lines_number_malaria_cases_paper.R

#  Fig2: Map,Bd-driven amphibian declines timing
Scripts/plot_spread_optimal_model_years_decline_paper.R

#  Fig 3: Event study
Scripts/model_main_specification_paper.R

#  Table 1: Main reg table
Scripts/models_main_robustness_checks_paper.R


#=== Appendix

#  Table S1: Summary Table and Table S2: Balance Table
Scripts/summary_table.R

# Table S3 (temperatures robustness checks), S4 (Land use + weather), S5 (county exclusion)
#Same script as Table 1

#  Fig S1: Need Ani's code 

#  Fig S2: Map raster Thinplate Spline
Scripts/plot_spline_interpolation_raster_paper.R

# Fig S3: Event studies with alternative models for elevation-dependent spread
Scripts/models_event_study_many_spreads.R

#  Fig S4: Thinplate Spline Event Studies
Scripts/models_splines_paper.R

#  Fig S5: Bacon-Goodman decomposition
Scripts/lardon.R, script to make the decomposition faster
Scripts/model_bacondecomp.R

# Fig S6: 
Scripts/plot_lines_mitigExpend_country_paper.R


## Data files

- sp object Costa Rica and Panama 
Data/cr_pan_agg_sp.rds

#- spread model in raster format for fig 2
Data/optimal_spread_model_extracted_dataframe_rho_linear_0.rds

- Observed dates of amphibian declines
Data/sites_declines_sp.rds

#-- thinplate splines models used in appendix
Data/preBigdf_to_merge.rds

#-- Mitigation efforts at the country level
Data/MalariaExpendSprayDataCRPan_cleaned.csv

- merged datasets 
Data/spread_bd_df_*.rds, where * is the choice of elevation model for the spatial spread (linear or tabletop) as well as the strength of elevation (varies between 0 and 1)





