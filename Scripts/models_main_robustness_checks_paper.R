
#---------------------------------------------------------------------------------------------------------------
#---------- This runs the main robustness table of the paper
#---------------------------------------------------------------------------------------------------------------

my_packages <- c("tidyverse","magrittr","broom","stargazer","lmtest","lfe")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)
map<-purrr::map 
select<-dplyr::select


unique <- base::unique

########################################################################
################################ LOAD ################################
########################################################################
source("Scripts/general_options.R")
spreadbd.df <- readRDS(paste0("Data/","spreadbd_df_linear_0.rds"))

message("removing Eastern Panama and Bocas del Toro (islands are always removed)")

spreadbd.df_preferred <- spreadbd.df %>%
  filter( !(Province %in% c("Comarca Guna Yala","Darien","Panama")) ) %>%
  filter(!County=="Bocas Del Toro")

message(length(unique(spreadbd.df_preferred$ID)), " spatial units kept in preferred model")

message(length(unique(spreadbd.df$ID)), " spatial units kept otherwise")

########################################################################################################################
#---------------- Run 4 regressions: preferred specification, with omitted regions, wioth weights, with BD AV
########################################################################################################################



#----------- Preferred Specification
spreadbd.df_preferred_min <- spreadbd.df_preferred %>%
  select(ID, Country, Province, County, Year, Malaria_pt, Population,Tree_cover, Bare_ground, Precip_annual, Temp_min_annual, Temp_max_annual, Temp_av, contains("bd_min") )

names(spreadbd.df_preferred_min) <- str_replace(names(spreadbd.df_preferred_min),"bd_min_","bd_") 


preferred_specification <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
                   Temp_av + #Temp_min_annual + Temp_max_annual + 
                   bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                   bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                   bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                 data = spreadbd.df_preferred_min)

summary(preferred_specification)  

preferred_specification_mean <- mean(spreadbd.df_preferred_min$Malaria_pt) %>% round(2)
preferred_specification_clusters <- unique(spreadbd.df_preferred_min$ID) %>% length

#----------- With omitted regions included

spreadbd.df_all <- spreadbd.df %>%
  select(ID, Country, Province, County, Year, Malaria_pt, Tree_cover, Bare_ground, 
         Precip_annual, Temp_min_annual, Temp_max_annual, Temp_av, contains("bd_min") )
names(spreadbd.df_all) <- str_replace(names(spreadbd.df_all),"bd_min_","bd_") 

specification_all_obs <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
                                  Temp_av + #Temp_min_annual + Temp_max_annual + 
                                  bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                                  bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                                  bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                                data = spreadbd.df_all)

summary(specification_all_obs)  

specification_all_obs_mean <- mean(spreadbd.df_all$Malaria_pt) %>% round(2)
specification_all_obs_clusters <- unique(spreadbd.df_all$ID) %>% length

#----------- With average


spreadbd.df_preferred_av <- spreadbd.df_preferred %>%
  select(ID, Country, Province, County, Year, Malaria_pt, Tree_cover, Bare_ground, Precip_annual, 
         Temp_min_annual, Temp_max_annual, Temp_av, contains("bd_av") )

names(spreadbd.df_preferred_av) <- str_replace(names(spreadbd.df_preferred_av),"bd_av_","bd_") 

specification_av <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
                                Temp_av + #Temp_min_annual + Temp_max_annual + 
                                bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                                bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                                bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                              data = spreadbd.df_preferred_av)

summary(specification_av)  


#----------- With weights


spreadbd.df_weights <- spreadbd.df_preferred_min %>%
  #select(Country,Province,County,Year,Population) %>%
  group_by(Year) %>%
  mutate(Population_year = sum(Population)) %>%
  ungroup() %>%
  mutate(County_pop_share = Population/Population_year) %>%
  mutate(weights_1990 = ifelse(Year==1990,County_pop_share,NA)) %>%
  mutate(weights_2000 = ifelse(Year==2000,County_pop_share,NA)) %>%
  group_by(Country,Province,County) %>%
  mutate(weights_1990 = mean(weights_1990,na.rm=T)) %>%
  mutate(weights_2000 = mean(weights_2000,na.rm=T)) 

specification_weights <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
                           Temp_av + #Temp_min_annual + Temp_max_annual + 
                           bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                           bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                           bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                         data = spreadbd.df_weights,
                         weights = spreadbd.df_weights$County_pop_share)

summary(specification_weights)  



#----------- Old Regression_table (before June 24, 2022)

# Manual SOLUTION TO THE IS.NA ISSUE (Package update): https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53

# detach("package:stargazer",unload=T)
# # Delete it
# remove.packages("stargazer")
# # Download the source
# download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# # Unpack
# untar("stargazer_5.2.3.tar.gz")
# # Read the sourcefile with .inside.bracket fun
# stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# # Move the length check 5 lines up so it precedes is.na(.)
# stargazer_src[1990] <- stargazer_src[1995]
# stargazer_src[1995] <- ""
# # Save back
# writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# # Compile and install the patched package
# install.packages("stargazer", repos = NULL, type="source")
library(stargazer)

stargazer(preferred_specification,specification_all_obs,specification_av,specification_weights,
          out = "Output/models_main_robustness_checks_paper/model_main_robustness_checks.tex", 
          header = F, 
          title = "Regression results for the preferred specification and robustness checks",
          single.row = T,
          no.space = T,
          #column.labels = c("Preferred specification","All observations","Average date of decline","Weighted regression"),
          column.labels = c("\\thead{Preferred \\\ specification}","\\thead{All \\\ observations}","\\thead{Average date \\\ of decline}","\\thead{Weighted \\\ regression}"),
          #keep = "^((?!factor).)*$", #Remove all variables that contain string "factor"
          perl = TRUE,
          #dep.var.labels = c("Annual county-level number of malaria cases per 1,000 population"),
          #label = "tbl:regResultsAll",
          dep.var.labels = "",
          dep.var.caption = c("\\textit{Dependent variable:} malaria cases per 1,000 population"),
          add.lines = list(c("Mean outcome", preferred_specification_mean,specification_all_obs_mean,preferred_specification_mean,preferred_specification_mean),
                           c("No. clusters", preferred_specification_clusters,specification_all_obs_clusters,preferred_specification_clusters,preferred_specification_clusters)),
          #c("Mean control",mean_control_treated),
          covariate.labels = c("Tree cover","Bare ground","Precipitation","Av. Temp.", #"Min. Temp.","Max. Temp.",
                               "$\\hat{\\gamma}_{-6^-}$","$\\hat{\\gamma}_{-5}$","$\\hat{\\gamma}_{-4}$",
                               "$\\hat{\\gamma}_{-3}$",  "$\\hat{\\gamma}_{-2}$","$\\hat{\\gamma}_{0}$",
                               "$\\hat{\\gamma}_{1}$",   "$\\hat{\\gamma}_{2}$", "$\\hat{\\gamma}_{3}$",
                               "$\\hat{\\gamma}_{4}$",   "$\\hat{\\gamma}_{5}$", "$\\hat{\\gamma}_{6}$",
                               "$\\hat{\\gamma}_{7}$",   "$\\hat{\\gamma}_{8}$", "$\\hat{\\gamma}_{9}$",
                               "$\\hat{\\gamma}_{10}$",  "$\\hat{\\gamma}_{11^+}$"),
          notes = c("Standard errors clustered at the county level in parentheses."),
          omit.stat = c("rsq","ser")
)




#------------------------------------------------------------------------------------------------------------
#------------------ Expanded regression table following ERL referee comments ---------------------------
#------------------------------------------------------------------------------------------------------------

#Function to exclude 20 or 40 counties and output table
robustness_tests_models_fct <- function(data_to_use,variable_for_robustness,title_table,columns_names_table,output_tex) {
  
  # Model excluding countie with highest density
  specification_top_20_excluded <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
                                                 Temp_av + #Temp_min_annual + Temp_max_annual + 
                                                 bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                                                 bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                                                 bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                                               data = filter(data_to_use,{{variable_for_robustness}}>20))

  preferred_specification_20_mean <- filter(data_to_use,{{variable_for_robustness}}>20) %>% 
    pull(Malaria_pt) %>% 
    mean %>%
    round(2)
  
  preferred_specification_20_clusters <- filter(data_to_use,{{variable_for_robustness}}>20) %>% 
    pull(ID) %>%
    unique() %>% 
    length()
  
  specification_top_40_excluded <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
                                                 Temp_av + #Temp_min_annual + Temp_max_annual + 
                                                 bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                                                 bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                                                 bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                                               data = filter(data_to_use,{{variable_for_robustness}}>40))
  
  preferred_specification_40_mean <- filter(data_to_use,{{variable_for_robustness}}>40) %>% 
    pull(Malaria_pt) %>% 
    mean %>%
    round(2)
  
  preferred_specification_40_clusters <- filter(data_to_use,{{variable_for_robustness}}>40) %>% 
    pull(ID) %>%
    unique() %>% 
    length()
  
  stargazer(preferred_specification,specification_top_20_excluded,specification_top_40_excluded,
            out = output_tex,
            header = F, 
            title = title_table,
            single.row = T,
            no.space = T,
            #column.labels = c("Preferred specification","All observations","Average date of decline","Weighted regression"),
            column.labels = columns_names_table,
            #keep = "^((?!factor).)*$", #Remove all variables that contain string "factor"
            perl = TRUE,
            #dep.var.labels = c("Annual county-level number of malaria cases per 1,000 population"),
            #label = "tbl:regResultsAll",
            dep.var.labels = "",
            dep.var.caption = c("\\textit{Dependent variable:} malaria cases per 1,000 population"),
            add.lines = list(c("Mean outcome", preferred_specification_mean,preferred_specification_20_mean,preferred_specification_40_mean),
                             c("No. clusters", preferred_specification_clusters,preferred_specification_20_clusters,preferred_specification_40_clusters)),
            covariate.labels = c("Tree cover","Bare ground","Precipitation","Av. Temp.",#"Min. Temp.","Max. Temp.",
                                 "$\\hat{\\gamma}_{-6^-}$","$\\hat{\\gamma}_{-5}$","$\\hat{\\gamma}_{-4}$",
                                 "$\\hat{\\gamma}_{-3}$",  "$\\hat{\\gamma}_{-2}$","$\\hat{\\gamma}_{0}$",
                                 "$\\hat{\\gamma}_{1}$",   "$\\hat{\\gamma}_{2}$", "$\\hat{\\gamma}_{3}$",
                                 "$\\hat{\\gamma}_{4}$",   "$\\hat{\\gamma}_{5}$", "$\\hat{\\gamma}_{6}$",
                                 "$\\hat{\\gamma}_{7}$",   "$\\hat{\\gamma}_{8}$", "$\\hat{\\gamma}_{9}$",
                                 "$\\hat{\\gamma}_{10}$",  "$\\hat{\\gamma}_{11^+}$"),
            notes = c("Standard errors clustered at the county level in parentheses."),
            omit.stat = c("rsq","ser")
  )
  
}


library(sf)
#Excluding highest density
counties_density <- readRDS("Code/Clean/cleaning_CR_PAN_map/cr_pan_agg_sp.rds") %>% st_as_sf() %>%
  mutate(Area_km2 = as.numeric(st_area(.)/1000000)) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  left_join(spreadbd.df_preferred_min %>% 
              filter(Year == 2010) %>%
              select(ID,Country,Province,County,Population)
  ) %>%
  mutate(Pop_per_km2 = Population/Area_km2) %>%
  arrange(-Pop_per_km2) %>%
  mutate(Density_order = row_number()) %>%
  select(-Population)

spreadbd.df_preferred_min_lower_density <- spreadbd.df_preferred_min %>%
  left_join(counties_density) 

robustness_tests_models_fct(data_to_use = spreadbd.df_preferred_min_lower_density,variable_for_robustness = Density_order,
                            title_table = "Regression results for the preferred specification and density robustness checks",
                            columns_names_table = c("\\thead{Preferred \\\ specification}","\\thead{Top 20 pop. \\\ density excluded}","\\thead{Top 40 pop. \\\ density excluded}"),
                            output_tex = "Output/models_main_robustness_checks_paper/model_robustness_checks_density_ERL.tex")

#Excluding lowest elevation
counties_elevation <- readRDS("Code/Clean/cleaning_elevation/CR_PAN_elevation_df.rds") %>%
  semi_join(spreadbd.df_preferred_min %>% #semi join to get the count of 136
              select(c("Country", "Province", "County"))) %>%
  arrange(Elevation) %>%
  mutate(Elevation_order = row_number()) 

spreadbd.df_preferred_min_higher_elev <- spreadbd.df_preferred_min %>%
  left_join(counties_elevation) #by = c("Country", "Province", "County")

robustness_tests_models_fct(data_to_use = spreadbd.df_preferred_min_higher_elev,variable_for_robustness = Elevation_order,
                            title_table = "Regression results for the preferred specification and elevation robustness checks",
                            columns_names_table = c("\\thead{Preferred \\\ specification}","\\thead{Bottom 20 \\\ elevation excluded}","\\thead{Bottom 40 \\\ elevation excluded}"),
                            output_tex = "Output/models_main_robustness_checks_paper/model_robustness_checks_elevation_ERL.tex")


#Excluding highest average temperature
counties_av_temp <- spreadbd.df_preferred_min %>%
  select(Country,Province,County,Temp_av) %>%
  group_by(Country,Province,County) %>%
  summarise(Overall_temp_av = mean(Temp_av) ) %>%
  arrange(-Overall_temp_av) %>%
  ungroup() %>%
  mutate(Av_temp_order = row_number()) 

spreadbd.df_preferred_min_temp <- spreadbd.df_preferred_min %>%
  left_join(counties_av_temp)  #by = c("Country", "Province", "County")


robustness_tests_models_fct(data_to_use = spreadbd.df_preferred_min_temp,variable_for_robustness = Av_temp_order,
                            title_table = "Regression results for the preferred specification and temperature robustness checks",
                            columns_names_table = c("\\thead{Preferred \\\ specification}","\\thead{Top 20 \\\ av. temp.  excluded}","\\thead{Top 40 \\\ av. temp. excluded}"),
                            output_tex = "Output/models_main_robustness_checks_paper/model_robustness_checks_av_temp_ERL.tex")

#check

spreadbd.df_preferred_min_higher_elev %>% filter(Elevation_order>135) %>% pull(Elevation) %>% mean
spreadbd.df_preferred_min_higher_elev %>% filter(Elevation_order>20) %>% pull(Malaria_pt) %>% mean
spreadbd.df_preferred_min_temp %>% filter(Av_temp_order>20) %>% pull(Malaria_pt) %>% mean
  

#------------------------------------------------------------------------------------------------------------
#------------------ Expanded regression table following ERL referee comments, #2 (with mean temp) ---------------------------
#------------------------------------------------------------------------------------------------------------


spreadbd.df_preferred_min %<>%
  mutate(Temp_diff = Temp_max_annual - Temp_min_annual,
         Temp_av_sq = Temp_av^2,
         Pop_1000 = Population / 1000)

with_mean2 <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + Temp_av + Temp_av_sq +
                    bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                    bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                    bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                  data = spreadbd.df_preferred_min)

with_min_max <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + #Temp_av +
                       Temp_min_annual + Temp_max_annual + 
                       bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                       bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                       bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                     data = spreadbd.df_preferred_min)


with_diff <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + Temp_diff +
                     bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                     bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                     bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                   data = spreadbd.df_preferred_min)




stargazer(preferred_specification,with_mean2,with_min_max,with_diff,
          out = "Output/models_main_robustness_checks_paper/model_robustness_checks_temperature.tex", 
          header = F, 
          title = "Regression results for the preferred specification and temperature robustness checks",
          single.row = T,
          no.space = T,
          perl = TRUE,
          dep.var.labels = "",
          dep.var.caption = c("\\textit{Dependent variable:} malaria cases per 1,000 population"),
          add.lines = list(c("Mean outcome", preferred_specification_mean,preferred_specification_mean,preferred_specification_mean,preferred_specification_mean),
                           c("No. clusters", preferred_specification_clusters,preferred_specification_clusters,preferred_specification_clusters,preferred_specification_clusters)),
         covariate.labels = c("Tree cover","Bare ground","Precipitation", 
                              "Av. Temp", "Av. Temp (sq.)", 
                              "Min. Temp.","Max. Temp.","Max-Min Temp",
                              "$\\hat{\\gamma}_{-6^-}$","$\\hat{\\gamma}_{-5}$","$\\hat{\\gamma}_{-4}$",
                              "$\\hat{\\gamma}_{-3}$",  "$\\hat{\\gamma}_{-2}$","$\\hat{\\gamma}_{0}$",
                              "$\\hat{\\gamma}_{1}$",   "$\\hat{\\gamma}_{2}$", "$\\hat{\\gamma}_{3}$",
                              "$\\hat{\\gamma}_{4}$",   "$\\hat{\\gamma}_{5}$", "$\\hat{\\gamma}_{6}$",
                              "$\\hat{\\gamma}_{7}$",   "$\\hat{\\gamma}_{8}$", "$\\hat{\\gamma}_{9}$",
                              "$\\hat{\\gamma}_{10}$",  "$\\hat{\\gamma}_{11^+}$"),
          notes = c("Standard errors clustered at the county level in parentheses."),
          omit.stat = c("rsq","ser")
)







# New S4 
with_pop <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + Temp_av +
                   Pop_1000 +
                   bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                   bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                   bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                 data = spreadbd.df_preferred_min)

no_luc <- felm(Malaria_pt ~ Precip_annual + Temp_av +
                 bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                 bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                 bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
               data = spreadbd.df_preferred_min)

no_weather <- felm(Malaria_pt ~ Tree_cover + Bare_ground + 
                   bd_b6max + bd_b5 + bd_b4 + bd_b3 + 
                   bd_b2 + bd_0 + bd_f1 + bd_f2 + bd_f3 + bd_f4 + bd_f5 + 
                   bd_f6 + bd_f7 + bd_f8 + bd_f9 + bd_f10 + bd_f11max | Year + ID | 0 | ID,
                 data = spreadbd.df_preferred_min)


stargazer(preferred_specification,with_pop,no_luc,no_weather,
          out = "Output/models_main_robustness_checks_paper/model_robustness_checks_additional.tex", 
          header = F, 
          title = "Regression results for the preferred specification and additional robustness checks",
          single.row = T,
          no.space = T,
          perl = TRUE,
          dep.var.labels = "",
          dep.var.caption = c("\\textit{Dependent variable:} malaria cases per 1,000 population"),
          add.lines = list(c("Mean outcome", preferred_specification_mean,preferred_specification_mean,preferred_specification_mean,preferred_specification_mean),
                           c("No. clusters", preferred_specification_clusters,preferred_specification_clusters,preferred_specification_clusters,preferred_specification_clusters)),
          covariate.labels = c("Tree cover","Bare ground","Precipitation", 
                               "Av. Temp","Pop. level ('000)",
                               "$\\hat{\\gamma}_{-6^-}$","$\\hat{\\gamma}_{-5}$","$\\hat{\\gamma}_{-4}$",
                               "$\\hat{\\gamma}_{-3}$",  "$\\hat{\\gamma}_{-2}$","$\\hat{\\gamma}_{0}$",
                               "$\\hat{\\gamma}_{1}$",   "$\\hat{\\gamma}_{2}$", "$\\hat{\\gamma}_{3}$",
                               "$\\hat{\\gamma}_{4}$",   "$\\hat{\\gamma}_{5}$", "$\\hat{\\gamma}_{6}$",
                               "$\\hat{\\gamma}_{7}$",   "$\\hat{\\gamma}_{8}$", "$\\hat{\\gamma}_{9}$",
                               "$\\hat{\\gamma}_{10}$",  "$\\hat{\\gamma}_{11^+}$"),
          notes = c("Standard errors clustered at the county level in parentheses."),
          omit.stat = c("rsq","ser")
)










