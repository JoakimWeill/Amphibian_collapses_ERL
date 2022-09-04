#-------------------------------------------------------------------------------------------
#--------------------- Run spline robustness tests for figure S4 ----------------------
#-------------------------------------------------------------------------------------------

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","sf","foreign","lfe")
library(DataCombine)
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select

source("Scripts/general_options.R")  
Big.df <- readRDS("Data/merged_splines_data.rds")

#-------------------------------------------- Run the models ----------------------------------------------------------------

reg_all_splines <- list(rep(NA,nrow(Big.df)))

for (j in 1:nrow(Big.df)) {
  #run regression
  reg_spline <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
                       Temp_av +#Temp_min_annual + Temp_max_annual + 
                       bd_min_b6max + bd_min_b5 + bd_min_b4 + bd_min_b3 + 
                       bd_min_b2 + bd_min_0 + bd_min_f1 + bd_min_f2 + bd_min_f3 + bd_min_f4 + bd_min_f5 + 
                       bd_min_f6 + bd_min_f7 + bd_min_f8 + bd_min_f9 + bd_min_f10 + bd_min_f11max | Year + ID | 0 | ID,
                     data = Big.df$myData[[j]])
  
  #clean the coefs we are going to plot, add baseline, and get the lambda (spline penalty term) 
  coefs_reg_cleaned <- summary(reg_spline)$coefficients %>% #Only keepthe coefs for b5 to f10
    as_tibble() %>%
    mutate(Var_name = rownames(summary(reg_spline)$coefficients)) %>%
    filter(str_detect(Var_name,"bd_min")) %>%
    filter(!Var_name %in% c("bd_min_b6max","bd_min_f11max")) %>%
    add_row(Estimate = 0, #added following the update of the tibble packages
            `Cluster s.e.` = 0,
            `t value` = 0,
            `Pr(>|t|)` = 0,
            .before = 5) %>%
    #InsertRow(c(0,0,0,0), 5) %>% #insert baseline row
    mutate(Year = seq(-5,10)) %>%
    rename(se_clustered = `Cluster s.e.`) %>%
    mutate(Lambda = Big.df$lambda[j])
  
  reg_all_splines[[j]] <-coefs_reg_cleaned
  
  rm(reg_spline)
  
}


reg_all_splines <- bind_rows(reg_all_splines)

reg_all_splines %<>%
  mutate(Lambda = ifelse(Lambda == 12.472, 12.5,Lambda))

reg_all_splines %<>%
  mutate(Lambda = as_factor(Lambda))
#-------------------------------------------- Plot the coefficients ----------------------------------------------------------------


ggplot(reg_all_splines %>% filter(Lambda!=50), aes(x=Year,y=Estimate)) + #color=Lambda in AES
  geom_ribbon(aes(ymin=Estimate-1.96*se_clustered, ymax=Estimate+1.96*se_clustered, fill = Lambda), alpha=0.2) + #fill=Lambda
  geom_point(aes(color = Lambda) , #color = Lambda
             #position=position_dodge(0.01),
             cex=2,
             alpha=0.8) + # show.legend = TRUE
  geom_line(aes(x=Year,y=Estimate-1.96*se_clustered, color = Lambda), linetype = "longdash")+
  geom_line(aes(x=Year,y=Estimate+1.96*se_clustered, color = Lambda), linetype = "longdash")+
  geom_line(aes(color = Lambda)) +
  xlab("Year relative to amphibian decline") + 
  ylab("Effect on malaria cases per 1,000 population") +
  geom_hline(yintercept=0) +
  scale_x_continuous(minor_breaks = seq(-5,10,1), breaks = seq(-5,10,5), limits = c(-5, 10), expand = c(.009, .007)) + 
  #  scale_x_discrete(limits= c(-5,-3,-1,1,3,5,7,10)) + #Adjusting axis
  theme_jo +
  theme(panel.grid.major = element_line(colour="gray75", size = (.75)), 
        legend.position="bottom",
        legend.box = "horizontal")+
  guides(fill = guide_legend(nrow = 1)) +
  labs(color = "\u03BB parameter", fill = "\u03BB parameter" )


relSize = 15 #relative size
figDestName <- paste0("Output/models_event_study_splines/multiple_splines.jpg")
ggsave(figDestName , width = relSize, height =relSize*0.5) #Saving the plot
