
#---------------------------------------------------------------------------------------------------------------
#---------- This runs the main specification for the paper
#---------------------------------------------------------------------------------------------------------------


my_packages <- c("tidyverse","magrittr","broom","stargazer","lmtest","lfe")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)
library(stargazer)
map<-purrr::map #For the nice iterations
select<-dplyr::select

exclude_bocas_del_toro = TRUE 
remove_east = TRUE

path_in <- "Data/"
########################################################################
################################ LOAD ################################
########################################################################
source("Code/general_options.R")  

#spreadbd.df <- readRDS(paste0(path_in,"spreadbd_df.rds"))
spreadbd.df <- readRDS(paste0(path_in,"spreadbd_df_linear_0.rds"))


if(remove_east) {
  message("removing Eastern Panama (islands are always removed)")
  
  spreadbd.df %<>%
    filter( !(Province %in% c("Comarca Guna Yala","Darien","Panama")) ) 
  
}

if(exclude_bocas_del_toro) {
  message("removing Bocas Del Toro")
  
  spreadbd.df %<>%
    filter(!County=="Bocas Del Toro")
}

message(length(unique(spreadbd.df$ID)), " spatial units kept")

#---------------- Stats for interpretation

summary(subset(spreadbd.df, Country=='CRI' & Year>=1991 & Year<=2001, select=c(Malaria_pt)))
summary(subset(spreadbd.df, Country=='PAN' & Year>=2002 & Year<=2007, select=c(Malaria_pt)))

  
#---------------- Run regression

main_reg <- felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
  Temp_av + #Temp_min_annual + Temp_max_annual + 
  bd_min_b6max + bd_min_b5 + bd_min_b4 + bd_min_b3 + 
  bd_min_b2 + bd_min_0 + bd_min_f1 + bd_min_f2 + bd_min_f3 + bd_min_f4 + bd_min_f5 + 
  bd_min_f6 + bd_min_f7 + bd_min_f8 + bd_min_f9 + bd_min_f10 + bd_min_f11max | Year + ID | 0 | ID,
  data = spreadbd.df)

summary(main_reg)  
main_mean <- mean(spreadbd.df$Malaria_pt) %>% round(2)
main_clusters <- unique(spreadbd.df$ID) %>% length()


coefs_reg_cleaned <- summary(main_reg)$coefficients %>% #Only keepthe coefs for b5 to f10
  as_tibble() %>%
  mutate(Var_name = rownames(summary(main_reg)$coefficients)) %>%
  filter(str_detect(Var_name,"bd_min")) %>%
  filter(!Var_name %in% c("bd_min_b6max","bd_min_f11max")) %>%
  add_row(Estimate = 0, #added following the update of the tibble packages
          `Cluster s.e.` = 0,
          `t value` = 0,
          `Pr(>|t|)` = 0,
          .before = 5) %>%
  mutate(Year = seq(-5,10)) %>%
  rename(se_clustered = `Cluster s.e.`) 
  
color_plot = "royalblue4"  

plot_coefs <- function(se_CV) {
  ggplot(coefs_reg_cleaned, aes(x=Year,y=Estimate)) + 
    geom_ribbon(aes(ymin=Estimate-se_CV*se_clustered, ymax=Estimate+se_CV*se_clustered),  fill = color_plot, alpha=0.2) + #fill=Lambda
    geom_point(color = color_plot, 
               position=position_dodge(0.01),
               cex=4) + # show.legend = TRUE
    geom_line(aes(x=Year,y=Estimate-se_CV*se_clustered), color = color_plot,linetype = "longdash")+
    geom_line(aes(x=Year,y=Estimate+se_CV*se_clustered), color = color_plot,linetype = "longdash")+
    geom_line(color = color_plot)+
    xlab("Year relative to amphibian decline") + 
    ylab("Effect on malaria cases per 1,000 population") +
    geom_hline(yintercept=0) +
    scale_x_continuous(minor_breaks = seq(-5,10,1), breaks = seq(-5,10,5), limits = c(-5, 10), expand = c(.009, .007)) + 
    theme_jo +
    theme(panel.grid.major = element_line(colour="gray75", size = (.75)))
}


# Both CI
ggplot(coefs_reg_cleaned, aes(x=Year,y=Estimate)) + #color=Lambda in AES
  geom_ribbon(aes(ymin=Estimate-1.96*se_clustered, ymax=Estimate+1.96*se_clustered),  fill = color_plot, alpha=0.2) + #fill=Lambda
  geom_point(color = color_plot, #color = Lambda
             position=position_dodge(0.01),
             cex=4) + # show.legend = TRUE
  geom_line(aes(x=Year,y=Estimate-1.645*se_clustered), color = color_plot,linetype = "longdash")+
  geom_line(aes(x=Year,y=Estimate+1.645*se_clustered), color = color_plot,linetype = "longdash")+
  geom_line(aes(x=Year,y=Estimate-1.96*se_clustered), color = color_plot,linetype = "dotted")+
  geom_line(aes(x=Year,y=Estimate+1.96*se_clustered), color = color_plot,linetype = "dotted")+
  geom_line(color = color_plot)+
  xlab("Year relative to amphibian decline") + 
  ylab("Effect on malaria cases per 1,000 population") +
  geom_hline(yintercept=0) +
  scale_x_continuous(minor_breaks = seq(-5,10,1), breaks = seq(-5,10,5), limits = c(-5, 10), expand = c(.009, .007)) + 
  theme_jo +
  theme(panel.grid.major = element_line(colour="gray75", size = (.75)))

relSize = 15 #relative size
figDestName <- paste0("Output/model_main_specification_paper/model_main_specification_",color_plot,"_90_95_CI.jpg")
ggsave(figDestName , width = relSize, height =relSize*0.5) #Saving the plot


#-------------------- Now output regression table
stargazer(main_reg,
          out = "Output/model_main_specification_paper/model_main_specification.tex", header = F, title = "Event study, preferred model specification",
          single.row = T,
          no.space = T,
          #keep = "^((?!factor).)*$", #Remove all variables that contain string "factor"
          perl = TRUE,
          dep.var.labels = c("Malaria per thousand"),
          #add.lines = list(c("Year FE", "Yes"),
          #                 c("County FE", "Yes")),
          add.lines = list(c("Mean outcome", main_mean),
                           c("No. clusters", main_clusters)),
                           #c("Mean control",mean_control_treated),
          #covariate.labels = c("Private", "Length of mains","No of connections","Average pressure","Year 2017", "Year 2018", "Year 2019"),
          notes = c("Standard errors clustered at the county level in parentheses."),
          omit.stat = c("rsq","ser")
)
