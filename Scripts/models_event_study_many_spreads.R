
#---------------------------------------------------------------------------------------------------------------
#---------- This runs the various spread models for Fig S3 (includes the elevation-dependent ones)
#---------------------------------------------------------------------------------------------------------------


my_packages <- c("tidyverse","magrittr","broom","stargazer","lmtest","lfe")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)
library(stargazer)
map<-purrr::map #
select<-dplyr::select

exclude_bocas_del_toro = TRUE 
remove_east = TRUE

path_in <- "Data/"
path_out <- "Output/models_event_study_many_spreads/"

########################################################################
################################ LOAD ################################
########################################################################

source("Scripts/general_options.R")  

all_files <- list.files(path_in)[list.files(path_in) %>% str_detect(".rds")]

all_files.df <- tibble(spread = all_files) %>%
  mutate(rho = str_extract(spread,"(?<=linear|tabletop_).*") %>% 
           str_remove(".rds") %>% 
           str_remove_all("_") %>%
           str_replace("0","0\\.") %>% as.numeric()
         ) %>%
  mutate(spread_model = str_extract(spread,"linear|tabletop"))  %>%
  mutate(df = map(spread, ~readRDS(paste0(path_in,.)) %>% 
                    filter( !(Province %in% c("Comarca Guna Yala","Darien","Panama")) ) %>% #removing Eastern Panama (islands are always removed)"
                    filter(!County=="Bocas Del Toro")
                  )
         ) %>%
  mutate(reg = map(df, ~felm(Malaria_pt ~ Tree_cover + Bare_ground + Precip_annual + 
                              Temp_av + #Temp_min_annual + Temp_max_annual + 
                              bd_min_b6max + bd_min_b5 + bd_min_b4 + bd_min_b3 + 
                              bd_min_b2 + bd_min_0 + bd_min_f1 + bd_min_f2 + bd_min_f3 + bd_min_f4 + bd_min_f5 + 
                              bd_min_f6 + bd_min_f7 + bd_min_f8 + bd_min_f9 + bd_min_f10 + bd_min_f11max | Year + ID | 0 | ID,
                            data = .))) %>%
  mutate(coefs = map(reg, ~summary(.x)$coefficients %>% #Only keepthe coefs for b5 to f10
                       as_tibble() %>%
                       mutate(Var_name = rownames(summary(.x)$coefficients)) %>%
                       filter(str_detect(Var_name,"bd_min")) %>%
                       filter(!Var_name %in% c("bd_min_b6max","bd_min_f11max")) %>%
                       add_row(Estimate = 0, #added following the update of the tibble packages
                               `Cluster s.e.` = 0,
                               `t value` = 0,
                               `Pr(>|t|)` = 0,
                               .before = 5) %>%
                       #InsertRow(c(0,0,0,0), 5) %>% #insert baseline row
                       mutate(Year = seq(-5,10)) %>%
                       rename(se_clustered = `Cluster s.e.`) )
         )
  
all_files.df$coefs[[1]]

message(length(unique(all_files.df$df[[2]]$ID)), " spatial units kept")


all_models <- all_files.df %>%
  select(rho,spread_model,coefs) %>%
  unnest(coefs) %>%
  mutate(Model = paste0(spread_model,", rho = ",rho) ) %>%
  mutate(Model = if_else(Model=="linear, rho = 0", "baseline", Model)) %>%
  mutate(Model = str_replace_all(Model,"rho", "\u03C1")) %>%
  mutate(spread_model = if_else(Model=="baseline", "baseline", spread_model)) 


#Long dash for the dotted ribbon (showing stabdard error) 

  ggplot(all_models, aes(x=Year,y=Estimate, color = Model)) + #color=Lambda in AES
    geom_ribbon(aes(ymin=Estimate-1.96*se_clustered, ymax=Estimate+1.96*se_clustered, fill = Model),  alpha=0.2) + #fill=Lambda
    geom_point(aes(color = Model), #color = Lambda
               position=position_dodge(0.01),
               cex=4) + # show.legend = TRUE
    facet_wrap(~spread_model) +
    geom_line(aes(x=Year,y=Estimate-1.96*se_clustered, color = Model),linetype = "longdash")+
    geom_line(aes(x=Year,y=Estimate+1.96*se_clustered, color = Model),linetype = "longdash")+
    geom_line(aes(color = Model) )+
    xlab("Year relative to amphibian decline") + 
    ylab("Effect on malaria cases per 1,000 population") +
    geom_hline(yintercept=0) +
    scale_x_continuous(minor_breaks = seq(-5,10,1), breaks = seq(-5,10,5), limits = c(-5, 10), expand = c(.009, .007)) + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_jo +
    theme(panel.grid.major = element_line(colour="gray75", size = (.75)),
          legend.position = "bottom",
          strip.text.x = element_text(size = 20),
          panel.spacing = unit(1.5, "lines"))


relSize = 15 #relative size
figDestName <- paste0(path_out,"model_many_spreads_event_study.jpg")
ggsave(figDestName , width = relSize, height =relSize*0.5, dpi = 150) #Saving the plot
