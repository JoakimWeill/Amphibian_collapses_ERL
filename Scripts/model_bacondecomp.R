
#---------------------------------------------------------------------------------------------------------------
#---------- This runs the bacongoodman decomposition
#---------------------------------------------------------------------------------------------------------------

my_packages <- c("doParallel","tidyverse","magrittr","Formula","data.table")
lapply(my_packages, library, character.only = TRUE)
library(bacondecomp)
library(lfe)
library(tictoc)
library(tidyverse)
library(ggthemes)
source("Scripts/lardon.R")
source("Scripts/general_options.R")

#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select


#remove_islands = TRUE
exclude_bocas_del_toro = TRUE #need to change file names too
remove_east = TRUE



########################################################################
################################ LOAD ################################
########################################################################
spreadbd.df <- readRDS("Data/spreadbd_df_linear_0.rds")

glimpse(spreadbd.df)

message("removing Eastern Panama and Bocas del Toro (islands are always removed)")

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




#----------


cores_number = 5
parallel::detectCores() 
registerDoParallel(cores=cores_number)
getDoParWorkers()


remove_na_var <- function(var_to_clean, df) {
  
  cleaned_df <- df %>%
    mutate(County_got_na = ifelse(is.na({{var_to_clean}}),1,0)) %>%
    group_by(FIPS) %>%
    mutate(County_got_na = max(County_got_na)) %>%
    filter(County_got_na==0) %>% #Only keep counties without NA values
    ungroup()
  cleaned_df
}



tic()
out <- lardon(formula = as.formula("Malaria_pt ~ bdmin_dummy"),
              data = spreadbd.df,
              id_var = "ID",
              time_var = "Year"
              )
toc()


saveRDS(out,"Output/model_bacondecomp/malaria_bacondecomp.rds")


twfe <- felm(data = spreadbd.df, Malaria_pt ~ bdmin_dummy | Year + ID | 0 | ID)
summary(twfe)



#----- good comparison


bd_ealier <- out %>%
  filter(type == "Earlier vs Later Treated")
bd_ealier$weight <- bd_ealier$weight / sum(bd_ealier$weight)
good_average <- sum(bd_ealier$estimate*bd_ealier$weight)

bd_later <- out %>%
  filter(type != "Earlier vs Later Treated")
bd_later$weight <- bd_later$weight / sum(bd_later$weight)
bad_average <- sum(bd_later$estimate*bd_later$weight)


ggplot(out)+
  geom_point(aes(x = weight, y = estimate, color = type))+
  scale_color_manual(values = c("Earlier vs Later Treated"="royalblue2","Later vs Earlier Treated"="red2"))+
  geom_hline(yintercept = bad_average, color = "red2")+
  geom_hline(yintercept = good_average, color = "royalblue2")+
  annotate("text",x = 0.02, y = -.5, label = paste0("Average: ", round(good_average,2)),  color = "royalblue2")+
  annotate("text",x = 0.02, y = 1.5, label = paste0("Average: ",round(bad_average,2)),  color = "red2")+
  theme_Publication()

ggsave("Output/model_bacondecomp/decomp_plot.png", width = 8, height = 7)
#all_decomps[[1]]