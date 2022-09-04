#-------------------------------------------------------------------------------------------
#-------------------------------- Summary table  --------------------------------------
#-------------------------------------------------------------------------------------------
getwd()
my_packages <- c("tidyverse","magrittr","broom","stargazer")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select


remove_islands = TRUE
remove_east = TRUE
no_matina = FALSE
exclude_bocas_del_toro = TRUE #need to change file names too



########################################################################
################################ LOAD ################################
########################################################################

spreadbd.df <- readRDS("Data/spreadbd_df_linear_0.rds")

########################################################################
################################ LOADED ################################
###########
glimpse(spreadbd.df)
## ------ Summary table

#----- Subsample to use dataset

caption_east = ""
if(remove_east) { #Includes Taboga and Balboa, the 2 islands
  
  spreadbd.df %<>%
    filter( !(Province %in% c("Comarca Guna Yala","Darien","Panama")) ) 
  
  caption_east = "_east_exluded"
  
}

caption_matina = ""
if(no_matina) {
  
  spreadbd.df %<>%
    filter( !(County == "Matina") ) 
  
  caption_matina = "_matina_excluded"
  
}


caption_island = ""
if(remove_islands) {
  
  caption_island = "_islands_removed"
  
}

caption_bocas = ""
if(exclude_bocas_del_toro) {
  spreadbd.df %<>%
    filter(!County=="Bocas Del Toro")
  caption_bocas = "_bocas_excluded"
}


#-------------------------------------------- Table -------------------------------------------------




df <- spreadbd.df%>%
  select(Province, County, Year, Population, Malaria, Malaria_pt, Tree_cover, Non_tree_vegetation, Bare_ground, 
         Precip_annual, Temp_min_annual, Temp_max_annual, Temp_av) %>%
  as.data.frame()
  

stargazer(df, 
          summary.stat = c("n", "mean", "sd","min","max"),
          header = FALSE,
          title = "Summary Statistics",
          out = paste0("Output/summary_table/summary_statistics",caption_east,caption_matina,caption_bocas,".tex"),
          covariate.labels = c("Year", "Population", "Malaria","Malaria per thousand", "Tree cover","Non tree vegetation",
                               "Bare ground","Annual precipitations", "Minimum annual temperature", "Maximum annual temperature","Average annual temperature")
          )


#By group

library(data.table)

my.summary_long = function(x) list(mean = mean(x, na.rm = T), 
                                   sd = sd(x, na.rm = T),
                                   #min = min(x, na.rm = T),
                                   #max= max(x, na.rm = T),
                                   n=  sum(!is.na(x))
)

summarise_this_df <- function(this_df,cols_to_use,group_to_use = "", all_summary = T) {
  
  this_dt <- as.data.table(this_df)
  
  if(all_summary) {
    my.summary <- my.summary_long
  } else {
    my.summary <- my.summary_short
  }
  
  this_dt_summarized <- this_dt[, unlist(lapply(.SD, my.summary),recursive = FALSE),
                                .SDcols = cols_to_use, by = group_to_use]  
  
  if(group_to_use=="") {
    this_dt_summarized_2 <- this_dt_summarized %>%
      as_tibble()
  } else {
    this_dt_summarized_2 <- this_dt_summarized %>%
      pivot_wider(names_from = all_of(group_to_use), #If we want the group stats in columns
                  values_from = names(this_dt_summarized)[-which(names(this_dt_summarized)==group_to_use)] ) 
  }
  
  this_dt_summarized_3 <- this_dt_summarized_2 %>%
    pivot_longer(everything(), names_to= c(".value", "variable"), names_sep = "\\.") %>%
    gather(var, value, -variable) %>% #rotate the dataset
    spread(variable, value) %>%
    mutate(Observations = nrow(this_dt)) %>%
    rename(Variable = var) %>%
    select(Variable,Observations,everything())
  
  this_dt_summarized_3
  
}


spreadbd.dt <- spreadbd.df %>%
  select(Year, Population, Malaria, Malaria_pt, Tree_cover, Non_tree_vegetation, 
         Bare_ground, Precip_annual, Temp_min_annual, Temp_max_annual, Temp_av,bdmin_dummy) %>%
  mutate(bdmin_dummy = if_else(bdmin_dummy==0,"Pre","Post")) %>%
  as.data.table()

df_summarized_by_group <- summarise_this_df(spreadbd.dt,cols_to_use=names(spreadbd.dt)[-length(names(spreadbd.dt))],
                                            group_to_use = "bdmin_dummy",all_summary = T) %>%
  mutate_if(is.numeric, ~round(.,2)) %>%
  select(-Observations) %>%
  mutate(Variable = case_when(Variable == "Bare_ground" ~ "Bare ground",
                              Variable == "Malaria_pt" ~ "Malaria per thousand",
                              Variable == "Non_tree_vegetation" ~ "Non tree vegetation",
                              Variable == "Temp_max_annual" ~ "Maximum annual temperature",
                              Variable == "Temp_min_annual" ~ "Minimum annual temperature",
                              Variable == "Temp_av" ~ "Average annual temperature",
                              Variable == "Tree_cover" ~ "Tree cover", 
                              Variable == "Precip_annual" ~ "Annual precipitations",
                              TRUE ~ Variable
                              )) %>%
  select("Variable","n_Pre","mean_Pre","sd_Pre", "n_Post", "mean_Post","sd_Post")

names(df_summarized_by_group) <- str_replace(names(df_summarized_by_group),"_Pre"," (pre)")
names(df_summarized_by_group) <- str_replace(names(df_summarized_by_group),"_Post"," (post)")


stargazer(df_summarized_by_group,
          summary = F,
          header = FALSE,
          title = "Balance table pre and post treatment",
          column.labels = c("Pre", "Post"),
          column.separate = c(5, 5),
          rownames = F,
          out = paste0("Output/summary_table/balance_table_pre_post",caption_east,caption_matina,caption_bocas,".tex"))
    
  
  
  
  
  
  # For sharing results quickly only
if(!require(matPkg)) remotes::install_github("MatthieuStigler/matPkg", upgrade = "never")
prj_table_to_pdf <- function(path_tex,
                             plus ="\\usepackage{booktabs}\n\\usepackage{dcolumn}\n\\usepackage{underscore}\n\\usepackage[english]{babel}"){
  path_pdf <- str_replace(path_tex, "\\.tex$", ".pdf")
  
  mat_table_to_pdf(x = path_tex,
                   filename = path_pdf,
                   is_path_x = TRUE, copy.mode = FALSE,
                   plus=plus)
}

prj_table_to_pdf("Output/summary_table/balance_table_pre_post_east_exluded_bocas_excluded.tex")
prj_table_to_pdf("Output/summary_table/summary_statistics_east_exluded_bocas_excluded.tex")
#---------- Correlation of land use variables
#
#
#land_use <- tibble(Year = Big.df$myData[[1]]$Year,
#             Tree_cover = Big.df$myData[[1]]$Tree_cover,
#             Non_tree_vegetation = Big.df$myData[[1]]$Non_tree_vegetation,
#             Bare_ground = Big.df$myData[[1]]$Bare_ground) %>%
#  filter(Year >= 1982) %>%
#  select(-Year)
#             
#
#stargazer(cor(land_use), 
#          title = "Land use pearson correlations",
#          out = "Tables/correlation_land_use.tex")


