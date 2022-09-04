#-- Plots the number of malaria cases, by country

my_packages <- c("tidyverse","magrittr","RColorBrewer")
extrafont::loadfonts()
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

#---------------------------------------------------------------------
more_packages <- c("ggthemes","gridExtra","ggthemes")
#install.packages(more_packages, repos = "http://cran.rstudio.com")
library(ggthemes)
#-----------------------------------------------------------------------------------------------------------------
source("Scripts/general_options.R")

################################ LOAD ################################

spreadbd.df <- readRDS("Data/spreadbd_df_linear_0.rds")

################################ LOADED ################################

cases_year <- spreadbd.df %>%
  group_by(Country,Year) %>%
  mutate(Malaria_sum = sum(Malaria, na.rm = T)) %>%
  distinct(Country,Year,Malaria_sum) %>%
  ungroup() %>%
  mutate(Country = ifelse(Country == "CRI","Costa Rica","Panama"))


######## PLOT FOR PAPER Fig 1 ###################
ggplot(data = cases_year, aes(Year, Malaria_sum/1000, color = Country, linetype = Country)) +
  geom_line(size = rel(1.5)) +
  scale_color_manual(values=c("#004488", "#DDAA33")) +  
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_linetype_manual(values = c("Costa Rica" = "solid", "Panama" = "longdash"))+
  ylab("Malaria cases \n (thousands/year)") +
  scale_x_continuous(limits = c(1976, 2016), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 8), expand = c(.004, 0)) + 
  theme_Publication() +
  theme(legend.key.width = unit(3,  "line"),
        legend.title=element_blank(), 
        axis.line.x = element_line(colour="black"),   # For some reason the axes lines drop when legend manipulated above 
        axis.line.y = element_line(colour="black"))

relSize = 8 
file_name = "Output/plot_lines_malaria_cases_country/lineplot_all_linetype"
ggsave(paste0(file_name,".png"), width = relSize, height =relSize) 


