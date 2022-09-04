
#---------------------------------------------------------------------------------------------------------------
#---------- This code plots the years of arrival, Fig. 2
#---------------------------------------------------------------------------------------------------------------

my_packages <- c("rgeos","raster","tidyverse","magrittr","sf","fields","ggrepel","rlang")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)
library("ggspatial")
map<-purrr::map 
select<-dplyr::select

source("Scripts/general_options.R")

##################################################################

cr_pan_agg.sp <- readRDS("Data/cr_pan_agg_sp.rds")
cr_pan_agg.sf <- cr_pan_agg.sp %>% st_as_sf()


cr_pan_agg.sf <- cr_pan_agg.sp %>% st_as_sf()
cr <- cr_pan_agg.sf[cr_pan_agg.sf$Country == "CRI",]
pan <- cr_pan_agg.sf[cr_pan_agg.sf$Country == "PAN",]
ds <- st_intersection(st_buffer(cr, 5), st_buffer(pan, 5))

cr_pan_agg.df <-  cr_pan_agg.sf %>% st_drop_geometry()

myData <- readRDS("Data/optimal_spread_model_extracted_dataframe_rho_linear_0.rds")

#--------------------------------------------------------------------- Plots


my_data_unique <- myData %>% distinct(Country,Province,County, .keep_all = T) %>% select(-Year)


declines <- readRDS("Data/sites_declines_sp.rds")

declines <- st_as_sf(declines)

coord<- st_coordinates(declines)
declines <- cbind(declines,coord)    


declines_floor <- declines %>% 
  mutate(year = floor(year))


cr_pan_summarised <- cr_pan_agg.sf %>%
  group_by(Country) %>%
  summarise()


map_predictions <- cr_pan_agg.sf %>% 
  left_join(my_data_unique, by = c("Country", "County", "Province", "ID")) %>%
  mutate(min_pred = ifelse( Province %in% c("Comarca Guna Yala","Darien","Panama") | County == "Bocas Del Toro", NA, min_pred) ) 


#-------------------------------------------- plots ----------------------------------------------------------------

#######--- crosshatching

source("Scripts/draw_crosshatch.R")

to_exclude <- cr_pan_agg.sf %>% 
  filter(County == "Chepigana") %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(order = row_number() ) %>%
  rename(long = X, lat = Y)

table(to_exclude$L2) #number of different units
table(to_exclude$L3) #number of different counties

to_exclude %<>%
  group_by(L1,L2,L3) %>% 
  nest() 

lines <- map_df(to_exclude$data, draw.crosshatch, width = 7000, pattern= "horizontal")

#plotting problem comes from lines 7 and 8 that are too short. Manually fix it.
lines[21,1] <- lines[22,1]
lines[21,3] <- lines[22,3]
lines[20,1] <- lines[22,1]
lines[20,3] <- lines[22,3]



#-------- 
to_exclude2 <- cr_pan_agg.sf %>% 
  filter(County %in% c("Bocas Del Toro","Balboa","Taboga") | Province %in% c("Comarca Guna Yala","Darien","Panama")) %>% 
  filter(County!="Chepigana") %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(order = row_number() ) %>%
  rename(long = X, lat = Y)

table(to_exclude2$L2) #number of different units
table(to_exclude2$L3) #number of different counties

to_exclude2 %<>%
  group_by(L1,L2,L3) %>% 
  nest() 

#Draw the lines here, using draw.crosshatch function
lines2 <- map_df(to_exclude2$data, draw.crosshatch, width = 7000, pattern= "horizontal")


ggplot(cr_pan_agg.sf) +
  geom_sf() +
  geom_segment(data=lines2, aes(x= x, y = y , xend = xend, yend = yend), 
             inherit.aes = F)


map_predictions_full <- cr_pan_agg.sf %>% left_join(my_data_unique, by = c("Country", "County", "Province", "ID"))

#-----------------  Preferred map for paper ------------------ 

all_lines <- lines %>%
  bind_rows(lines2)

ggplot(map_predictions_full) +
  geom_sf(aes(fill = min_pred), color = "gray75", size = .15) +                           #county bnd
  geom_sf(data = ds, color = "white", size = 1, fill = NA) +             #country bnd
  geom_point(data = declines_floor, aes(X,Y), colour = "black", show.legend = F)+
  geom_label_repel(data = declines_floor, aes(X,Y, label = year), 
                   fontface = "bold", nudge_y = c(110000, 130000, 100000, 50000, 70000,100000),
                   size =8)+
  scale_fill_viridis_c(name = "Date of Decline (DoD) ", 
                       breaks = c(1970,1980, 1990, 2000, 2010), 
                       limits=c(min(map_predictions_full$min_pred, na.rm = T),max(map_predictions_full$min_pred)), #max(map_predictions$min_pred, na.rm = T)
                       na.value = "gray85")+
  geom_segment(data=all_lines, aes(x= x, y = y , xend = xend, yend = yend), 
               inherit.aes = F,
               color = "grey31") +
  annotation_scale(location = "bl", width_hint = 0.2,
                   pad_x = unit(0.75, "in"), pad_y = unit(0.75, "in"), 
                   text_cex = 1.5, text_family = font_plot) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1.5, "in"), pad_y = unit(1, "in"),
                         height = unit(1, "in"), width = unit(1, "in"),
                         style = north_arrow_fancy_orienteering(fill = rep("black",2) , 
                                                          text_size = 20,
                                                          text_family = font_plot)) +
  annotate("text", x = 360000, y =  890000, label = "Panama", color = "black",size=8)+
  annotate("text", x = 160000, y = 1000000, label = "Costa \n Rica", color = "black",size=8)+
  theme_jo +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",  
        legend.key.width = unit(5, "line"))

relSize = 15 #relative size
figDestName<-paste0("Output/plot_spread_optimal_model_years_decline/map_optimal_spread_rate_min_pred_crosshatched_1.jpg")
ggsave(figDestName, width = relSize, height =relSize*0.75) #Saving the plot
dev.off()
