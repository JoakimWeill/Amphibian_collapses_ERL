
#---------------------------------------------------------------------------------------------------------------
#---------- This code produces the spline plot
#---------------------------------------------------------------------------------------------------------------

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","ggrepel","tmap","tmaptools","sf")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map 
select<-dplyr::select

source("Scripts/general_options.R")
#-----------------------------------------------------------------------------------------------------------------

amph_decline.sp <- readRDS("Data/sites_declines_sp.rds")

cr_pan_agg.sp <- readRDS("Data/cr_pan_agg_sp.rds")

Big.df <- readRDS("Data/preBigdf_to_merge.rds")


declines <- st_as_sf(amph_decline.sp)
coord<- st_coordinates(declines)
declines <- cbind(declines,coord)

Big.df
to_plot <- 1
Big.df$lambda[[to_plot]]

message("Penalty used in the spline that we are about to plot: ",Big.df$lambda[[to_plot]])

#------------------ Create a "contour" from the raster (to get the decadal lines)

interp_contour <- rasterToContour(Big.df$interpolated.raster[[to_plot]], nlevels = 5, levels = c(1980,1990,2000,2010,2020))

interp_contour <- st_as_sf(interp_contour) %>%
  st_cast(interp_contour, to = "POINT")

raster <- Big.df$interpolated.raster[[to_plot]]

cr_pan_agg.sf <- st_as_sf(cr_pan_agg.sp)

cr <- cr_pan_agg.sf[cr_pan_agg.sf$Country == "CRI",]
pan <- cr_pan_agg.sf[cr_pan_agg.sf$Country == "PAN",]
ds <- st_intersection(st_buffer(cr, 5), st_buffer(pan, 5))



#------------------------------------------------------ Interpolation raster (smooth) ---------------------------------------------------------------------------------------

interp_contour1 <- rasterToContour(Big.df$interpolated.raster[[to_plot]], nlevels = 5, levels = c(1980,1990,2000,2010,2020))


interp_contour1.sf <- st_as_sf(interp_contour1) %>%
  mutate(coords = map(geometry, ~st_coordinates(.) %>%  as_tibble)) %>% 
  st_set_geometry(NULL) %>% 
  as_tibble() %>% 
  unnest() %>%
  mutate(gr = paste(level, L1, sep = "_")) #%>% 
#mutate(level = as.integer(as.character(level)))

interp_contour1.sf

contour_text <- interp_contour1.sf %>%
  group_by(level) %>%
  mutate(x = mean(X),
         y = mean(Y)) %>%
  select(level,x,y) %>%
  distinct()


#Transform raster to dataframe
pix <- as(Big.df$interpolated.raster[[to_plot]], "SpatialPixelsDataFrame")
raster.df <- as.data.frame(pix)
colnames(raster.df) <- c("value", "x", "y")

declines_floor <- declines %>% 
  mutate(year = floor(year))


#------------------------------------------------------------- Plot -------------------------------------------------------------


ggplot(cr_pan_agg.sf,  size = .15) +
  geom_tile(data=raster.df, aes(x=x, y=y, fill=value), alpha=0.9) + 
  geom_sf(color = "gray75", size = .15, fill = NA) +
  geom_sf(data = ds, color = "white", size = 1.2, fill = NA) +
  scale_fill_viridis_c(name = "Date of Decline (DoD) ", 
                       breaks = c(1970,1980, 1990, 2000, 2010), 
                       na.value = "gray85")+  #scale_fill_gradientn(colours = myPalette(12),  breaks = c(1975,1980,1985,1990,1995,2000,2005,2010)) + 
  geom_point(data = declines_floor, aes(X,Y), colour = "black", show.legend = F)+
  geom_label_repel(data = declines_floor, aes(X,Y, label = year), 
                   fontface = "bold", nudge_y = c(110000, 130000, 100000, 50000, 70000,100000),
                   size =8)+
  geom_path(data = interp_contour1.sf, aes(x=X, y=Y, group = gr), linetype = "dashed")+
  annotate("text", x = contour_text$x, y = contour_text$y, label = seq(1980, to = (1980+nrow(contour_text)*10-10), by = 10) , alpha = 0.8)+   
  annotate("text", x = 360000, y =  890000, label = "Panama", color = "black",size=8)+
  annotate("text", x = 160000, y = 1000000, label = "Costa \n Rica", color = "black",size=8)+
  annotate("text", x = 360000, y =  890000, label = "Panama", color = "black",size=8)+
  annotate("text", x = 160000, y = 1000000, label = "Costa \n Rica", color = "black",size=8)+
  theme_jo +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",  # c(0.5, 0
        legend.key.width = unit(5, "line"))


to_plot_char <- str_replace(as.character(Big.df$lambda[[to_plot]]),"\\.","-") %>% str_sub(1,4)
relSize = 15 #relative size

figDestName<-paste0("Output/plot_spline_interpolation_raster/interpolation_smoothed_lambda_",to_plot_char,".jpg")  

ggsave(figDestName, width = relSize, height =relSize*0.75) #Saving the plot