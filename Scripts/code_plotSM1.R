my_packages <- c("rgeos","raster","terra","tidyverse","magrittr","sf","fields","ggrepel",
                 "rlang","ggspatial") #tmap #fastDummies #ggspatial
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)
source("Scripts/general_options.R")
source("Scripts/draw_crosshatch.R")

alt <- raster("Data/alt_cri_pan.tif")

cr_pan_agg.sp <- readRDS("Data/cr_pan_agg_sp.rds")
cr_pan_agg.sf <- cr_pan_agg.sp %>% st_as_sf()

cr <- cr_pan_agg.sf[cr_pan_agg.sf$Country == "CRI",]
pan <- cr_pan_agg.sf[cr_pan_agg.sf$Country == "PAN",]
ds <- st_intersection(st_buffer(cr, 5), st_buffer(pan, 5))

cr_pan_agg.df <-  cr_pan_agg.sf %>% st_drop_geometry()

cr_pan_summarised <- cr_pan_agg.sf %>%
  group_by(Country) %>%
  summarise()

to_exclude <- cr_pan_agg.sf %>% 
  filter(County == "Chepigana") %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(order = row_number() ) %>%
  rename(long = X, lat = Y)

to_exclude %<>%
  group_by(L1,L2,L3) %>% 
  nest() 

# Draw the lines here, using draw.crosshatch function
lines <- map_df(to_exclude$data, draw.crosshatch, width = 7000, pattern= "horizontal")

# Problem comes from lines 7 and 8 that are too short. Manually fix it.
lines[21,1] <- lines[22,1]
lines[21,3] <- lines[22,3]
lines[20,1] <- lines[22,1]
lines[20,3] <- lines[22,3]


to_exclude2 <- cr_pan_agg.sf %>% 
  filter(County %in% c("Bocas Del Toro","Balboa","Taboga") | Province %in% c("Comarca Guna Yala","Darien","Panama")) %>% 
  filter(County!="Chepigana") %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(order = row_number() ) %>%
  rename(long = X, lat = Y)

to_exclude2 %<>%
  group_by(L1,L2,L3) %>% 
  nest() 

#Draw the lines here, using draw.crosshatch function
lines2 <- map_df(to_exclude2$data, draw.crosshatch, width = 7000, pattern= "horizontal")

all_lines <- lines %>%
  bind_rows(lines2)

alt <- projectRaster(alt, 
                     crs="+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
pts <- rasterToPoints(alt, spatial = TRUE)
elp  <- as.data.frame(pts, xy=TRUE) %>% drop_na()


# mybrk <- seq(0, max(elp$alt_cri_pan), 500)
colour_breaks <- classInt::classIntervals(elp$alt_cri_pan, n = 10, style = 'fisher')$brks %>% round(-2)

colours <- terrain.colors(length(colour_breaks))

max_value <- max(elp$alt_cri_pan)

ggplot() +
  geom_raster(data = elp, aes(x = x, y = y, fill = alt_cri_pan)) +
  scale_fill_gradientn(limits  = range(0, max_value),
                       colours = colours,
                       labels = waiver(),
                       values = scales::rescale(colour_breaks, to = c(0, 1), from = c(0, max_value)), 
                       name = "Elevation \n(in m)") + 
  geom_sf(fill = "transparent", data = cr_pan_agg.sf) +
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
        legend.position="bottom",  # c(0.5, 0.8),
        legend.key.width = unit(5, "line"))

relSize <- 15
ggsave("elevation_map.jpg", width = relSize, height =relSize*0.75)
dev.off()

# road network
spr <- readRDS("Data/cleaning_CR_PAN_map/cr_pan_agg_sp.rds")
rn <- readRDS("Data/gROADS-v1_cri_pan.rds")
rn <- as(vect(rn), "Spatial")
rn1 <- rn[spr,]

alt <- raster("alt_cri_pan.tif")
rn1 <- crop(rn, alt)
rn1 <- st_as_sf(rn1)
rns <- st_transform(rn1, st_crs(cr_pan_agg.sf))

alt <- projectRaster(alt, 
                     crs="+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")


# create slope and hillshade
slope <- terrain(alt, opt='slope')
aspect <- terrain(alt, opt='aspect')
hill <- hillShade(slope, aspect, 40, 270)

hs <- rasterToPoints(hill, spatial = TRUE)
hs  <- as.data.frame(hs, xy=TRUE) %>% drop_na()
names(hs)[1] <- "hillshade"

ggplot() +
  geom_raster(data = hs, aes(x = x, y = y, fill = hillshade)) +
  scale_fill_gradient(low = "gray50", high = "white", guide = "none") + 
  geom_sf(fill = "transparent", data = cr_pan_agg.sf) +
  geom_sf(data = rns, aes(colour = "Road network"), show.legend = "line") +
  scale_colour_manual(values = c("Road network" = "red"),
                      guide = guide_legend(override.aes = list(linetype = "solid"))) +
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
        legend.position= c(0.6, 0.8),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.key.width = unit(5, "line"))

relSize <- 15
ggsave("road_network_map.jpg", width = relSize, height =relSize*0.75)
dev.off()