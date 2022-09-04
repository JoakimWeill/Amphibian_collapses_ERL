#-- Generate Figure S6 (mitigation efforts by country)

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","ggrepel","hrbrthemes","sf","cowplot","rlang","vctrs","reshape","gridExtra","ggpubr")
extrafont::loadfonts()
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

#---------------------------------------------------------------------
more_packages <- c("ggthemes","gridExtra","ggthemes")
#install.packages(more_packages, repos = "http://cran.rstudio.com")

theme_Publication <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line.x = element_line(colour="black"),
            axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            #legend.title = element_text(face="italic"),
            legend.title=element_blank(),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


#All variables capitalized ; values str_to_title ; remove accents from all variables and values

#-----------------------------------------------------------------------------------------------------------------
source("Scripts/general_options.R")

########################################################################
################################ LOAD ################################
########################################################################

mitExp<-read_csv("Data/MalariaExpendSprayDataCRPan_cleaned.csv", na=".")

########################################################################
################################ LOADED ################################
########################################################################

mitExpDf <- data.frame(sapply(mitExp,c))
mitExpDf <- subset(mitExpDf, select = c(Year ,SrcsMalFinanCR_Tot,SrcsMalFinanPan_Tot,NumHouseSprayCR,NumHouseSprayPan) )
mitExpDf <- mitExpDf[ which(mitExpDf$Year>=1976), ]                     # keep only 1976 forward
sapply(mitExpDf, class)                                                 # check class
mitExpDf <- data.frame(sapply(mitExpDf, as.numeric))                    # convert character to numeric
mitExpDf[c(2,3)] <- mitExpDf[c(2,3)]*10^(-6)                            # scale data for plotting
mitExpDf[c(4,5)] <- mitExpDf[c(4,5)]*10^(-4)




mitExpDfCR = mitExpDf
mitExpDfCR <- subset(mitExpDfCR, select = c(Year ,SrcsMalFinanCR_Tot,NumHouseSprayCR) )
names(mitExpDfCR)[names(mitExpDfCR) == 'SrcsMalFinanCR_Tot'] <- 'Total funding (millions US$)'
names(mitExpDfCR)[names(mitExpDfCR) == 'NumHouseSprayCR'] <- 'Houses sprayed (x 10,000)'
MoltenCR <- melt(mitExpDfCR, id.vars = "Year")

mitExpDfPan = mitExpDf
mitExpDfPan <- subset(mitExpDfPan, select = c(Year ,SrcsMalFinanPan_Tot,NumHouseSprayPan) )
names(mitExpDfPan)[names(mitExpDfPan) == 'SrcsMalFinanPan_Tot'] <- 'Total funding (millions US$)'
names(mitExpDfPan)[names(mitExpDfPan) == 'NumHouseSprayPan'] <- 'Houses sprayed (x 10,000)'
MoltenPan <- melt(mitExpDfPan, id.vars = "Year")

ttlfs = 6
p1 = ggplot(data = MoltenCR, aes(Year, value, color = variable,group = variable,linetype = variable)) + 
  geom_line(size = rel(1.5)) +
  scale_color_manual(values=c("#004488", "#DDAA33")) +  
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #scale_linetype_manual(values = c("Costa Rica" = "solid", "Panama" = "dotted"))+# "longdash"))+
  ylab("Malaria prevention") +
  scale_x_continuous(limits = c(1976, 2017), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 8) , expand = c(0, 0)) + 
  theme_Publication() +
  theme(legend.key.width = unit(5, "line"),
        legend.title=element_blank(), 
        axis.line.x = element_line(colour="black"),   # For some reason the axes lines drop when legend manipulated above 
        axis.line.y = element_line(colour="black")) + 
  annotate("text", x = 1997, y = 8*.95, label = "Costa Rica", fontface = 2, size = ttlfs) +
  labs(tag = "A")

p2 = ggplot(data = MoltenPan, aes(Year, value, color = variable,group = variable,linetype = variable)) + 
  geom_line(size = rel(1.5)) +
  scale_color_manual(values=c("#004488", "#DDAA33")) +  
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #scale_linetype_manual(values = c("Costa Rica" = "solid", "Panama" = "dotted"))+# "longdash"))+
  ylab("Malaria prevention") +
  scale_x_continuous(limits = c(1976, 2017), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(0, 12) , expand = c(0, 0)) + 
  theme_Publication() +
  theme(legend.key.width = unit(5, "line"),
        legend.title=element_blank(), 
        axis.line.x = element_line(colour="black"),   # For some reason the axes lines drop when legend manipulated above 
        axis.line.y = element_line(colour="black")) +
  annotate("text", x = 1997, y = 12*.95, label = "Panama", fontface = 2, size = ttlfs) +
  labs(tag = "B")
# ggarrange works well but legend at bottom gets cut off.
#ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")# ncol = 1,  nrow = 2)#align = "hv") #vjust = .5)

# Using cowplot -- "main" is the two figures combined into 1, when then makes the top row of the final plot_grid object, with the legend being the second row (with relative smaller height)
main = cowplot::plot_grid(p1 + theme(legend.position = "none"), p2 + theme(legend.position = "none"))
cowplot::plot_grid(main, cowplot::get_legend(p1), ncol=1, nrow = 2, rel_heights = c(1,.1))

file_name <- "Output/plot_lines_mitigExpend_country/lineplot_mitigExpSpray_country"
wid = 12 #relative size
ht  = wid*.75
ggsave(paste0(file_name,".png"), width = wid, height = ht) #Saving the plot
dev.off()
