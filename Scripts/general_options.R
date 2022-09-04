# General options used for plotting

#options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors
library(extrafont)


library(tidyverse)
font_plot = "Arial Narrow" #"CMU Serif"
theme_jo <-   theme(plot.title = element_text(hjust=0, size=30, margin=margin(b=10), family=font_plot, face="bold"),
                    plot.subtitle = element_text(hjust=0, size=26, margin=margin(b=15), family=font_plot, face="plain"),
                    plot.caption=element_text(hjust=1, size=24, margin=margin(t=10), family=font_plot, face="italic"),
                    text=element_text(family=font_plot),
                    axis.text.x=element_text(size=24, margin=margin(t=0)),
                    axis.text.y=element_text(size=24, margin=margin(r=0)),
                    axis.title=element_text(size=24, family=font_plot),
                    axis.title.x=element_text(size=24, family=font_plot, face="plain"), #hjust=xj,
                    axis.title.y=element_text(size=24, family=font_plot, face="plain"), #hjust=yj, 
                    axis.title.y.right=element_text(size=24, angle=90, family=font_plot, face="plain"), #hjust=yj,
                    strip.text=element_text(hjust=0, size=12, face="plain", family=font_plot),
                    plot.margin=margin(30, 30, 30, 30),
                    #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
                    legend.title=element_text(size=24),
                    legend.text=element_text(size=24),
                    #legend.position="bottom",
                    #legend.box = "horizontal",
                    panel.background = element_rect(fill = "white"),
                    panel.grid=element_line(color="#cccccc", size=0.2),
                    panel.grid.major=element_line(color="#cccccc", size=0.2),
                    panel.grid.minor=element_line(color="#cccccc", size=0.15))  

theme_Publication <- function(base_size=14, base_family=font_plot) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(size=30),
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
            # legend.margin = unit(0, "cm"),
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
