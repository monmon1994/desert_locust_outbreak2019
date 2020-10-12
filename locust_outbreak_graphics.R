library(readr)
library(tidyverse)
library(extrafont)
loadfonts()

#### Load

load("data/data.RData")


## Which countries are most affected?

x <- d %>%
    #Filter for only current month, only swarms
    filter(STARTDATE >= as.Date("2020-04-01"), CAT == "Swarm") %>%
    #Calculate area per country
    group_by(COUNTRYID) %>%
    summarise(area = sum(AREAHA, na.rm = F), swarms = n()) %>% 
    arrange(-area)

head(x)

## control vs swarms

ggplot(x, aes(month, ha/100, color = var)) + 
    geom_line(size = 1.5) + labs(y = "km²") +
    ggtitle("Locusts multiply faster than control operations can keep up") +
    scale_y_continuous(expand = expand_scale(mult = c(0,.05))) +
    labs(caption = "Source: FAO Locust Hub and Kira Schacht",
         x = "") +
    theme(panel.background = element_blank(),
      text = element_text(family = "Proxima Nova Rg", size = 11),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_rect(fill = "#f5f5f5"),
      plot.background = element_blank(),
      strip.text.x = element_text(hjust = 0),
      plot.caption = element_text(hjust = 0),
      plot.title = element_text(size = 14), 
      plot.title.position = "plot",
      legend.position = "right") +
    guides(color = guide_legend(title = ""))
    #scale_color_manual(values = dw_info[2:1] %>% unname) 

ggsave("locusts_control.png", dpi = 600)


ggplot(data=d_plot %>% filter(month == months), aes(x=LONG, y=LAT)) + 
    #Create country borders
    geom_polygon(data=shp, aes(x=long, y=lat, group=group), colour = "black", alpha = 0.1) +
    #Create points on map
    geom_point(aes(colour = CAT), size = 5) +
    #Convert to map coordinate projection
    coord_map() +
    #Theme settings
    scale_x_continuous(limits = c(-20,80)) + scale_y_continuous(limits = c(-5,35)) +
    scale_colour_manual(values = dw_info[2:1] %>% unname) +
    theme_void() + theme(legend.position = "none")
