library(raster)
library(rworldmap)
library(tidyverse)

library(ggplot2)

library(rnaturalearth)
library(ggspatial)
library(ggOceanMaps)
points <- read.csv("./Data/Map.csv")
TR <- subset(points, Location== "Tundra Ridge")
GB <- subset(points, Location== "Godbout")
boundary <- extent(-130, -10, 30, 75)
boundary
map_outline <- getMap(resolution="high")
map_outline <- crop(map_outline, y=boundary) %>% fortify()
ocean <- ggplot()+
  # Plot map land (fill), and outline (colour & size)
  geom_polygon(data=map_outline, aes(x=long, y=lat, group=group), fill="grey90", colour="grey60", size=0.5) +
  coord_quickmap(expand = F)+
  # Axes labels
  xlab("Longitude")+
  ylab("Latitude")+
  # Adjust background theme/colour
  theme(
    axis.text = element_text(colour="black", size=12),
    axis.title = element_text(colour="black", size=14),
    panel.background = element_rect(fill="white"),
    panel.border = element_rect(fill=NA, colour="black", size=0.5),
    legend.text=element_text(size=12),
    legend.title=element_blank(),
    legend.key.size=unit(0.7, "cm"),
    legend.position="none"
  ) +
annotate("text", x=-30, y=45, label="North Atlantic Ocean",fontface="italic", size=6, color="black")+
  annotate("text", x=-46, y=57, label="Labrador Sea", fontface="italic", size=4, color="black")+
  annotate("text", x=-43, y= 50, label="Saint Lawrence River", fontface="italic", size=3, color="black")+
geom_spatial_point(data= TR,
                   aes(x= Longitude, y = Latitude),
                   pch=21, size= 3, stroke = 0.7, alpha=1,
                   color= "black",
                   fill= "black") +
  geom_spatial_point(data= GB,
                     aes(x= Longitude, y = Latitude),
                     pch=21, size= 3, stroke = 0.7, alpha=1,
                     color= "black",
                     fill= "grey") +
  geom_spatial_label(data=TR,
                           aes(x= Longitude, y = Latitude, label= Location), hjust=1.1)+
  geom_spatial_label(data=GB,
                     aes(x= Longitude, y = Latitude, label= Location), hjust=1.1)+
  geom_segment(x= -53, y = 50,
               xend= -65, yend= 49.7, color= "black", data=map_outline,
               arrow=arrow(angle=30, length=unit(0.1,"inches")))+
  theme(panel.background = element_rect(fill="#BEDAEC"),
        panel.grid.major= element_line(colour =NA),
        panel.grid.minor = element_line(colour = NA))
ocean                           
ggsave ( plot=ocean, filename= "output/Result_graph/Ocean_map.png")
