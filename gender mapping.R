m<-read.csv("C:/Users/anany/OneDrive/Documents/a.csv")
#spplot(m,"State",  col.regions=rgb(0,m$Population,0), colorkey=T, main="Indian States")
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)

shp <- readShapeSpatial("C:/Users/anany/OneDrive/Documents/IND_adm1.shp")
shp<-readOGR(dsn=".",layer="IND_adm1")
plot(shp)

shp.f <- fortify(shp, region ="NAME_1")

#shp.f%>%
# distinct(id) %>%
#write.csv("G:/Dekstop/desktop/desktop/districts.csv", row.names = FALSE)

merge.shp<-merge(shp.f,m, by="id")
final.plot<-merge.shp[order(merge.shp$order), ]

map <- ggplot(data = final.plot, aes(x = long, y = lat, group = group))

# cleaning background
theme_bare <- theme(
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  legend.text=element_text(size=7),
  legend.title=element_text(size=8),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "gray", fill=NA, size=0.5)
)

centroids <- setNames(do.call("rbind.data.frame", by(merge.shp, merge.shp$group, function(x) {Polygon(x[c('long', 'lat')])@labpt})), c('long', 'lat'))
centroids$label <- merge.shp$id[match(rownames(centroids), merge.shp$group)]


#Plotting map

map +
  geom_polygon(aes(fill = Scaled_Capacity), color = 'black', size = 0.1) +
  scale_fill_gradient(limits=c(0,1),high = "#cc0000", low = "#ffff00", guide = "colorbar") +
  coord_fixed(1.3)+guides(fill=guide_colorbar(title="....")) +
  theme(legend.justification=c(0,0), legend.position=c(0,0))+ theme(legend.justification=c(0,0), legend.position=c(0,0))+  theme_bare

#extra addition:-
#ggplot() +
# geom_polygon(data = final.plot,
#             aes(x = long, y = lat, group = group, fill = count),
#            color = "black", size = 0.25) +
#coord_map()+
#scale_fill_distiller(name="IMR", palette = "YlGn")+
#labs(title="IMR of Different States of India 2013")+
#xlab('Longitude')+
#ylab('Latitude')