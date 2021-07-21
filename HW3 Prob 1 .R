library(maps)

airports <- read.csv("airports.txt", header=TRUE)
flights <- read.csv("routes.dat", header=TRUE, as.is=TRUE)

head(flights)
head(airports)



# Select only large airports: ones with more than 10 connections in the data.

tab <- table(flights$Source.ID)
big.id <- names(tab)[tab>10]

airport.index <- which(unlist(lapply(airports$ID,function(x){
  any(big.id==x)
})))
airports <- airports[airport.index,]



flight.source.index <-  which(unlist(lapply(flights$Source.ID,function(x){
  any(big.id==x)
})))
flight.target.index <-  which(unlist(lapply(flights$Destination.ID,function(x){
  any(big.id==x)
})))
flights  <- flights[intersect(flight.source.index,flight.target.index),]

airports

head(flights)
install.packages("usmap")
library(usmap)
library(ggplot2)
library(viridis)

confirmed_world$state<-confirmed_world$Province_State
plot_usmap(data = confirmed_world, values = "Confirmed", color = "red") + 
  scale_fill_viridis(name = "Confirmed 4/8/20", label = scales::comma) + 
  theme(legend.position = "right")
map("state", col="tomato", fill=TRUE, bg="black", lwd=0.1)


map("county", col="tomato", fill=TRUE, bg="black", lwd=0.1)

### These are great colors for visualizing the map. But not so good for background



map("state", col="grey30", fill=TRUE, bg="black", lwd=0.1)

# Add a point on the map for each airport:
points(x=airports$longitude, y=airports$latitude, pch=19,
       cex=(airports$Visits/70)^2, col="orange")

# Next we will generate a color gradient to use for the edges in the network. Heavier edges will be lighter in color.

col.1 <- adjustcolor("orange red", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)


for(i in 1:nrow(flights))  {
  node1 <- airports[airports$ID == flights[i,]$Source,]
  node2 <- airports[airports$ID == flights[i,]$Target,]
  edge.ind <- round(100*flights[i,]$Freq / max(flights$Freq))
  
  lines(c(node1[1,]$longitude, node2[1,]$longitude),
        c(node1[1,]$latitude, node2[1,]$latitude), col=edge.col[edge.ind], lwd=edge.ind/30)
}

### good one. But can be improved by using arcs

#install.packages("geosphere")


library('geosphere')
### The package contains many useful functions you can use if you want to calculate something related to geographical locations --- arcs, arc lengths etc.


### how to get an arc between any two points on the map?

airports[1:2,]

gcIntermediate( c(airports[1,]$Long, airports[1,]$Lat),
                c(airports[2,]$Long, airports[2,]$Lat),
                n=10, addStartEnd=TRUE )



map("state", col="grey30", fill=TRUE, bg="black", lwd=0.1)


for(i in 1:nrow(flights))  {
  node1 <- airports[airports$ID == flights[i,]$Source,]
  node2 <- airports[airports$ID == flights[i,]$Target,]
  
  arc <- gcIntermediate( c(node1[1,]$Long, node1[1,]$Lat),
                         c(node2[1,]$Long, node2[1,]$Lat),
                         n=1000, addStartEnd=TRUE )
  
  lines(arc)
}



#### For your HW3, you will generate the global network. Notice that there is one difference you need to handle.

map("world", col="light blue",  border="gray10", fill=TRUE, bg="gray30")

points(x=airports$Long, y=airports$Lat, pch=19,
       cex=(airports$Visits/70)^2, col="orange")
library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(ggmap)
library(geosphere)
library(plyr)

urbanareasin <- read_sf("ne_10m_urban_areas.shp")
worldmap = map_data ("world")
wrld<-c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#090D2A", fill="#090D2A", alpha=0.8, data=worldmap))

urb <- c(geom_polygon(aes(long, lat, group = group),
                      size = 0.3,
                      color = "#ffffff",
                      fill = "#ffffff",
                      alpha = 1,
                      data = urbanareasin))
ggplot() + wrld + urb + theme(panel.background = element_rect(fill='#00001C',colour='#00001C'),
                              panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + annotate("text",x=max(worldmap$long),y=-60,hjust=.9,size=3,
                                                                                          label=paste("World at night!","Created by Weimin Wang",sep="\n"),color="white")

rm(list = ls())

fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}

df = read.csv('routes_all.csv', stringsAsFactor=F)
plot_countries = c('United States', 'United Kingdom', 'Spain', 'Germany', 'France', 'Italy','China', 'Singapore')
df = df[df$from %in% plot_countries,]

# calculate routes for each row
routes = gcIntermediate(df[,c('lon1', 'lat1')], df[,c('lon2', 'lat2')], 200, breakAtDateLine = FALSE, addStartEnd = TRUE, sp=TRUE)
# fortify to dataframe
fortifiedroutes = fortify.SpatialLinesDataFrame(routes)

# merge to form great circles
routes_count = data.frame('count'=df$count, 'id'=1:nrow(df), 'Countries'=df$from)
greatcircles = merge(fortifiedroutes, routes_count, all.x=T, by='id')

# get worldmap
worldmap = map_data ("world")

# wrld layer
wrld<-c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#090D2A",
                     fill="#090D2A", alpha=0.8, data=worldmap))
# urban layer
install.packages("sf")
library(sf)
library(rgdal)
urbanareasin <- read_sf("ne_10m_urban_areas.shp")
urb <- c(geom_polygon(aes(long, lat, group = group),
                      size = 0.3,
                      color = "#ffffff",
                      fill = "#ffffff",
                      alpha = 0.8,
                      data = urbanareasin))

# final combine
ggplot() +
  wrld + 
  geom_line(aes(long,lat,group=id, color=Countries), alpha = 0.3, size=0.01, data= greatcircles) +
  theme(panel.background = element_rect(fill='#00001C',colour='#00001C'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = c(0,0.4),
        legend.justification = c(0,1),
        legend.background = element_rect(colour = NA, fill = NA),
        legend.key = element_rect(colour = NA, fill = NA, size = 10),
        legend.text = element_text(colour='white', size = 20)) +
  guides(fill = guide_legend(keywidth = 20, keyheight = 20)) 




library(ggplot2)
library(maps)
library(rgeos)
library(maptools)
library(ggmap)
library(geosphere)
library(plyr)


rm(list = ls())
fortify.SpatialLinesDataFrame = function(model, data, ...){
  ldply(model@lines, fortify)
}

airports <- read.csv("airports.txt", header=TRUE)
flights <- read.csv("routes.dat", header=TRUE, as.is=TRUE)

head(flights)
head(airports)

flights.ag <- ddply(flights, c("Source.airport","Destination.airport"), function(x) count(x$Destination.airport))

flights<-flights.ag[,c(1:2,4)]
plot_countries = c('United States', 'United Kingdom', 'Spain', 'Germany', 'France', 'Italy','China', 'Singapore')
flights = flights[flights$Source.airport %in% plot_countries,]
airports<-airports[,c(1:5,7:8)]

tab <- table(flights$Source.airport)
big.id <- names(tab)[tab>10]

airport.index <- which(unlist(lapply(airports$IATA,function(x){
  any(big.id==x)
})))
airports <- airports[airport.index,]

flight.source.index <-  which(unlist(lapply(flights$Source.airport,function(x){
  any(big.id==x)
})))
flight.target.index <-  which(unlist(lapply(flights$Destination.airport,function(x){
  any(big.id==x)
})))
flights  <- flights[intersect(flight.source.index,flight.target.index),]

airports

map("world", col="light blue",  border="gray10", fill=TRUE, bg="gray30")

points(x=airports$Long, y=airports$Lat, pch=19, cex=0.3,col="red")



col.1 <- adjustcolor("yellow", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)
library(rgeos)
library(maps)
RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}
for(i in 1:nrow(flights))  {
  node1 <- airports[airports$IATA == flights[i,]$Source.airport,]
  node2 <- airports[airports$IATA == flights[i,]$Destination.airport,]
  arc <- gcIntermediate( c(node1[1,]$Long, node1[1,]$Lat),
                         c(node2[1,]$Long, node2[1,]$Lat),
                         n=1000, breakAtDateLine=TRUE, addStartEnd=TRUE, sp=TRUE)
  edge.ind <- round(100*flights[i,]$freq / max(flights$freq))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

RegroupElements <- function(df, longcol, idcol){  
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) {          # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1     # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2      # parts that are moved
  }
  g <-  paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}

if (node1[1,]$Long + node2[1,]$Long>180){
  arc<-gcIntermediate( c(node2[1,]$Long, node2[1,]$Lat),
                       c(node1[1,]$Long, node1[1,]$Lat),
                       n=1000, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
}
else{
  arc <- gcIntermediate( c(node1[1,]$Long, node1[1,]$Lat),
                         c(node2[1,]$Long, node2[1,]$Lat),
                         n=1000, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
}

ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df))  # rassign the order variable
  df[,ordercol] <- o
  df
}




