setwd('D:/ASSR/TakeHome/data')

usePackage<-function(p){
  # load a package if installed, else load after installation.
  # Args:
  #   p: package name in quotes
  
  if (!is.element(p, installed.packages()[,1])){
    print(paste('Package:',p,'Not found, Installing Now...'))
    install.packages(p, dep = TRUE)}
  print(paste('Loading Package :',p))
  require(p, character.only = TRUE)  
}

usePackage("leaflet")
usePackage("dplyr")
usePackage("data.table")
usePackage("sp")
usePackage("rgeos")
usePackage("raster")
usePackage("rgdal")
usePackage("GISTools")
usePackage("data.table")
usePackage("dplyr")

realis <- fread('realis2018.csv')
realis$pa <- toupper(realis$`Planning Area`)
centroids <- readOGR('DGP Centroids.shp')
centroids <-  spTransform(centroids, CRS("+proj=moll +ellps=WGS84"))
dgp <- readOGR("MP14_PLNG_AREA_WEB_PL.shp")
dgp <-  spTransform(dgp, CRS("+proj=longlat +ellps=GRS80"))
one_unit <- subset(realis, realis$`No. of Units` == 1 & realis$`Transacted Price ($)` <= 2500000)
pa_units <- aggregate(realis$`No. of Units`,
                      by = list(realis$pa),
                      FUN = sum)
colnames(pa_units) = c('PA', 'Units')
m <- merge(dgp,pa_units, by.x ='PLN_AREA_N', by.y = 'PA')

pal <-
  colorBin(palette = brewer.pal(10,"YlGnBu"),
           domain = c(0,2000),
           na.color = "#00000000",
           bins=c(0,5,10,50,100,200,400,600,800,1000,1200,1400,1600,1800,2000))
# create the base map, default will be openstreetmap if not selected 
# added centroids point as well
leaflet(dgp) %>% addTiles() %>% addPolygons(fillColor = ~pal(m$Units),
                                                                            weight = 2,
                                                                            opacity = 1,
                                                                            color = "grey",
                                                                            dashArray = "1",
                                                                            fillOpacity = 0.8) %>% addLegend("topright", pal, values=(0:2000), title = "Transacted Units", labFormat = labelFormat(suffix = " Units", between = '-'))%>% addLabelOnlyMarkers(data = centroids,
                                                                                                                                                                                                                                                       lng = ~cen_x_dgp, lat = ~cen_y_dgp, label = ~DGP,
                                                                                                                                                                                                                                                       labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE))

