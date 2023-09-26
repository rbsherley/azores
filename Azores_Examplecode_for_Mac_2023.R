# These 5 lines will check whether you have the lastest version of R installed and update it if not. You only need to run these lines once, the first time you use this code.
install.packages('devtools')
require(devtools)
install_github('andreacirilloac/updateR')
require(updateR)
updateR() # enter your computer password in the console when prompted


# This line will install the packages (or libraries) needed to use the functions below. You only need to run this line once, the first time you use this code.
install.packages(c("curl", "sf", "terra", "elevatr","rgdal"))

# load the curl package, used to download some datasets from remote locations:
require(curl)

# Set your working directory (where R should look to find files and save outputs). You should change this so that it reflects the location where this code file is stored on your computer. The map data and the data sets will be in "Downloads"
setwd("~/Downloads")

# This will download some whale data collected on a previous year's field course
curl_download("https://raw.githubusercontent.com/rbsherley/azores/master/whaledata.csv", destfile = "whaledata.csv")
# This will download a high-resolution map of Portugal
curl_download("https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_PRT.gpkg", destfile = "gadm41_PRT.gpkg")
list.files()


whaledata<-read.csv("~/Downloads/whaledata.csv")
head(whaledata)

require(sf)
finwhale <- whaledata[whaledata$Species == "Bp",]
finwhale<-st_as_sf(finwhale, coords = c('Longitude', 'Latitude'), crs = 4326)
plot(finwhale["Total.seen"], pch=15, axes = TRUE)

map_portugal <- st_read("~/Downloads/gadm41_PRT.gpkg")
azores<-st_crop(map_portugal,xmin=-29,xmax=-25,ymin=36,ymax=39)
plot(st_geometry(azores$geom),col="gray",axes=TRUE)
plot(st_geometry(finwhale["Total.seen"]), pch=15, cex=0.5,add=T, col = "blue")

require(terra)
require(rgdal)
get_dem <- function(locations, resolution = 500) {
  e <- terra::ext(locations)
  lat <- (e$ymin + e$ymax)/2
  z <- ceiling(log((cos(lat * pi/180) * 2 * pi * 6378137) / (256 * resolution), 2))
  z <- ifelse(z > 14, 14, z)
  r <- get_elev_raster(locations, z = z, src = "aws")
  r<-rast(r)
  return(r)
}

require(elevatr)
require(progress)
bathy<-get_dem(azores)
terra::plot(bathy)

bathy[bathy>0]<-0
mypal <- colorRampPalette(c("darkblue", "blue", "lightskyblue", "white"))(255)

terra::plot(bathy, col = mypal, main = "Bathymetry", grid=TRUE)
plot(st_geometry(azores),col="gray",axes = TRUE,add = TRUE)
plot(st_geometry(finwhale["Total.seen"]), pch=15, cex=0.5, col = "red", add=TRUE)

quartz(height=4,width=8)
terra::plot(bathy, col = mypal, grid=TRUE)
plot(st_geometry(azores$geom),col="gray",axes = TRUE,add = TRUE)
plot(st_geometry(finwhale["Total.seen"]), pch=15, cex=0.5, col = "red", add=TRUE)
quartz.save("myplot.pdf",type="pdf")
dev.off()
