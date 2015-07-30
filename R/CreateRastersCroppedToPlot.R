library(raster)
library(rgdal)
library(rgeos)

## Read in the master image with all of it's layers
image_directory <- "PAN_SPOT"
image1_name <- "geomatica_SPOT_panshp_wRatios"
image2_name <- "geomatica_SPOT_panshp_wRatios_wTexture"

if (exists(commandArgs())) {
    args <- commandArgs(trailingOnly = T)
    image_directory <- args[1]
    image1_name <- args[2]
    image2_name <- args[3]
    image3_name <- args[4]
}

image1_path <- paste0("../",image_directory,"/",image1_name,".tif")
image2_path <- paste0("../",image_directory,"/",image2_name,".tif")




image1 <- brick(image1_path)
image2 <- brick(image2_path)

image <- stack(image1, image2)

if (exists("image3_name")){
    image3_path <- paste0("../",image_directory,"/",image3_name,".tif")
    image_3 <- brick(image3_path)
    image <- stack(image1, image2, image3)
}

## Read in the plot centers shape file
plot_centers <- readOGR(dsn = "../FieldData/PlotCenterShpFile", layer = "plotCenter")
plot_centers <- spTransform(plot_centers,CRSobj = CRS("+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))


## Create polygon shapefile around plot centers...What should the radius be?

rad <- 200
# Make Diamond polygons around each plot
dia <- list()
for (i in seq_along(plot_centers)) {
      dia[i] <- gBuffer(plot_centers[i,], width = rad,quadsegs = 1)
  }
#dia <-do.call(bind,dia)  Don't run this, but if want to combine into one spatialpolygon object do.

## create a directory for the cropped rasters

dir.create(paste0("../",image_directory,"/RastersAroundPlots")


## For every plot, crop the master image, and save the small raster in the directory

for (i in seq_along(dia)){
    r <- crop(image,dia[[i]])
    writeRaster(r, file = paste0("../",image_directory,"/RastersAroundPlots/Plot",i,".tif"),overwrite = T)
}
