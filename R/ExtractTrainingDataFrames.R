# Extracting Training Data from the image
library("magrittr")
library("raster")


##################################################################################################
#                            Functions
##################################################################################################
extract_bind_df_addclass <- function(x) {
  w <- raster::extract(image,x)
  w <- do.call("rbind",w)
  w <- data.frame(w)
  w$Class <- names(x)
  return(w)
}



##################################################################################################
#                            Load Images and stack into one
##################################################################################################
image_directory <- "PAN_SPOT"
image1_name <- "geomatica_SPOT_panshp_wRatios"
image2_name <- "geomatica_SPOT_panshp_wRatios_wTexture"
image3_name <- "geomatica_SPOT_panshp_wRatios_wTexture5x5"



if (exists(commandArgs())) {
    args <- commandArgs(trailingOnly = T)
    image_directory <- args[1]
    image1_name <- args[2]
    image2_name <- args[3]
    image3_name <- args[4]
}

image1_path <- paste0("../",image_directory,"/",image1_name,".tif")
image2_path <- paste0("../",image_directory,"/",image2_name,".tif")
image3_path <- paste0("../",image_directory,"/",image3_name,".tif")


image1 <- brick(image1_path)
image2 <- brick(image2_path)
image3 <- brick(image3_path)

image <- brick(image1, image2, image3)
image <- stack(image1, image2)
plotRGB(image,4,3,2,stretch = "lin")



##################################################################################################
##################################################################################################



##################################################################################################
#                      Make the Dataframe for classification
##################################################################################################



################################
################################
#   Load the shapefiles for each class,
#       These would have been likely generated in ENVI and exported.
#       They should have been drawn on top of the image we are classifying.

# These shapefiles came from ENVI ROI,
# they were drawn on top of the image we are classifying


## Ted's ROIs for training data
t_water <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_water", encoding = "ESRI Shapefile")
t_grass <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_grass", encoding = "ESRI Shapefile")
t_tree <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_tree", encoding = "ESRI Shapefile")
t_soil <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_soil", encoding = "ESRI Shapefile")
t_impervious <- readOGR(dsn = "../PAN_SPOT/ROIs", layer = "pan_spot_subset_impervious", encoding = "ESRI Shapefile")
names(t_water) <- "water"
names(t_grass) <- "grass"
names(t_tree) <- "tree"
names(t_soil) <- "soil"
names(t_impervious) <- "impervious"

list_classes <- list(t_water, t_grass, t_tree, t_soil, t_impervious)


beginCluster()
b <- lapply(list_classes, function(x) extract_bind_df_addclass(x))
endCluster()

classified_px <- do.call("rbind", b)
classified_px$Class %<>% as.factor()

ted_df <- classified_px
write.table(ted_df, paste0("../",image_directory,"/ExtractedTrainingDataFrames/ted_roi_train_df.txt"))

## Lei's ROIs for training data
l_water <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_water", encoding = "ESRI Shapefile")
l_grass <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_grass", encoding = "ESRI Shapefile")
l_tree <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_tree", encoding = "ESRI Shapefile")
l_soil <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_soil", encoding = "ESRI Shapefile")
l_impervious <- readOGR(dsn = "../PAN_SPOT/ROIs/lei", layer = "pan_spot_lei_impervious", encoding = "ESRI Shapefile")
names(l_water) <- "water"
names(l_grass) <- "grass"
names(l_tree) <- "tree"
names(l_soil) <- "soil"
names(l_impervious) <- "impervious"

list_classes <- list(l_water, l_grass, l_tree, l_soil, l_impervious)

beginCluster()
b <- lapply(list_classes, function(x) extract_bind_df_addclass(x))
endCluster()


classified_px <- do.call("rbind", b)

classified_px$Class %<>% as.factor()

lei_df <- classified_px
write.table(lei_df, paste0("../",image_directory,"/ExtractedTrainingDataFrames/lei_roi_train_df.txt"))
##################################################################################################
##################################################################################################
