
library(glcm)
library(raster)
library(spatial.tools)
library(dplyr)
library(doParallel)

######################################################## functions ####

namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)),deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}

ndvi_nodrop <- function(image_w4bands,red_bandnumber,nir_bandnumber,...) {
  red_band <- image_w4bands[,,red_bandnumber,drop=FALSE]
  nir_band <- image_w4bands[,,nir_bandnumber,drop=FALSE]
  ndvi <- (nir_band - red_band)/(nir_band + red_band)
  return(ndvi)
}

savi_nodrop <- function(image_w4bands,red_bandnumber,nir_bandnumber,L,...) {
  red_band <- image_w4bands[,,red_bandnumber,drop=FALSE]
  nir_band <- image_w4bands[,,nir_bandnumber,drop=FALSE]
  savi <- (nir_band - red_band)/(nir_band + red_band + L) * (1+L)
  return(savi)
}

ratio <- function(image_w4bands, numerator_bandNumber) {
  r <- image_w4bands[,,numerator_bandNumber,drop = F] / sum(image_w4bands)
  return(r)
}

create_GLCM_layers_parallel <- function(list_rasterlayers, vec_window_sizes, dir, cpus) {
  cl <- makeCluster(spec = cpus, methods = FALSE)
  # Register the cluster with foreach:
  registerDoParallel(cl)
  GLCM_rasters <- foreach(i = 1:length(list_rasterlayers), .packages = c('glcm','raster')) %:%
    foreach (j = 1:length(window_sizes), .packages = c('glcm','raster')) %dopar% {
      raster <- list_rasterlayers[[i]]
      dir <- dir
      window_size <- vec_window_sizes[j]
      w_s <- c(window_size,window_size)
      a <- glcm(raster,shift = dir, window = w_s,na_opt = "center", na_val = 0, asinteger = T)
      names(a)<- paste0(names(list_rasterlayers[[i]]),"_",vec_window_sizes[j],"x",vec_window_sizes[j],"_",names(a))
      a
    }
  stopCluster(cl) # Stops the cluster
  registerDoSEQ()
  return(unlist(GLCM_rasters))
}


#############       Objective:        #####################################

#
#             Take a 4 layer rasterstack, generate additional layers/features
#           Do this in an efficient/fast way.
#           Use rasterEngine and parallel processing

#             - SAVI
#             - NDVI
#             - 4 ratio layers (band_n/(sum(allbands)))
#             - NIR 3x3 window GLCM texture layers
#             - SAVI 3x3 window GLCM texture layers
#             - NIR 5x5 window GLCM texture layers
#             - SAVI 5x5 window GLCM texture layers
#             - NIR 9x9 window GLCM texture layers
#             - SAVI 9x9 window GLCM texture layers


#######################################################################################
####################    script to get SAVI, NDVI, and the 4 ratios

####################
####### INPUTS
############################################################
args <- commandArgs(trailingOnly = T)
#rasterFileName <- "SPOT_PanSharp_subsubset"
#directory_path <- "../PAN_SPOT/subset_images/"
rasterFileName <- args[1]
directory_path <- paste0("../",args[2],"/")
#rasterFileName <- "SPOT_PanSharp_subsubset"
#directory_path <- "../PAN_SPOT/"
raster_stack <- stack(paste0(directory_path,rasterFileName,".tif")) %>%
  setMinMax()
# quartz()
# plotRGB(raster_stack,4,3,2,stretch = "lin")
nir_bandnumber <- 4
red_bandnumber <- 3
green_bandnumber <- 2
blue_bandnumber <- 1
L <- .5
cpus <- detectCores()

############################################################

NIR <- raster_stack[[nir_bandnumber]]
names(NIR) <- "NIR"
Red <- raster_stack[[red_bandnumber]]
names(Red) <- "Red"
Green <- raster_stack[[green_bandnumber]]
names(Green) <- "Green"
Blue <- raster_stack[[blue_bandnumber]]
names(Blue) <- "Blue"

#######  Ratios, SAVI, NDVI
cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
# Register the cluster with foreach:
registerDoParallel(cl)

numerator_bandNumber<-1
ratio1 <-
  rasterEngine(
    # Match the variable name in the function to the raster:
    image_w4bands = raster_stack,
    # Assign the function:
    fun=ratio,
    args=list(numerator_bandNumber = numerator_bandNumber)
  )
names(ratio1) <- "ratio1"

numerator_bandNumber<-2
ratio2 <-
  rasterEngine(
    # Match the variable name in the function to the raster:
    image_w4bands = raster_stack,
    # Assign the function:
    fun=ratio,
    args=list(numerator_bandNumber = numerator_bandNumber)
  )
names(ratio2) <- "ratio2"

numerator_bandNumber<-3
ratio3 <-
  rasterEngine(
    # Match the variable name in the function to the raster:
    image_w4bands = raster_stack,
    # Assign the function:
    fun=ratio,
    args=list(numerator_bandNumber = numerator_bandNumber)
  )
names(ratio3) <- "ratio3"

numerator_bandNumber<-4
ratio4 <-
  rasterEngine(
    # Match the variable name in the function to the raster:
    image_w4bands = raster_stack,
    # Assign the function:
    fun=ratio,
    args=list(numerator_bandNumber = numerator_bandNumber)
  )
names(ratio4) <- "ratio4"

SAVI <-
  rasterEngine(
    # Match the variable name in the function to the raster:
    image_w4bands = raster_stack,
    # Assign the function:
    fun=savi_nodrop,
    args=list(red_bandnumber = red_bandnumber, nir_bandnumber = nir_bandnumber, L = L)
  )
SAVI <- SAVI[[1]]  # convert SAVI from "brick" to raster "layer"

NDVI <-
  rasterEngine(
    # Match the variable name in the function to the raster:
    image_w4bands = raster_stack,
    # Assign the function:
    fun=ndvi_nodrop,
    args=list(red_bandnumber = red_bandnumber, nir_bandnumber = nir_bandnumber)
  )
NDVI <- NDVI[[1]]   # convert NDVI from "brick" to raster "layer"

stopCluster(cl) # Stops the cluster
registerDoSEQ()
#######################################################################################


r <- stack(raster_stack,NDVI,SAVI,ratio1,ratio2,ratio3,ratio4)
writeRaster(r,paste0(directory_path,rasterFileName,"_wRatios.tif"),overwrite =T)






## ####################    script to get GLCM   ##########################################

## ####################
## ##### INPUTS   to get GLCM
## ##########################################
## list_layersforGLCM <- namedList(NIR,SAVI)
## window_sizes <- c(3,9) # specifies the size of the window to use in glcm


## ##########################################
## all_dir <- list(c(0,1), c(1,1), c(1,0), c(1,-1))
## glcm_layers <- create_GLCM_layers_parallel(list_rasterlayers = list_layersforGLCM,
##                                            vec_window_sizes = window_sizes,
##                                            dir = all_dir,
##                                            cpus = cpus)


## glcm_stack_all <- do.call("stack",glcm_layers)

## vert_dir <- list(c(0,1))

## glcm_layers <- create_GLCM_layers_parallel(list_rasterlayers = list_layersforGLCM,
##                                            vec_window_sizes = c(3,9),
##                                            dir = vert_dir,
##                                            cpus = cpus)

## glcm_stack_vert <- do.call("stack",glcm_layers)


## #######################################################################################





## #######################################################################################

## #############  Combine all the layers created into one rasterbrick & export


## r_texture <- stack(glcm_stack_all, glcm_stack_vert)
## writeRaster(r_texture, paste0(directory_path,rasterFileName,"_wTexture.tif"),overwrite=T)
## #######################################################################################










