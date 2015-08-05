library(glcm)
library(raster)
args <- commandArgs(trailingOnly = T)

#r <- stack(x = paste0("../",args[1],"/",args[2],".tif"))
#layer_forGLCM <- as.numeric(args[3])
## r <-raster::stack(x = "../PAN_SPOT/SPOT_PanSharp_subset.tif")
## layer_forGLCM <- 3

r <- raster::stack(x = "../NAIP/madison_NDVI.tif")
layer_forGLCM <- 1

r_texture <- glcm(r[[layer_forGLCM]], shift = c(0,1), na_opt = "ignore", asinteger = T)


writeRaster(r_texture, paste0("../","NAIP","/","madison","_Texture.tif"),overwrite = T, progress = "text")


#writeRaster(r_texture, paste0("../",args[1],"/",args[2],"_Texture.tif"),overwrite = T)

