library(glcm)
library(raster)
args <- commandArgs(trailingOnly = T)

r <- stack(x = paste0("../",args[1],"/",args[2],".tif"))
layer_forGLCM <- as.numeric(args[3])

## r <-raster::stack(x = "../PAN_SPOT/SPOT_PanSharp_subset.tif")
## layer_forGLCM <- 3


r_texture <- glcm(r[[layer_forGLCM]], window = c(5,5), shift = c(0,1), na_opt = "ignore", asinteger = T)

r <- stack(r,r_texture)
writeRaster(r_texture, paste0("../",args[1],"/",args[2],"_wTexture5x5.tif"),overwrite = T)
