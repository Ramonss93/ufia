                                        # This file calls the raster package's as.data.frame function

                                        # This converts the image into a format that I can easily use the machine learning predict functions for (for those that won't be able to predict using the raster packages's predict function)


library(raster)


args <- commandArgs(trailingOnly = T)


Rb <- brick("../PAN_SPOT/geomatica_SPOT_panshp_wAdditionalFeatures.tif")
Rb <- brick("../PAN_SPOT/geomatica_SPOT_panshp.tif")

rb <- brick(paste0("../",args[1],".tif"))
rb_test <- brick("../PAN_SPOT/SPOT_PanSharp_subsubset_wAddedFeatures.tif")

rb_df <- as.data.frame(rb_test, xy=T)

s
summary(rb_df)
str(rb_df)



bs <- blockSize(Rb)
canProcessInMemory(Rb)

largeRaster.as.data.frame <- function(raster, filename){
    bs <- blockSize(raster)
    for (i in 1:bs$n){
        v <- getValues(raster, row=
    }
}
