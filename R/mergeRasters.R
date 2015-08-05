library(raster)

#merging stacked rasters around plots
rasters_to_merge <- list.files(path = "../PAN_SPOT/RastersAroundPlots", full.names = T)
rasters_to_merge <- rasters_to_merge[grep(pattern = "Plot[0-9]*.tif", x = rasters_to_merge)]
rasters_to_merge

r_list <- lapply(rasters_to_merge, FUN = stack)
plots_101 <- do.call(merge, args = r_list)
writeRaster(plots_101, filename = "../PAN_SPOT/RastersAroundPlots/merged101plots.tif")



#merging classifications around plots

image_directory <- "PAN_SPOT"
classifications <- list.files(path = paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters"),full.names = T)



ted.rf.classifications <- classifications[grep(pattern = "*ted.rf.mod_Plot[0-9]+.tif$", x = classifications)]
r_list <- lapply(ted.rf.classifications, FUN = raster)
ted.rf.RastersAtEachPlot <- do.call(merge, args=r_list)
writeRaster(ted.rf.RastersAtEachPlot, filename = paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters/","ted.rf.tif"), overwrite = T)
plot(ted.rf.RastersAtEachPlot)

ted.svm.classifications <- classifications[grep(pattern = "*ted.svm.mod_Plot[0-9]+.tif$", x = classifications)]
r_list <- lapply(ted.svm.classifications, FUN = raster)
ted.svm.RastersAtEachPlot <- do.call(merge, args=r_list)

writeRaster(ted.svm.RastersAtEachPlot, filename = paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters/","ted.svm.tif"), overwrite = T)


ted.knn.classifications <- classifications[grep(pattern = "*ted.knn.mod_Plot[0-9]+.tif$", x = classifications)]
ted.knn.classifications
r_list <- lapply(ted.knn.classifications, FUN = raster)
r_list
ted.knn.RastersAtEachPlot <- do.call(merge, args=r_list)
writeRaster(ted.knn.RastersAtEachPlot, filename = paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters/","ted.knn.tif"), overwrite = T)


lei.rf.classifications <- classifications[grep(pattern = "*lei.rf.mod_Plot[0-9]+.tif$", x = classifications)]
r_list <- lapply(lei.rf.classifications, FUN = raster)
lei.rf.RastersAtEachPlot <- do.call(merge, args=r_list)
writeRaster(lei.rf.RastersAtEachPlot, filename = paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters/","lei.rf.tif"), overwrite = T)


lei.svm.classifications <- classifications[grep(pattern = "*lei.svm.mod_Plot[0-9]+.tif$", x = classifications)]
r_list <- lapply(lei.svm.classifications, FUN = raster)
lei.svm.RastersAtEachPlot <- do.call(merge, args=r_list)
writeRaster(lei.svm.RastersAtEachPlot, filename = paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters/","lei.svm.tif"), overwrite = T)
plot(lei.svm.RastersAtEachPlot)

lei.knn.classifications <- classifications[grep(pattern = "*lei.knn.mod_Plot[0-9]+.tif$", x = classifications)]
r_list <- lapply(lei.knn.classifications, FUN = raster)
lei.knn.RastersAtEachPlot <- do.call(merge, args=r_list)
writeRaster(lei.knn.RastersAtEachPlot, filename = paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters/","lei.knn.tif"), overwrite = T)
plot(lei.knn.RastersAtEachPlot)


