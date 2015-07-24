
##################################################################################################
##################################################################################################
#                            Classify Image
##################################################################################################
##################################################################################################

image_directory <- "../PAN_SPOT/"


########################################
### Load Classification Models
########################################
rf_best_mod <- load("../



###################################################
#                            Random Forest
###################################################
rf_filename <- paste0(image_directory,"ClassifiedImages/",image_name,"_rf_classification.tif")
beginCluster()
system.time(
rf_raster <- clusterR(image,predict, args = list(model = rf_best_mod))
)
endCluster()
rf_raster %<>% add_UFIA_classes()

writeRaster(rf_raster, rf_filename,overwrite = T)


###################################################
#                            SVM
###################################################
svm_filename <- paste0(image_directory,"ClassifiedImages/",image_name,"_svm_classification.tif")

beginCluster()
system.time(
  svm_raster <- clusterR(image,predict, args = list(model = svm_best_mod))
)
endCluster()
svm_raster %<>% add_UFIA_classes()

writeRaster(svm_raster, svm_filename,overwrite = T)

###################################################
#                            KNN
###################################################
knn_filename <- paste0(image_directory,"ClassifiedImages/",image_name,"_knn_classification.tif")

beginCluster()
system.time(
knn_raster <- clusterR(image, calc, args = list(fun = knn_calc), export = c("train","cl", "best_k"))
)
returnCluster()

writeRaster(knn_raster, knn_filename,overwrite = T)






##################################################################################################
##################################################################################################
#                            Plot Classified Images
##################################################################################################
##################################################################################################

plot(rf_raster, col = UFIA_pal)

plot(svm_raster, col = UFIA_pal)

plot(knn_raster, col = UFIA_pal)






