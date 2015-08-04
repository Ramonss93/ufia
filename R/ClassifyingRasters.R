library("foreach")
library("doParallel")
library("e1071")
library("randomForest")
library("class")
library("dplyr")
library("magrittr")
library("snow")
library("raster")
library("rgdal")
library("mlr")
library("kernlab")
library("irace")


##################################################################################################
#                            Functions
##################################################################################################


add_UFIA_classes <- function(x) {
  x <- ratify(x)
  rat <- levels(x)[[1]]
  rat$landcover <- c('grass', 'impervious', 'soil', 'tree', 'water')
  levels(x) <- rat
  return(x)
}

UFIA_pal <- c(grass = "#FFFF99", impervious = "#F0027F", soil = "#FDC086", tree = "#7FC97F", water = "#386CB0")



##################################################################################################
##################################################################################################
#                            Classify Image
##################################################################################################
##################################################################################################

image_directory <- "PAN_SPOT"


########################################
### Load Classification Models
########################################
list_best_mods_4bands <- readRDS(paste0("../",image_directory,"/bestModels4bands.Rdata"))
list_best_mods_4bands

list_best_mods <- readRDS(paste0("../",image_directory,"/bestModels.Rdata"))
list_best_mods



######################################
###   For each raster around a plot, and for each model, predict the class of the landcover
######################################

dir.create(paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters"))

directory_toputClassifiedRasters <- paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters")



#  Classifying Raster surrounding field Plots  RandomForest
cl <- makeCluster(detectCores())
registerDoParallel(cl)
for (i in c(1,4)) {   #  rf 
    foreach (j = c(1,10,20,30,40,50,60,70,80,90), .packages = c("raster","randomForest")) %dopar% {  # on 10 plots
         r1 <- stack(paste0("../",image_directory,"/RastersAroundPlots/Plot",j,".tif"))
         r1 <- r1[[-18]]
         names(r1) <- attributes(list_best_mods[[2]]$learner.model@terms)$term.labels # assign names that the model was trained on.
         r2 <- raster::predict(object = r1, list_best_mods[[i]]$learner.model)
         r2 <- add_UFIA_classes(r2)
         writeRaster(r2,filename = paste0(directory_toputClassifiedRasters,"/",attributes(list_best_mods)$names[i],"_Plot",j,".tif"),overwrite = T)
     }
 }

r20 <- raster::predict(r1, list_best_mods[[1]]$learner.model)
plot(r20)

list_best_mods[[4]]


#  Classifying Raster surrounding field Plots  SVM
cl <- makeCluster(detectCores())
registerDoParallel(cl)
for (i in c(2,5)) {   #  SVM
    foreach (j = c(1,10,20,30,40,50,60,70,80,90), .packages = c("raster")) %dopar% {  # on 10 plots
         r1 <- stack(paste0("../",image_directory,"/RastersAroundPlots/Plot",j,".tif"))
         r1 <- r1[[-18]]
         names(r1) <- attributes(list_best_mods[[2]]$learner.model@terms)$term.labels # assign names that the model was trained on.
         r2 <- raster::predict(object = r1, list_best_mods[[i]]$learner.model)
         r2 <- add_UFIA_classes(r2)
         writeRaster(r2,filename = paste0(directory_toputClassifiedRasters,"/",attributes(list_best_mods)$names[i],"_Plot",j,".tif"),overwrite = T)
     }
 }



# Creating PNGs 
j <- 10
mod <- list_best_mods[[5]]
r <- stack(paste0("../",image_directory,"/RastersAroundPlots/Plot",j,".tif"))
r <- r[[-18]]
names(r) <- attributes(mod$learner.model@terms)$term.labels
xy <- as.data.frame(r, xy = T)
C <- predict(mod$learner.model, xy)
df <- cbind(xy, C)
str(C)
ggplot(data = df, aes(x = x, y = y, fill = C)) + geom_raster()




# Creating PNGs 
xy <- as.data.frame(r, xy = T)
C2 <- predict(mod, newdata =  xy)
df <- cbind(xy, C2$data)
str(df)

table(C2$response, df$C)


a <- df %>%
    mutate(maxprob = pmax(prob.grass, prob.impervious, prob.soil, prob.tree, prob.water))
str(a)
str(df)
ggplot(data = df, aes(x = x, y = y, fill = C)) + geom_raster()
ggplot(data = a, aes(x = x, y = y, fill = response, alpha = maxprob)) + geom_raster()











r5 <- stack("../PAN_SPOT/RastersAroundPlots/ClassifiedRasters/lei.svm.mod_Plot1.tif")
r5

plot(r5, col = UFIA_pal)






r3 <- raster::predict(object = r1, list_best_mods[[4]]$learner.model)

plot(r3, col = UFIA_pal)

                         
# For KNN I need to use the calc function





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








