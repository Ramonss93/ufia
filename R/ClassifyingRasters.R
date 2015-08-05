library("modeest")
library("stringr")
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
    foreach (j = 1:101, .packages = c("raster","randomForest")) %dopar% {  # on 101 plots
         r1 <- stack(paste0("../",image_directory,"/RastersAroundPlots/Plot",j,".tif"))
         r1 <- r1[[-18]]
         names(r1) <- attributes(list_best_mods[[2]]$learner.model@terms)$term.labels # assign names that the model was trained on.
         r2 <- raster::predict(object = r1, list_best_mods[[i]]$learner.model)
         #r2 <- add_UFIA_classes(r2)
         writeRaster(r2,filename = paste0(directory_toputClassifiedRasters,"/",attributes(list_best_mods)$names[i],"_Plot",j,".tif"),overwrite = T)
     }
 }


#  Classifying Raster surrounding field Plots  SVM
cl <- makeCluster(detectCores())
registerDoParallel(cl)
for (i in c(2,5)) {   #  SVM
    foreach (j = 1:101, .packages = c("raster")) %dopar% {  # on 101 plots
         r1 <- stack(paste0("../",image_directory,"/RastersAroundPlots/Plot",j,".tif"))
         r1 <- r1[[-18]]
         names(r1) <- attributes(list_best_mods[[2]]$learner.model@terms)$term.labels # assign names that the model was trained on.
         r2 <- raster::predict(object = r1, list_best_mods[[i]]$learner.model)
#         r2 <- add_UFIA_classes(r2)
         writeRaster(r2,filename = paste0(directory_toputClassifiedRasters,"/",attributes(list_best_mods)$names[i],"_Plot",j,".tif"),overwrite = T)
     }
 }

#  Classifying Raster surrounding field Plots  KNN
cl <- makeCluster(detectCores())
registerDoParallel(cl)
for (i in c(3,6)) {   #  KNN
    foreach (j = 1:101, .packages = c("raster", "mlr")) %dopar% {  # on 101 plots
         r1 <- stack(paste0("../",image_directory,"/RastersAroundPlots/Plot",j,".tif"))
         r1 <- r1[[-18]]
         names(r1) <- attributes(list_best_mods[[2]]$learner.model@terms)$term.labels # assign names that the model was trained on.
         r1_df <- as.data.frame(r1)
         r_pred <- predict(list_best_mods[[i]], newdata = r1_df)
         r2 <- raster(r1)
         r2[] <- r_pred$data$response
         r2
         writeRaster(r2,filename = paste0(directory_toputClassifiedRasters,"/",attributes(list_best_mods)$names[i],"_Plot",j,".tif"),overwrite = T)
     }
 }




# Creating RF and SVM probability raster layers
r <- stack("../PAN_SPOT/RastersAroundPlots/merged101plots.tif")
plotRGB(r, 4,3,2, stretch = "lin")
r <- stack("../PAN_SPOT/RastersAroundPlots/Plot1.tif")
r <- r[[-18]]
names(r) <- attributes(mod$learner.model@terms)$term.labels
rdf <- as.data.frame(r, xy =T)
a <- predict(list_best_mods[[1]], newdata = rdf)
r_prob.tree <- raster(r)
#r_prob.tree[] <- a$data$prob.tree
#r_prob.grass <- raster(r)
## r_prob.grass[] <- a$data$prob.grass
## plot(r_prob.grass)

rdf <- cbind(rdf,a)
head(rdf)

rdf <- rdf %>%
    mutate(max.prob = pmax(prob.grass, prob.impervious, prob.soil, prob.tree))
           
plt <- ggplot(data = rdf, aes(x=x, y=y, fill = response, alpha = max.prob)) + geom_raster() + coord_equal() + scale_fill_manual(values = UFIA_pal)
plt






# Creating Plots
j <- 10
mod <- list_best_mods[[5]]
r <- stack(paste0("../",image_directory,"/RastersAroundPlots/Plot",j,".tif"))
r <- r[[-18]]
names(r) <- attributes(mod$learner.model@terms)$term.labels
xy <- as.data.frame(r, xy = T)
Class <- predict(mod$learner.model, xy)
df <- cbind(xy, Class)
str(C)
str(df)
plt <- ggplot(data = r_df, aes(x = x, y = y, fill = Class)) + geom_raster() + ggtitle(title) + scale_fill_manual(values = UFIA_pal) + coord_equal()




# Create PNGs and tif layers of just impervious and just tree at 10 plots.
files <- list.files(path = paste0("../",image_directory,"/RastersAroundPlots/ClassifiedRasters"),full.names = T)
files <- files[grep(pattern = "*_Plot[0-9]+.tif$", x = files)]
files
i <- 1
for (i in 20:length(files)) {
    r <- raster(files[i])
    plot(r)
    r_df <- as.data.frame(r, xy= T)
    title <- names(r_df)[3]
    names(r_df)[3] <- "Class"
    plt <- ggplot(data = r_df, aes(x = x, y = y, fill = Class)) + geom_raster() + ggtitle(title) + scale_fill_manual(values = UFIA_pal) + coord_equal()
    png_filename <- paste0(str_sub(files[i], start = 1, end = nchar(files[i])-4), ".png")
    print(png_filename)
    png(filename = png_filename, height = 600, width = 600)
    plt
    dev.off()
                                        #separate tree cover
    r_tree <- layerize(r)[[4]]
    tree_filename <- paste0(str_sub(files[i], start = 1, end = nchar(files[i])-4), "_treeLayer.tif")

    writeRaster(r_tree, filename = tree_filename, overwrite = T)
                                        #separate impervious cover
    r_impervious <- layerize(r)[[2]]
    impervious_filename <- paste0(str_sub(files[i], start = 1, end = nchar(files[i])-4), "_imperviousLayer.tif")
    writeRaster(r_impervious, filename = impervious_filename, overwrite = T)
    plot(r_impervious)
}


            
length(files)

plot(layerize(r))[[2]]

            str(r)




head(r_df)

r <- 
plot(r)



ted.svm.plt












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








