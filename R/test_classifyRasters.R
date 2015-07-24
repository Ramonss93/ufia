test rasterEngine classification
require("randomForest")
require("spatial.tools")
require("e1071")
require("class")
require("dplyr")

# first find the speed to classify knn without rasterEngine

# Load up a 3-band image:
tahoe_highrez <- setMinMax(
  brick(system.file("external/tahoe_highrez.tif", package="spatial.tools")))
tahoe_highrez
plotRGB(tahoe_highrez)

# Load up some training points:
tahoe_highrez_training_points <- readOGR(
  dsn=system.file("external", package="spatial.tools"),
  layer="tahoe_highrez_training_points")

# View(tahoe_highrez_training_points)


# Extract data to train the randomForest model:
tahoe_highrez_training_extract <- raster::extract(
  tahoe_highrez,
  tahoe_highrez_training_points,
  df=TRUE) %>%
  dplyr::select(-ID)

# Add the class column

tahoe_highrez_training_extract$Class <- tahoe_highrez_training_points$SPECIES



##################################################
##################################################



USE RASTER::PREDICT capability!!!!!!!!!!!!!!!!!
  this is prevent me from having to turn the raster brick into a dataframe


df <- tahoe_highrez_training_extract

# Use random forest to predict class for each pixel in the image.
tahoe_rf <- randomForest(Class ~ .,
                         data=tahoe_highrez_training_extract
)



pred_cols <- names(tahoe_highrez)
class_formula <- paste0("Class ~ ",paste(pred_cols, collapse=" + "))
class_formula

rf_best_mod <- best.tune(randomForest, 
                      as.formula(class_formula),
                      data = df,
                      ranges = list(mtry = 3,
                                    nodesize = 5:6,
                                    ntree = seq(1,11,10)))

rf_best_mod
system.time(
tahoe_rf_raster <- predict(tahoe_highrez, tahoe_rf)
)
tahoe_df <- as.data.frame(tahoe_highrez)

sfQuickInit(cpus = 2)
system.time(
rf_raster <- predict_rasterEngine(object = rf_best_mod, newdata = tahoe_highrez)
)
sfQuickStop()
plot(rf_raster)
plot(tahoe_rf_raster)







PrepDF_forKNN <- function(df, predcols, respcol, test_pct = 20) {
  train_pct = 100 - test_pct
  #Seeding for reproducibility
  set.seed(test_pct)
  test_indx <- sample(nrow(df), nrow(df) * test_pct/100, replace=FALSE)
  test.all <- df[test_indx,]
  
  test.preds <- test.all[,predcols]
  test.resp <- test.all[,respcol]
  
  #Prep. training data  which is nothing but remaing test data
  train.all <- df[-test_indx,]
  train.preds <- train.all[,predcols]
  train.resp <- train.all[,respcol]
  
  rtrn_list <- list(testPreds = as.matrix(test.preds), testResp = as.matrix(test.resp), trainPreds = as.matrix(train.preds), trainResp = as.matrix(train.resp))
  
  return(rtrn_list)
  
}

df <- tahoe_highrez_training_extract
resp_col <- "Class"
pred_cols <- colnames(df) [! colnames(df) %in% resp_col]


# Tune the KNN with the training and test data
x <- as.matrix(df[,pred_cols])
y <- as.factor(as.matrix(df[,resp_col]))


obj2 <- tune.knn(x, y, k = seq(1,20,1))
knn_best_mod <- obj2$best.model
plot(obj2)
best_k <- obj2$best.parameters[[1]]
best_k
knn_in <- PrepDF_forKNN(df,predcols = pred_cols, respcol = resp_col, test_pct = 50)
knn.pred <- knn(train = knn_in$trainPreds, test = knn_in$testPreds, cl = knn_in$trainResp, k = best_k)

table(knn_in$testResp,knn.pred)
classAgreement(table(knn_in$testResp,knn.pred))
Accuracy <- classAgreement(table(knn_in$testResp,knn.pred))$diag
Accuracy



# Use KNN to predict class for each pixel in the image
knn_in <- PrepDF_forKNN(df,predcols = pred_cols, respcol = resp_col, test_pct = 2)
t_df <- as.data.frame(tahoe_highrez)
train_pred_mat <- as.matrix(df[,pred_cols])
train_cls_mat <- as.matrix(df [, resp_col])
knn.pred <- knn(train = train_pred_mat, test = as.matrix(t_df), cl = train_cls_mat, k = 3)
knn.raster <- raster(tahoe_highrez)
values(knn.raster) <- knn.pred

quartz()
plot(knn.raster)
quartz()
plotRGB(tahoe_highrez)













# Use random forest to predict class for each pixel in the image.
train <- sample(1:nrow(tahoe_highrez_training_extract), nrow(tahoe_highrez_training_extract)/2)
tahoe_rf <- randomForest(Class ~ .,
                         data=tahoe_highrez_training_extract
                         )

best.tune(randomForest, 
     Class ~ . ,
     data = df,
     ranges = list(mtry = 1:3,
                   nodesize = 1:10,
                   ntree = seq(1,500,10)))


tahoe_rf
tahoe_rf_raster <- predict(tahoe_highrez, tahoe_rf)

quartz()
plot(tahoe_rf_raster)




# Use svm to predic class for each pixel in the image
tune.out <- tune(svm, Class ~ ., data = df,
                     ranges = list(kernel = c("linear", "radial", "polynomial"), 
                                   cost = c(0.001, 0.1, 1, 5, 10, 100),
                                   gamma = c(0.5,1,2,3,4)))
best.mod <- tune.out$best.model

tahoe_svm_raster <- predict(tahoe_highrez, best.mod)


quartz()
plot(tahoe_svm_raster)


a





















































































##### E. Using predict() with rasterEngine #####
# Typically, classification/continuous variable models
# are developed and then applied one band at a time.
# Because the function receives the pixel data as an
# array, and predict often wants a matrix or
# data.frame, we must first have our function convert
# the array of pixels to a data.frame. Also, the
# function should contain the library/require statement
# for any packages it might need.

require(c("spatial.tools","doParallel"))

# Begin a parallel cluster and register it with foreach:
cpus = 2 # The number of nodes/cores to use in the cluster
# This can be modified to fit a specific cluster/multicore computer setup
cl <- makeCluster(spec = cpus, type = "PSOCK", methods = FALSE)
# Register the cluster with foreach:
registerDoParallel(cl)

# Load up a 3-band image:
tahoe_highrez <- setMinMax(
  brick(system.file("external/tahoe_highrez.tif", package="spatial.tools")))
tahoe_highrez
plotRGB(tahoe_highrez)

# Load up some training points:
tahoe_highrez_training_points <- readOGR(
  dsn=system.file("external", package="spatial.tools"),
  layer="tahoe_highrez_training_points")

# View(tahoe_highrez_training_points)


# Extract data to train the randomForest model:
tahoe_highrez_training_extract <- extract(
  tahoe_highrez,
  tahoe_highrez_training_points,
  df=TRUE)

# Fuse it back with the SPECIES info:
tahoe_highrez_training_extract$SPECIES <- tahoe_highrez_training_points$SPECIES

# Note the names of the bands:
names(tahoe_highrez_training_extract) # the extracted data
names(tahoe_highrez) # the brick

# Generate a randomForest model:
tahoe_rf <- randomForest(SPECIES~tahoe_highrez.1+tahoe_highrez.2+tahoe_highrez.3,
                         data=tahoe_highrez_training_extract)

tahoe_rf

# This model will be passed along as a parameter to our function.

randomForest_raster_predict <- function(inraster,rfModel,...)
{
  # We need to load randomForest (note this is only
  # loaded once per worker on most cluster setups):
  require(randomForest)
  
  # First, preserve the names:
  band_names <- dimnames(inraster)[3][[1]]
  
  # This will "flatten" the array to a matrix (we lose the names here):
  inraster_matrix <- inraster
  dim(inraster_matrix) <- c(dim(inraster)[1]*dim(inraster)[2],dim(inraster)[3])
  
  # Now, let’s coerce this to a data.frame:
  inraster.df <- as.data.frame(inraster_matrix)
  
  # We need to re-set the names because randomForest requires it:
  names(inraster.df) <- band_names
  
  # Now we can use this in a predict statement:
  out_predictions <- predict(rfModel,inraster.df)
  
  # We must return this as a numeric array (a raster cannot
  # use character classes), and set the dims to match the input.
  # Also, right now rasterEngine requires ALL outputs to be double
  # precision floating point, so we cannot use "as.numeric" on the
  # factor, because that will return an integer.
  
  out_predictions_array <- array(as.double(out_predictions),dim=c(dim(inraster)[1:2],1))
  
  return(out_predictions_array)
}

# Now, rasterEngine. Notice we pass the randomForest model along
# via the args= setting.
system.time(
  tahoe_lidar_rf_class <-
    rasterEngine(
      # Match the variable name in the function to the raster:
      inraster=tahoe_highrez,
      # Assign the function:
      fun=randomForest_raster_predict,
      args=list(rfModel=tahoe_rf)
    )
)
tahoe_lidar_rf_class <- setMinMax(tahoe_lidar_rf_class)
tahoe_lidar_rf_class
plot(tahoe_lidar_rf_class)

# Stop the cluster
stopCluster(cl) # Stops the cluster
registerDoSEQ() # (Optional but avoids a warning) registers a sequential backend with foreach.




