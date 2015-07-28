library("doParallel")
library("e1071")
library("randomForest")
library("class")
library("dplyr")
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
  x %<>% ratify()
  rat <- levels(x)[[1]]
  rat$landcover <- c('grass', 'impervious', 'soil', 'tree', 'water')
  levels(x) <- rat
  return(x)
}

UFIA_pal <- c(grass = "#FFFF99", impervious = "#F0027F", soil = "#FDC086", tree = "#7FC97F", water = "#386CB0")


##################################################################################################
#                           Trying to use package MLR
##################################################################################################
## Create Task, Tune Learners, Create learners, Get Model and Predict

####
## Create TASKS
####
ted_classif.task <- makeClassifTask(id = "ted_panSpot", data = t_scld_sampe_df, target = "Class")
ted_classif.task

lei_classif.task <- makeClassifTask(id = "lei_panSpot", data = lei_df, target = "Class")
lei_classif.task

                                        # Tune Classifiers
# Set parameters etc


ctrl <- makeTuneControlIrace(maxExperiments = 300L)
rdesc <- makeResampleDesc("CV",iters = 3L)

rf.ps <- makeParamSet(
    makeIntegerParam("nodesize", lower = 1L, upper = 20L),
    makeIntegerParam("ntree", lower = 1L, upper = 10L,
                      trafo = function(x) 2^x),
    makeIntegerLearnerParam("mtry", lower = 1L, upper = 10L))

svm.ps <- makeParamSet(
                    makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
                    makeNumericParam("sigma", lower = -10, upper = 10,
                                     trafo = function(x) 2^x,
                                     requires = quote(kernel == "rbfdot")),
                    makeIntegerParam("degree", lower = 2L, upper = 5L,
                                     requires = quote(kernel == "polydot")),
                    makeIntegerParam("C", lower = 1L, upper = 200L))

knn.ps <- makeParamSet(
    makeIntegerParam("k",lower = 1L, upper = 30L))

#### tune for ted and lei data, and rf and svm and knn

lei.rf.res <- tuneParams("classif.randomForest", lei_classif.task, rdesc, par.set = rf.ps, control = ctrl)
lei.svm.res <- tuneParams("classif.ksvm", lei_classif.task, rdesc, par.set = svm.ps, control = ctrl)
lei.knn.res <-  tuneParams("classif.knn", lei_classif.task, rdesc, par.set = knn.ps, control = ctrl)

ted.rf.res <-  tuneParams("classif.randomForest", ted_classif.task, rdesc, par.set = rf.ps, control = ctrl)
ted.svm.res <-  tuneParams("classif.ksvm", ted_classif.task, rdesc, par.set = svm.ps, control = ctrl)
ted.knn.res <-  tuneParams("classif.knn", ted_classif.task, rdesc, par.set = knn.ps, control = ctrl)

best_tuning_results_df <- rbind.all.columns(lei.rf.res, lei.svm.res, lei.knn.res, ted.rf.res, ted.svm.res, ted.knn.res)
write.table(best_tuning_results_df, file="../PAN_SPOT/best_tuning_results.txt")

best_tuning_results <- list(lei.rf.res, lei.svm.res, lei.knn.res, ted.rf.res, ted.svm.res, ted.knn.res)
save(best_tuning_results, file = "../PAN_SPOT/best_tuning_results.Rdata")


#### make Learners using the best parameters found by tuning.




rf.lrn <- makeLearner("classif.randomForest",predict.type="prob", fix.factors.prediction = T)
rf.lrn

svm.lrn <- makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = T)
svm.lrn
svm.lrn$par.set
knn.lrn <- makeLearner("classif.knn")
knn.lrn$par.set
knn.mod <- train(knn.lrn, classif.task)
a <- getLearnerModel(knn.mod)

rf.mod <- train(rf.lrn, classif.task)
rf.mod
getLearnerModel(rf.mod)

svm.mod <- train(svm.lrn,classif.task)
svm.mod
getLearnerModel(svm.mod)

good.mod <- getLearnerModel(svm.mod)

i <- predict(small_image,good.mod)
plot(i)


plot


                                        # create multiplexed learner
lrn <- makeModelMultiplexer(list(
    makeLearner("classif.randomForest"),
    makeLearner("classif.ksvm")
    ))

                                        # wrap in tuning
inner <- makeResampleDesc("CV", iters = 3L)
ctrl <- makeTuneControlIrace(maxExperiments = 300)
tune.ps <- makeModelMultiplexerParamSet(lrn,
                                        makeIntegerParam("nodesize", lower = 1L, upper = 20L),
                                        makeIntegerParam("ntree", lower = 1L, upper = 3L,
                                                         trafo = function(x) 10^x),
                                        makeIntegerLearnerParam("mtry", lower = 1L, upper = 10L),
                                        makeDiscreteParam("kernel", values = c("vanilladot", "rbfdot")),
                                        makeNumericParam("sigma", lower = -10, upper = 10,
                                                         trafo = function(x) 2^x,
                                                         requires = quote(kernel == "rbfdot"))
                                        )

lrn <- makeTuneWrapper(lrn, inner, mmce, tune.ps, ctrl)
lrn
task <- makeClassifTask(data = lei_df, target = "Class")
task
outer <- makeResampleDesc("CV")

res <- resample(lrn, task, outer, models = TRUE)
res$models[[1]]
res

## This is a way that failed because it was too big for irace
##                                         # create multiplexed learner
## lrn <- makeModelMultiplexer(list(
##     makeLearner("classif.randomForest"),
##     makeLearner("classif.ksvm"),
##     makeLearner("classif.knn")
##     ))

##                                         # wrap in tuning
## inner <- makeResampleDesc("CV", iters = 3L)
## ctrl <- makeTuneControlIrace(maxExperiments = 500)
## tune.ps <- makeModelMultiplexerParamSet(lrn,
##                                         makeIntegerParam("nodesize", lower = 1L, upper = 20L),
##                                         makeIntegerParam("ntree", lower = 1L, upper = 3L,
##                                                          trafo = function(x) 10^x),
##                                         makeIntegerLearnerParam("mtry", lower = 1L, upper = 10L),
##                                         makeDiscreteParam("kernel", values = c("vanilladot", "polydot", "rbfdot")),
##                                         makeNumericParam("sigma", lower = -10, upper = 10,
##                                                          trafo = function(x) 2^x,
##                                                          requires = quote(kernel == "rbfdot")),
##                                         makeIntegerParam("degree", lower = 2L, upper = 5L,
##                                                          requires = quote(kernel == "polydot")),
##                                         makeIntegerParam("k", lower = 1, upper = 100)
##                                         )

## lrn <- makeTuneWrapper(lrn, inner, mmce, tune.ps, ctrl)
## lrn
## task <- makeClassifTask(data = lei_df, target = "Class")
## task
## outer <- makeResampleDesc("CV")

## res <- resample(lrn, task, outer, models = TRUE)
## res$models[[1]]
## res



##################################################################################################
#                            Create Predictive Models   This is the old way.  I should try with package mlr
##################################################################################################



df <- classified_px

# Subsetting classified pixels
sub <- sample(1:nrow(df),size =1000, replace = F)
df <- df[sub,]

# df_test <- classified_px[-sub,]
# df_test <- df_test[sample(1:nrow(df_test), size = 1000),]

# Generating Formula
pred_cols <- names(image)
class_formula <- paste0("Class ~ ",paste(pred_cols, collapse=" + "))
class_formula


###################################################
#                            Random Forest
###################################################

mtry_range <- seq(1, length(pred_cols)/2, 4)
nodesize_range <- c(5,10,30,50,100)
ntree_range <- seq(100,600,50)

rf_out <- tune(randomForest,
               as.formula(class_formula),
               data = df,
               ranges = list(mtry = mtry_range,
                             nodesize = nodesize_range,
                             ntree = ntree_range))
summary(rf_out)
rf_best_mod <- rf_out$best.model
rf_best_mod

save(rf_best_mod, file =  "classificationModels/rf_best_mod")

###################################################
#                            SVM
###################################################
svm_out <- tune(svm,
                as.formula(class_formula),
                data = df,
                ranges = list(kernel = c("radial","linear"),
                              cost = c(10,90,100,300),
                              gamma = c(0.001,0.01,0.1,1,10,100)))


svm_out <- tune(svm,
                as.formula(class_formula),
                data = df,
                ranges = list(kernel = c("radial","linear"),
                              cost = c(0.1,1,5,10),
                              gamma = c(0.001,0.01,0.1)))

summary(svm_out)
svm_best_mod <- svm_out$best.model
svm_best_mod

###################################################
#                            KNN
###################################################

train <- as.matrix(df[,pred_cols])
cl <- df[,"Class"]
k_range <- seq(1,50,5)

knn_out <- tune.knn(x=train,
                    y = cl,
                    k = k_range,
                    tunecontrol = tune.control(sampling = "boot"))
summary(knn_out)
plot(knn_out)
best_k <- knn_out$best.parameters
best_k

knn_calc <- function(x) knn(train, x, cl, k = best_k)






lei_pred <- predict(svm_best_mod, lei_df)


table(lei_pred,lei_df$Class)
??accuracy
