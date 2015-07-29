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




##############################
Read in the tuning results
##############################

#### make Learners using the best parameters found by tuning.

lei.knn.res <- readRDS(file = "../PAN_SPOT/lei.knn.res.Rdata")
lei.knn.res

knn.ress <- readRDS(file = "../PAN_SPOT/knn.res.Rdata")

lei.knn.lrn <- setHyperPars(makeLearner("classif.knn", fix.factors.prediction = T),
                            par.vals = lei.knn.res$x)
lei.knn.lrn

lei.rf.lrn <-setHyperPars(makeLearner("classif.randomForest",predict.type="prob", fix.factors.prediction = T),
                          par.vals = lei.rf.res$x)
lei.rf.lrn

lei.svm.lrn <- setHyperPars(makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = T),
                            par.vals = lei.svm.res$x)
lei.svm.lrn


lei.knn.mod <- train(lei.knn.lrn, lei_classif.task)
lei.knn.mod
lei.knn.smallimage <- predict(small_image, getLearnerModel(lei.knn.mod))



svm.lrn$par.set
knn.lrn <- makeLearner("classif.knn")
lei.knn.lrn$par.set

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
