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
namedList <- function(...) {
        L <- list(...)
            snm <- sapply(substitute(list(...)),deparse)[-1]
            if (is.null(nm <- names(L))) nm <- snm
            if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
            setNames(L,nm)
    }



##################################################################################################
#                      Read in the Dataframes for classification
##################################################################################################
directory <- "PAN_SPOT"
                                        # Ted
t_df <- read.table(file = "../PAN_SPOT/ExtractedTrainingDataFrames/ted_roi_train_df.txt")

## Remove Columns that have NA's (and are also probably not very useful
detectNA <- function(x) any(is.na(x))
a <- sapply(t_df, detectNA)
t_df <- t_df[,!a]


# Sample 6000 pizxels (try to reduce spatial autocorrelation and number of pixels)
sub <- sample(x=1:nrow(t_df), size = 6000)
t_samp_df <- t_df[sub,]




                                        # Lei
l_df <- read.table(file = "../PAN_SPOT/ExtractedTrainingDataFrames/lei_roi_train_df.txt")
## Remove Columns that have NA's (and are also probably not very useful
detectNA <- function(x) any(is.na(x))
a <- sapply(l_df, detectNA)
l_df <- l_df[,!a]

## Scale the columns of the data to have mean 0 and sd 1 for svm and knn classification
#l_scld_df <- as.data.frame(scale(l_df[,1:length(l_df)-1]))
#l_scld_df$Class <- l_df$Class




##################################################################################################
#                           Trying to use package MLR
##################################################################################################
## Create Task, Tune Learners, Create learners, Get Model and Predict

####
## Create TASKS
####
ted.classif.task <- makeClassifTask(id = "ted_panSpot", data = t_samp_df, target = "Class")
ted.classif.task

ted.full.classif.task <- makeClassifTask(id = "ted_panSpot", data = t_df, target = "Class")
ted.full.classif.task

lei.classif.task <- makeClassifTask(id = "lei_panSpot", data = l_df, target = "Class")
lei.classif.task

                                        # Tune Classifiers
# Set parameters etc


ctrl <- makeTuneControlIrace(maxExperiments = 200L)
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
                    makeIntegerParam("C", lower = 5L, upper = 30L,
                                     trafo = function(x) 5*x + 50))

knn.ps <- makeParamSet(
    makeIntegerParam("k",lower = 1L, upper = 30L))

#### tune for ted and lei data, and rf and svm and knn

lei.rf.res <- tuneParams("classif.randomForest", lei_classif.task, rdesc, par.set = rf.ps, control = ctrl)
lei.svm.res <- tuneParams("classif.ksvm", lei_classif.task, rdesc, par.set = svm.ps, control = ctrl)
lei.knn.res <-  tuneParams("classif.knn", lei_classif.task, rdesc, par.set = knn.ps, control = ctrl)

ted.rf.res <-  tuneParams("classif.randomForest", ted_classif.task, rdesc, par.set = rf.ps, control = ctrl)
ted.svm.res <-  tuneParams("classif.ksvm", ted_classif.task, rdesc, par.set = svm.ps, control = ctrl)
ted.knn.res <-  tuneParams("classif.knn", ted_classif.task, rdesc, par.set = knn.ps, control = ctrl)


#### Aggregate and save results
best_tuning_results_df <- bind_rows(
    as.data.frame(lei.rf.res$opt.path),
    as.data.frame(lei.svm.res$opt.path),
    as.data.frame(lei.knn.res$opt.path),
    as.data.frame(ted.rf.res$opt.path),
    as.data.frame(ted.svm.res$opt.path),
    as.data.frame(ted.knn.res$opt.path))

write.table(best_tuning_results_df, file="../PAN_SPOT/best_tuning_results.txt")

best_tuning_results <- list(lei.rf.res, lei.svm.res, lei.knn.res, ted.rf.res, ted.svm.res, ted.knn.res)
saveRDS(best_tuning_results, file = "../PAN_SPOT/best_tuning_results.Rdata")



#### Create Learners
lei.rf.lrn <-setHyperPars(makeLearner("classif.randomForest",predict.type="prob", fix.factors.prediction = T),
                          par.vals = lei.rf.res$x)
lei.rf.lrn

lei.svm.lrn <- setHyperPars(makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = T),
                            par.vals = lei.svm.res$x)
lei.svm.lrn

lei.knn.lrn <- setHyperPars(makeLearner("classif.knn", fix.factors.prediction = T),
                            par.vals = lei.knn.res$x)
lei.knn.lrn


ted.rf.lrn <-setHyperPars(makeLearner("classif.randomForest",predict.type="prob", fix.factors.prediction = T),
                          par.vals = ted.rf.res$x)
ted.rf.lrn

ted.svm.lrn <- setHyperPars(makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = T),
                            par.vals = ted.svm.res$x)
ted.svm.lrn

ted.knn.lrn <- setHyperPars(makeLearner("classif.knn", fix.factors.prediction = T),
                            par.vals = ted.knn.res$x)
ted.knn.lrn

#### Train Learners to create models

lei.rf.mod <- train(lei.rf.lrn, lei.classif.task)
lei.svm.mod <- train(lei.svm.lrn, lei.classif.task)
lei.knn.mod <- train(lei.knn.lrn, lei.classif.task)

ted.rf.mod <- train(ted.rf.lrn, ted.full.classif.task)
ted.svm.mod <- train(ted.svm.lrn, ted.full.classif.task)
ted.knn.mod <- train(ted.knn.lrn, ted.full.classif.task)


#### Save Models

models <- namedList(lei.rf.mod, lei.svm.mod, lei.knn.mod, ted.rf.mod, ted.svm.mod, ted.knn.mod)
saveRDS(models, file = paste0("../",directory,"/bestModels.Rdata"))









#### See how well Ted's predicts Lei's and how well Lei's predicts ted's

lei.rf.predted <- predict(lei.rf.mod$learner.model, t_df)
ted.rf.predlei <- predict(ted.rf.mod$learner.model, l_df)

lei.svm.predted <- predict(lei.svm.mod$learner.model, t_df)
ted.svm.predlei <- predict(ted.svm.mod$learner.model, l_df)

classAgreement(table(lei.rf.predted, t_df$Class))
classAgreement(table(ted.rf.predlei, l_df$Class))
classAgreement(table(lei.svm.predted, t_df$Class))
classAgreement(table(ted.svm.predlei, l_df$Class))


table(ted.svm.predlei, l_df$Class)
table(lei.svm.predted, t_df$Class)
