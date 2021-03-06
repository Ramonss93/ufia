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

rbind.all.columns <- function(x, y) {

        x.diff <- setdiff(colnames(x), colnames(y))
            y.diff <- setdiff(colnames(y), colnames(x))

            x[, c(as.character(y.diff))] <- NA

            y[, c(as.character(x.diff))] <- NA

            return(rbind(x, y))
    }



##################################################################################################
#                      Read in the Dataframes for classification
##################################################################################################
                                        # Ted
t_df <- read.table(file = "../PAN_SPOT/ExtractedTrainingDataFrames/ted_roi_train_df.txt")

## Remove Columns that have NA's (and are also probably not very useful
detectNA <- function(x) any(is.na(x))
a <- sapply(t_df, detectNA)
t_df <- t_df[,!a]


## Scale the columns of the data to have mean 0 and sd 1 for svm and knn classification
t_scld_df <- as.data.frame(scale(t_df[,1:length(t_df)-1]))
t_scld_df$Class <- t_df$Class
# Sample 6000 pizxels (try to reduce spatial autocorrelation and number of pixels)
sub <- sample(x=1:nrow(t_scld_df), size = 6000)
t_scld_samp_df <- t_scld_df[sub,]




                                        # Lei
l_df <- read.table(file = "../PAN_SPOT/ExtractedTrainingDataFrames/lei_roi_train_df.txt")
## Remove Columns that have NA's (and are also probably not very useful
detectNA <- function(x) any(is.na(x))
a <- sapply(l_df, detectNA)
l_df <- l_df[,!a]
## Scale the columns of the data to have mean 0 and sd 1 for svm and knn classification
l_scld_df <- as.data.frame(scale(l_df[,1:length(l_df)-1]))
l_scld_df$Class <- l_df$Class




##################################################################################################
#                           Trying to use package MLR
##################################################################################################
## Create Task, Tune Learners, Create learners, Get Model and Predict

####
## Create TASKS
####
ted_classif.task <- makeClassifTask(id = "ted_panSpot", data = t_scld_samp_df, target = "Class")
ted_classif.task

lei_classif.task <- makeClassifTask(id = "lei_panSpot", data = l_scld_df, target = "Class")
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


#### Aggregate and save results

best_tuning_results_df <- rbind.all.columns(lei.rf.res, lei.svm.res, lei.knn.res, ted.rf.res, ted.svm.res, ted.knn.res)
write.table(best_tuning_results_df, file="../PAN_SPOT/best_tuning_results.txt")

best_tuning_results <- list(lei.rf.res, lei.svm.res, lei.knn.res, ted.rf.res, ted.svm.res, ted.knn.res)
save(best_tuning_results, file = "../PAN_SPOT/best_tuning_results.Rdata")

tasks <- list(ted_classif.task, lei_classif.task)
save(tasks, file = "../PAN_SPOT/classifTasks.Rdata")
