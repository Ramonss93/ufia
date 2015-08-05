library(mlr)
library(kernlab)
library(raster)


x1 <- rnorm(16)
x2 <- rnorm(16, 3)

r1 <- raster(ncol = 4, nrow = 4)
r1[] <- x1

r2 <- raster(ncol = 4, nrow = 4)
r2[] <- x2

s <- stack(r1,r2)

x1 <- rnorm(10)
x2 <- rnorm(10, 3)
C <- sample(c("a","b","c"), 16, T)
d <-  data.frame(x1, x2, C)
d
classif <- makeClassifTask(id = "example", data = d, target = "C")
nd <- data.frame(x2,x1)

lrn <- makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = T)
t <- train(lrn, classif)
t$learner.model

raster::predict(s, t$learner.model)
raster::predict(s, t)

s_df <- as.data.frame(s)
s_df
predict(t, newdata = s_df)


res1 <- predict(t, newdata = nd)
res1

res2 <- predict(t$learner.model, newdata = nd)
res2

res3 <- predict(t$learner.model, newdata = nd, type = "probabilities")
res3

lrn <- makeLearner("classif.ksvm", predict.type = "response", fix.factors.prediction = T)
t <- train(lrn, classif)
res4 <- predict(t, newdata = nd)
res4

res5 <- predict(t$learner.model, newdata = nd)
res5

res6 <- predict(t$learner.model, newdata = nd, type = "probabilities")
res6

res4$data$response==res5


t <- ksvm(C~., data = d, prob.model = T)
t
res3 <- kernlab::predict(t, newdata = data.frame(x2,x1,x3), type = "probabilities")
res4 <- kernlab::predict(t, newdata = data.frame(x2,x1,x3), type = "response")
str(res3)
res3
res4





res2 <- predict(t$learner.model, data.frame(x2,x1,x3))
res2



x1_scld <- scale(x1)
x2_scld <- scale(x2)
x3_scld <- scale(x3)
C <- sample(c("a","b","c"), 50, T)
d <-  data.frame(x1_scld, x2_scld, x3_scld, C)


classif <- makeClassifTask(id = "example", data = d, target = "C")
lrn <- makeLearner("classif.ksvm", predict.type = "response", fix.factors.prediction = T)
t <- train(lrn, classif)

res1 <- predict(t, newdata = data.frame(x2_scld,x1_scld,x3_scld))
res1

res2 <- predict(t$learner.model, data.frame(x2_scld,x1_scld,x3_scld))
res2

res1$data$response == res2











library(randomForest)

classif <- makeClassifTask(id = "example", data = d, target = "C")
lrn <- makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = T)
t <- train(lrn, classif)

res1 <- predict(t, newdata = data.frame(x2,x1,x3))
res1

res2 <- predict(t$learner.model, data.frame(x2,x1,x3))
res2

res1$data$response == res2
