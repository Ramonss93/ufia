library(mlr)
library(kernlab)

set.seed(8)
x1 <- rnorm(10)
x2 <- rnorm(10, 3)
x3 <- rnorm(10, -20, 3)
C <- sample(c("a","b","c"), 10, T)
d <-  data.frame(x1, x2, x3, C)
classif <- makeClassifTask(id = "example", data = d, target = "C")
nd <- data.frame(x2,x1,x3)

lrn <- makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = T)
t <- train(lrn, classif)
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
