library(mlr)
library(kernlab)

x1 <- rnorm(50)
x2 <- rnorm(50, 3)
x3 <- rnorm(50, -20, 3)
C <- sample(c("a","b","c"), 50, T)
d <-  data.frame(x1, x2, x3, C)


classif <- makeClassifTask(id = "example", data = d, target = "C")
lrn <- makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = T)
t <- train(lrn, classif)

res1 <- predict(t, newdata = data.frame(x2,x1,x3))
res1

res2 <- predict(t$learner.model, data.frame(x2,x1,x3))
res2

res1$data$response == res2
