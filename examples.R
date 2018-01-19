# Examples for PFI type packages.
library(pdp)
# library(ICEbox)
# library(ALEPlot)
# library(mlr)
# library(dplyr)
library(breakDown)
library(kernlab)

load("winequality_red.rda")
wine_svm <- ksvm(quality ~., data = winequality_red)
wine_pd <- pdp::partial(wine_svm, pred.var = "pH",
                        train = winequality_red[, -12])
pdp::autoplot(wine_pd)
plotPartial(wine_pd)
data("boston")
boston_svm <- e1071::svm(cmedv ~., data = boston)
pd_boston <- pdp::partial(boston_svm, pred.var = "lstat")
plotPartial(pd_boston)

boston_rf <- randomForest::randomForest(cmedv ~., data = boston)
pd_boston_rf <- pdp::partial(boston_rf, pred.var = "lstat")
dim(pd_boston_rf)
plotPartial()

library(mlr)
wine_task <- makeRegrTask("wine", winequality_red, "quality")
lrn <- makeLearner("regr.svm")
wine_svm <- train(lrn, wine_task)
wine_partial_mlr <- mlr::generatePartialDependenceData(wine_svm,
                                                       input = wine_task,
                                                       features = "pH")
mlr::plotPartialDependence(wine_partial_mlr)
wine_partial_mlr

n_distinct(winequality_red$fixed_acidity)
