library(mlr)
library(live)
library(tidyverse)
library(bigstep)
library(data.table)

wine_task <- makeRegrTask("wine", winequality_red, target = "quality")
methods <- makeLearners(c("randomForest", "svm", "lm", "nnet"), type = "regr")

wine_benchmark <- benchmark(methods, wine_task)
wine_benchmark

# library(xtable)
# xtable(wine_benchmark)
# xtable(winequality_red[5, ])

wine_svm <- train("regr.svm", wine_task)
predict(wine_svm, task = wine_task)

wine_task2 <- makeRegrTask("wine", winequality_red[-5, ], target = "quality")
methods2 <- makeLearners(c("randomForest", "svm", "lm", "nnet"), type = "regr")

wine_svm2 <- train("regr.svm", wine_task2)
predict(wine_svm2, newdata = winequality_red[1, ])

similar <- sample_locally(data = winequality_red,
                          explained_instance = winequality_red[1, ],
                          explained_var = "quality",
                          size = 500,
                          standardise = TRUE)
similar <- add_predictions(data = winequality_red,
                           to_explain = similar,
                           black_box_model = "regr.svm")
wine_expl <- fit_explanation(live_object = similar,
                             white_box = "regr.lm",
                             selection = TRUE)
plot_explanation(model = wine_expl,
                 regr_plot_type = "waterfallplot",
                 explained_instance = winequality_red[1, ])
plot_explanation(model = wine_expl,
                 regr_plot_type = "forestplot",
                 explained_instance = winequality_red[1, ])
