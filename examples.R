# Examples for PFI type packages.
library(e1071)
library(corrplot)
library(lime)
library(breakDown)

load("winequality_red.rda")
nc <- which(colnames(winequality_red) == "quality")
nobs <- winequality_red[5, -nc]

wine_svm <- svm(quality ~., data = winequality_red)

wine_expl <- lime(winequality_red[, -nc], wine_svm)
model_type.svm <- function(x, ...) "regression"
svm_explained <- lime::explain(nobs,
                               wine_expl, n_features = 10)

plot_features(svm_explained)
plot_explanations(svm_explained)

# breakDown
explain_bd <- broken(wine_svm, new_observation = nobs,
                     data = winequality_red[, -nc],
                     predict.function = kernlab:::predict)
plot(explain_bd)
