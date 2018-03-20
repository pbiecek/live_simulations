# Examples for PFI type packages.
library(e1071)
library(corrplot)
library(lime)
library(breakDown)

corrplot(cor(winequality_red))

load("winequality_red.rda")
wine_svm <- svm(quality ~., data = winequality_red)

nc <- which(colnames(winequality_red) == "quality")
wine_expl <- lime(winequality_red[, -nc], wine_svm)
model_type.svm <- function(x, ...) "regression"
svm_explained <- lime::explain(winequality_red[88, -nc],
                               wine_expl, n_features = 10)

plot_features(svm_explained)
plot_explanations(svm_explained)

# breakDown
explain_bd <- broken(wine_svm, new_observation = winequality_red[88, -nc],
                     data = winequality_red[, -nc],
                     predict.function = kernlab:::predict)
plot(explain_bd)

### Maybe write an issue ----
# data("WhiteWine")
# white_svm <- svm(quality ~., data = WhiteWine)
# white_ice <- ice(white_svm, WhiteWine, WhiteWine$quality,
#                  predictor = "pH")
# plot(wine_ice, plot_points_indices = 1, frac_to_plot = 1,
#      plot_pdp = F)
