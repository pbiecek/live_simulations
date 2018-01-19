# Examples for PFI type packages.
library(e1071)
library(pdp)
library(ICEbox)
library(ALEPlot)
library(corrplot)
library(lime)
library(breakDown)

corrplot(cor(winequality_red))

load("winequality_red.rda")
winequality_red <- as.data.frame(winequality_red) # important. pdp sucks with tibbles
wine_svm <- svm(quality ~., data = winequality_red)
wine_pd <- pdp::partial(wine_svm, pred.var = "pH",
                        train = winequality_red)
plotPartial(wine_pd)

wine_ice <- ice(wine_svm, winequality_red, winequality_red$quality,
                predictor = "pH")
plot(wine_ice, plot_points_indices = 1:2, frac_to_plot = 1)


ALEPlot(winequality_red[, -12], wine_svm,
        J = which(colnames(winequality_red) == "pH"),
        pred.fun = function(X.model, newdata) predict(X.model, newdata))

data("HR_data")
hr_svm <- svm(left ~., data = HR_data)
HR_data[888, "left"]
predict(hr_svm)[888]
nc <- which(colnames(HR_data) == "left")
hr_expl <- lime(HR_data[, -nc], hr_svm)
model_type.svm <- function(x, ...) "classification"
svm_explained <- lime::explain(HR_data[88, -nc],
                               hr_expl, n_labels = 1, n_features = 10)

plot_features(svm_explained)
plot_explanations(svm_explained)


### Maybe write an issue ----
# data("WhiteWine")
# white_svm <- svm(quality ~., data = WhiteWine)
# white_ice <- ice(white_svm, WhiteWine, WhiteWine$quality,
#                  predictor = "pH")
# plot(wine_ice, plot_points_indices = 1, frac_to_plot = 1,
#      plot_pdp = F)
