### Libraries. ----
# install.packages("lime")
# install.packages("randomForestExplainer")
# install.packages("xgboostExplainer")
library(lime)
library(mlr)
library(live)
library(tidyverse)
library(MLExpRessoData)
library(randomForest)
library(randomForestExplainer)
library(pdp)
### Code from TCGA vignette. ----
set.seed(1)
# homemade functions
predfun <- function(object, newdata, ...) {
  randomForest:::predict.randomForest(object, newdata = newdata, type = "prob")[,1]
}
# very basic normalisation
BRCA <- MLExpRessoData::BRCA_mRNAseq_all_surv
BRCA[,-(1:2)] <- 1000000*BRCA[,-(1:2)]/rowSums(BRCA[,-(1:2)])
options(expressions = 500000)
colnames(BRCA) <- make.names(colnames(BRCA))
# try first 10000 variables
nData <- BRCA[,-1]
# pvals <- sapply(3:ncol(BRCA), function(i) wilcox.test(BRCA[,i]~BRCA[,2])$p.value) # takes a short while
# save(pvals, file = "pvals.rda")
load("pvals.rda")
# Training the black box.
nData <- BRCA[,c(2,2 + which(pvals < 0.001))]
trees <- randomForest(survival_status~., data = nData, ntree=10000, localImp = TRUE)
nind1 <- sample(which(nData$survival_status == 1), 100, replace = TRUE)
nind2 <- sample(which(nData$survival_status == 0), 100, replace = TRUE)
nData <- nData[c(nind1, nind2),]
### Explaining the model. ----
# 1. Using randomForestExplainer - globally.
# Global variable importance
# importance_frame <- randomForestExplainer::measure_importance(trees) # takes a while
# save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
# 2. Using lime package - locally.
# ?lime
lime_explanation <- lime(nData[, -which(colnames(nData) == "survival_status")], trees)
model_type.randomForest <- function(x, ...) "classification"
# better use mlr.
forest_explained <- lime::explain(nData[2, -which(colnames(nData) == "survival_status")], lime_explanation, n_labels = 1, n_features = 10)
# 3. Using live package - locally.
similar <- sample_locally(data = nData,
                          explained_instance = nData[2,],
                          explained_var = "survival_status",
                          size = 2000)
similar <- add_predictions(nData, similar,
                           black_box_model = trees,
                           predict_fun = predfun)
trained <- fit_explanation(live_object = similar,
                           white_box = "regr.lm",
                           selection = TRUE)
### Visualization tools.
# 1. Plots from randomForestExplainer
# plot_multi_way_importance(importance_frame, size_measure = "accuracy_decrease")
# 2. Plots from lime package.
plot_features(forest_explained)
plot_explanations(forest_explained)
# Add interpretation.
# 3. PDP-plots.
# pdp_explanation <- partial(trees, pred.var = c("STC2", "CALM2", "PGK1")) # takes a while
# pdp_explanation <- partial(trees, pred.var = c("CALM2"), ice = TRUE, type = "classification") # takes a while
save(pdp_explanation, file = "pdp_explanation.rda")
load("pdp_explanation.rda")
plotPartial(pdp_explanation)
# 4. ICE
install.packages("ICEbox")
library(ICEbox)
?ice
ice_explanation <- ice(trees, nData, predictor = "STC2", predictfcn = predfun)
plot(ice_explanation, plot_points_indices = 1:2, frac_to_plot = 1)
plot(ice_explanation)
ice_explanation <- ice(trees, nData, predictor = "CALM2", predictfcn = predfun)
plot(ice_explanation, plot_points_indices = 1:2, frac_to_plot = 1)
plot(ice_explanation, centered = T)
# plot(dice(ice_explanation))
# 5. Plots from live.
plot_explanation(trained,
                 regr_plot_type = "forestplot",
                 explained_instance = nData[1,])
plot_explanation(trained,
                 regr_plot_type = "waterfallplot",
                 explained_instance = nData[1,])
