# Examples: explaining single predictions.
# Libraries & data ----
library(e1071)
library(corrplot)
library(lime)
library(breakDown)
library(live)
library(shapleyr)
library(mlr)
library(tidyr)
library(dplyr)
library(xtable)
load("winequality_red.rda")
nc <- which(colnames(winequality_red) == "quality")
nobs <- winequality_red[5, -nc]
# Model ----
wine_svm <- svm(quality ~., data = winequality_red)
# LIME ----
wine_expl <- lime(winequality_red, wine_svm)
model_type.svm <- function(x, ...) "regression"
svm_explained <- lime::explain(nobs, wine_expl, n_features = 11)
# LIME: plot
plot_features(svm_explained)
plot_explanations(svm_explained)
# breakDown ----
explain_bd <- broken(wine_svm, new_observation = nobs,
                     data = winequality_red[, -nc],
                     predict.function = predict,
                     baseline = "Intercept")
# breakDown: plot
plot(explain_bd)
# live
wine_sim <- sample_locally(winequality_red, winequality_red[5, ], "quality", 500)
wine_sim_svm <- add_predictions(winequality_red, wine_sim, wine_svm)
wine_expl_live <- fit_explanation(wine_sim_svm, "regr.lm")
wine_expl_live2 <- fit_explanation(wine_sim_svm, "regr.lm", selection = T)
plot_explanation(wine_expl_live, "waterfallplot", winequality_red[5, ])
plot_explanation(wine_expl_live, "forestplot", winequality_red[5, ])
plot_explanation(wine_expl_live2, "waterfallplot", winequality_red[5, ])
# SHAP
# requires the use of mlr
tsk <- makeRegrTask("wine", winequality_red, "quality")
shp <- shapley(1, model = train("regr.lm", tsk), task = tsk)
# plot.shapley.singleValue(1, model = train("regr.lm", tsk), task = tsk)
brk <- data.frame(var = explain_bd$variable_name, explain_bd$contribution)
shp %>%
  gather(var, score) %>%
  full_join(brk, by = "var") %>%
  arrange(desc(abs(score))) %>%
  rename(bd_score = `explain_bd.contribution`) %>%
  mutate_if(is.numeric, function(x) round(x, 2)) %>%
  filter(!(var %in% c("Intercept", ""))) %>%
  xtable()
