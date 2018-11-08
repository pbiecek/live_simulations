# Examples: explaining single predictions.
# Libraries & data ----
library(e1071)
library(lime)
library(breakDown)
library(live)
library(shapleyR)
library(mlr)
library(tidyr)
library(dplyr)
library(xtable)
data('wine')
nc <- which(colnames(wine) == "quality")

# Model ----
wine_svm <- svm(quality ~., data = wine)
# LIME ----
wine_expl <- lime(wine, wine_svm)
model_type.svm <- function(x, ...) "regression"
set.seed(17)
svm_explained <- lime::explain(wine[5, ], wine_expl, n_features = 11)
# LIME: plot
plot_features(svm_explained)
# breakDown ----
explain_bd <- broken(wine_svm, new_observation = wine[5, -nc],
                     data = wine[, -nc],
                     baseline = "Intercept",
                     keep_distributions = TRUE)
# breakDown: plot
plot(explain_bd)
plot(explain_bd, plot_distributions = T)
# live
wine_sim <- sample_locally(wine, wine[5, ], "quality", 2000, seed = 17)
wine_sim_svm <- add_predictions(wine_sim, wine_svm)
wine_expl_live <- fit_explanation(wine_sim_svm)
plot(wine_expl_live, "waterfall")
plot(wine_expl_live, "forest")
wine_expl_tree <- fit_explanation(wine_sim_svm, "regr.ctree", kernel = identity_kernel,
                                  hyperpars = list(maxdepth = 2))
plot(wine_expl_tree)
# SHAP
# requires the use of mlr
tsk <- makeRegrTask("wine", wine, "quality")
set.seed(17)
shp <- shapley(5, model = train("regr.svm", tsk), task = tsk)
# plot.shapley.singleValue(1, model = train("regr.lm", tsk), task = tsk)
brk <- data.frame(var = explain_bd$variable_name, explain_bd$contribution)
shp$values %>%
  gather(var, score) %>%
  full_join(brk, by = "var") %>%
  filter(!(var %in% c("Intercept", "", "_Id", "_Class"))) %>%
  mutate(score = as.numeric(score)) %>%
  arrange(desc(abs(score))) %>%
  rename(bd_score = `explain_bd.contribution`) %>%
  mutate_if(is.numeric, function(x) round(x, 2)) %>%
  xtable()
class(shp) <- c("shapley.singleValue", "list")
plot(shp)
