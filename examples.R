# Examples: explaining single predictions.
# Libraries & data ----
if(!require(shapleyR)) {
  devtools::install_github("redichh/shapleyr")
}
library(e1071)
library(lime)
library(breakDown)
library(live)
library(mlr)
library(shapleyR)
library(tidyverse)
library(xtable)
data('wine')
nc <- which(colnames(wine) == "quality")

# Model ----
wine_svm <- svm(quality ~., data = wine)
# LIME ----
set.seed(17)
wine_expl <- lime(wine, wine_svm)
model_type.svm <- function(x, ...) "regression"
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
wine_sim <- sample_locally(data = wine,
                           explained_instance = wine[5, ],
                           explained_var = "quality",
                           size = 2000,
                           seed = 17)
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

# sessionInfo()
# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
#
# Matrix products: default
#
# locale:
#   [1] LC_COLLATE=Polish_Poland.1250  LC_CTYPE=Polish_Poland.1250    LC_MONETARY=Polish_Poland.1250
# [4] LC_NUMERIC=C                   LC_TIME=Polish_Poland.1250
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] bindrcpp_0.2.2    xtable_1.8-3      forcats_0.3.0     stringr_1.3.1     dplyr_0.7.7
# [6] purrr_0.2.5       readr_1.1.1       tidyr_0.8.1       tibble_1.4.2      tidyverse_1.2.1
# [11] live_1.5.9        breakDown_0.1.6   lime_0.4.0        e1071_1.7-0       shapleyR_0.1
# [16] reshape2_1.4.3    mlr_2.13          ParamHelpers_1.11 ggplot2_3.1.0     combinat_0.0-8
# [21] checkmate_1.8.5
#
# loaded via a namespace (and not attached):
#   [1] nlme_3.1-137       lubridate_1.7.4    devtools_1.13.6    httr_1.3.1         tools_3.5.1
# [6] backports_1.1.2    R6_2.3.0           lazyeval_0.2.1     colorspace_1.3-2   withr_2.1.2
# [11] tidyselect_0.2.5   compiler_3.5.1     parallelMap_1.3    glmnet_2.0-16      cli_1.0.1
# [16] rvest_0.3.2        xml2_1.2.0         sandwich_2.5-0     scales_1.0.0       mvtnorm_1.0-8
# [21] digest_0.6.18      stringdist_0.9.5.1 pkgconfig_2.0.2    htmltools_0.3.6    forestmodel_0.5.0
# [26] htmlwidgets_1.3    rlang_0.2.2        readxl_1.1.0       rstudioapi_0.8     BBmisc_1.11
# [31] shiny_1.2.0        bindr_0.1.1        zoo_1.8-4          jsonlite_1.5       magrittr_1.5
# [36] modeltools_0.2-22  Matrix_1.2-14      Rcpp_0.12.19       munsell_0.5.0      stringi_1.1.7
# [41] multcomp_1.4-8     yaml_2.2.0         MASS_7.3-50        plyr_1.8.4         grid_3.5.1
# [46] strucchange_1.5-1  parallel_3.5.1     promises_1.0.1     crayon_1.3.4       lattice_0.20-35
# [51] haven_1.1.2        splines_3.5.1      hms_0.4.2          magick_2.0         pillar_1.3.0
# [56] party_1.3-1        codetools_0.2-15   stats4_3.5.1       fastmatch_1.1-0    XML_3.98-1.16
# [61] glue_1.3.0         data.table_1.11.8  modelr_0.1.2       httpuv_1.4.5       foreach_1.4.4
# [66] cellranger_1.1.0   gtable_0.2.0       assertthat_0.2.0   gower_0.1.2        mime_0.6
# [71] coin_1.2-2         broom_0.5.0        later_0.7.5        class_7.3-14       survival_2.42-3
# [76] shinythemes_1.1.1  iterators_1.0.10   memoise_1.1.0      TH.data_1.0-9
