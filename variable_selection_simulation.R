### Libraries ----
library(MASS)
library(tidyverse)
library(live)
library(mlr)
library(e1071)
library(bigstep)
### Simulation: false and true discoveries in different settings. ----
replications <- 10000
params_number <- 20
black_box <- "regr.svm"
design_matrix <- mvrnorm(200, rep(0, params_number), diag(1, params_number))
# Linear model, no variable selection.
experiment_simple <- lapply(1:replications, function(x) {
    non_zero_parameters <- sample(params_number, 4)
    parameters <- rep(0, params_number)
    parameters[non_zero_parameters] <- 4
    full_df <- design_matrix %>%
        as_tibble() %>%
        mutate(y = design_matrix%*%parameters + rnorm(nrow(.)))
    similar <- simulateSimilar(full_df, full_df[5, ], "y", black_box, size = 50)
    regr_obj <- getLearnerModel(trainWhiteBox(similar, "regr.lm"))
    summ <- summary(regr_obj)
    p_vals <- summ$coefficients[-1, 4]
    which_params <- which(p_vals < 0.05)
    tibble(r_squared = summ$r.squared,
           FalseDiscoveryN = length(setdiff(which_params, non_zero_parameters)),
           TrueDiscoveryN = sum(non_zero_parameters %in% which_params),
           estimError = sum((parameters[[1]] - coef(regr_obj)[-1])^2))
}) %>%
    bind_rows()
# save(experiment_simple, file = "experiment_simple.rda")
# Linear model, stepwise variable selection.
experiment_step <- lapply(1:replications, function(x) {
    non_zero_parameters <- sample(params_number, 4)
    parameters <- rep(0, params_number)
    parameters[non_zero_parameters] <- 4
    full_df <- design_matrix %>%
        as_tibble() %>%
        mutate(y = design_matrix%*%parameters + rnorm(nrow(.)))
    similar <- simulateSimilar(full_df, full_df[5, ], "y", black_box, size = 50)
    which_params <- selectModel(as.data.frame(similar$data[, -21]),
                                as.data.frame(similar$data$y), crit = aic)
    which_params <- as.numeric(gsub("V", "", which_params))
    tibble(FalseDiscoveryN = length(setdiff(which_params, non_zero_parameters)),
           TrueDiscoveryN = sum(non_zero_parameters %in% which_params))
}) %>%
    bind_rows()
save(experiment_step, file = "experiment_step.rda")
# Partially linear model, no variable selection.
models_number <- 10
experiment <- lapply(1:replications, function(x) {
    non_zero_parameters <- lapply(1:models_number, function(x) sample(params_number, 4))
    parameters <- lapply(1:models_number, function(x) {
        y <- rep(0, params_number)
        y[non_zero_parameters[[x]]] <- 4
        y
    })
    splitted <- design_matrix %>%
        as_tibble() %>%
        split(1:nrow(.) %% models_number)
    full_df <- lapply(1:models_number, function(x) {
        tmp <- splitted[[x]]
        tmp$y <- (as.matrix(tmp))%*%(as.matrix(parameters[[x]])) + rnorm(nrow(tmp))
        tmp
    }) %>%
        bind_rows()
    similar <- simulateSimilar(full_df, full_df[5, ], "y", black_box, size = 50)
    regr_obj <- getLearnerModel(trainWhiteBox(similar, "regr.lm"))
    summ <- summary(regr_obj)
    p_vals <- summ$coefficients[-1, 4]
    which_params <- which(p_vals < 0.05)
    tibble(r_squared = summ$r.squared,
           FalseDiscoveryN = length(setdiff(which_params, non_zero_parameters[[1]])),
           TrueDiscoveryN = sum(non_zero_parameters[[1]] %in% which_params),
           estimError = sum((parameters[[1]] - coef(regr_obj)[-1])^2))
}) %>%
    bind_rows()
# save(experiment, file = "experiment.rda")
# Partially linear model, stepwise variable selection.
experiment_step2 <- lapply(1:replications, function(x) {
    non_zero_parameters <- lapply(1:models_number, function(x) sample(params_number, 4))
    parameters <- lapply(1:models_number, function(x) {
        y <- rep(0, params_number)
        y[non_zero_parameters[[x]]] <- 4
        y
    })
    splitted <- design_matrix %>%
        as_tibble() %>%
        split(1:nrow(.) %% models_number)
    full_df <- lapply(1:models_number, function(x) {
        tmp <- splitted[[x]]
        tmp$y <- (as.matrix(tmp))%*%(as.matrix(parameters[[x]])) + rnorm(nrow(tmp))
        tmp
    }) %>%
        bind_rows()
    similar <- simulateSimilar(full_df, full_df[5, ], "y", black_box, size = 50)
    which_params <- selectModel(as.data.frame(similar$data[, -21]), as.data.frame(similar$data$y), crit = aic)
    which_params <- as.numeric(gsub("V", "", which_params))
    tibble(FalseDiscoveryN = length(setdiff(which_params, non_zero_parameters[[1]])),
           TrueDiscoveryN = sum(non_zero_parameters[[1]] %in% which_params))
}) %>%
    bind_rows()
# save(experiment_step2, file = "experiment_step2.rda")
