### Libraries ----
library(MASS)
library(mlr)
library(live)
library(dplyr)
### Simulations: local fidelity ----
simulation_1 <- lapply(1:1000, function(x) {
  data_matrix <- mvrnorm(n = 1000, rep(0, 30), diag(1, 30))
  data_matrix <- data_matrix %>%
    as.data.frame() %>%
    mutate(y = 4*(V1 + V2 + V3 + V4 + V5))
  svm_model <- e1071::svm(y ~ ., data = data_matrix)
  local_dataset <- sample_locally(data_matrix, data_matrix[1, ], "y", 100)
  local_dataset <- add_predictions(data_matrix, local_dataset, svm_model)
  fitted_model <- local_dataset$data$y
  explained <- fit_explanation(local_dataset, "regr.lm")
  fitted_explanation <- predict(getLearnerModel(explained))
  data.frame(mse = mean((fitted_model - fitted_explanation)^2),
             mae = mean(abs(fitted_model - fitted_explanation)),
             one_difference = predict(getLearnerModel(explained),
                                      newdata = data_matrix[1, ]) - predict(svm_model,
                                                                            newdata = data_matrix[1, ]))
})
result_1 <- bind_rows(simulation_1) %>%
  summarise_all(mean)
simulation_2 <- lapply(1:1000, function(x) {
  data_matrix <- mvrnorm(n = 1000, rep(0, 30), diag(1, 30))
  data_matrix <- data_matrix %>%
    as.data.frame() %>%
    mutate(y = 4*(V1 + V2 + V3 + V4 + V5))
  svm_model <- e1071::svm(y ~ ., data = data_matrix)
  local_dataset <- sample_locally(data_matrix, data_matrix[1, ], "y", 500)
  local_dataset <- add_predictions(data_matrix, local_dataset, svm_model)
  fitted_model <- local_dataset$data$y
  explained <- fit_explanation(local_dataset, "regr.lm")
  fitted_explanation <- predict(getLearnerModel(explained))
  data.frame(mse = mean((fitted_model - fitted_explanation)^2),
             mae = mean(abs(fitted_model - fitted_explanation)),
             one_difference = predict(getLearnerModel(explained),
                                      newdata = data_matrix[1, ]) - predict(svm_model,
                                                                            newdata = data_matrix[1, ]))
})
result_2 <- bind_rows(simulation_2) %>%
  summarise_all(mean)
simulation_3 <- lapply(1:1000, function(x) {
  data_matrix <- mvrnorm(n = 1000, rep(0, 30), diag(1, 30))
  data_matrix <- data_matrix %>%
    as.data.frame() %>%
    mutate(y = 4*(V1 + V2 + V3 + V4 + V5))
  svm_model <- e1071::svm(y ~ ., data = data_matrix)
  local_dataset <- sample_locally(data_matrix, data_matrix[1, ], "y", 1000)
  local_dataset <- add_predictions(data_matrix, local_dataset, svm_model)
  fitted_model <- local_dataset$data$y
  explained <- fit_explanation(local_dataset, "regr.lm")
  fitted_explanation <- predict(getLearnerModel(explained))
  data.frame(mse = mean((fitted_model - fitted_explanation)^2),
             mae = mean(abs(fitted_model - fitted_explanation)),
             one_difference = predict(getLearnerModel(explained),
                                      newdata = data_matrix[1, ]) - predict(svm_model,
                                                                            newdata = data_matrix[1, ]))
})
result_3 <- bind_rows(simulation_3) %>%
  summarise_all(mean)

simulation_4 <- lapply(1:1000, function(x) {
  data_matrix <- mvrnorm(n = 1000, rep(0, 30), diag(1, 30))
  data_matrix <- data_matrix %>%
    as.data.frame() %>%
    mutate(y = 4*(V1 + V2 + V3 + V4 + V5))
  svm_model <- e1071::svm(y ~ ., data = data_matrix)
  local_dataset <- sample_locally(data_matrix, data_matrix[1, ], "y", 50)
  local_dataset <- add_predictions(data_matrix, local_dataset, svm_model)
  fitted_model <- local_dataset$data$y
  explained <- fit_explanation(local_dataset, "regr.lm")
  fitted_explanation <- predict(getLearnerModel(explained))
  data.frame(mse = mean((fitted_model - fitted_explanation)^2),
             mae = mean(abs(fitted_model - fitted_explanation)),
             one_difference = predict(getLearnerModel(explained),
                                      newdata = data_matrix[1, ]) - predict(svm_model,
                                                                            newdata = data_matrix[1, ]))
})
result_4 <- bind_rows(simulation_4) %>%
  summarise_all(mean)

# result <- list(n50 = result_4,
#                n100 = result_1,
#                n500 = result_2,
#                n1000 = result_3)
# save(result, file = "result.rda")
