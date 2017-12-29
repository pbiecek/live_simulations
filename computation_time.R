### Libraries ----
library(live)
library(tidyverse)
library(mlr)
library(microbenchmark)
library(data.table)
library(MLExpRessoData)
### Data (code from TCGA vignette) ----
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
nind1 <- sample(which(nData$survival_status == 1), 100, replace = TRUE)
nind2 <- sample(which(nData$survival_status == 0), 100, replace = TRUE)
nData <- nData[c(nind1, nind2),]
### Benchmark ----
# Create neighbourhood using different tools.
live_neighbourhood <- function(dataset_size = 2000) {
  sample_locally(data = nData,
                 explained_instance = nData[1,],
                 explained_var = "survival_status",
                 size = dataset_size)
}
pick_and_replace <- function(instance, variables_count, original_data) {
  var_to_pick <- sample(1:variables_count, 1)
  instance[var_to_pick] <- sample(unlist(original_data[, var_to_pick], use.names = F), 1)
  instance
}
datatable_neighbourhood <- function(dataset_size = 2000) {
  namesOfCols <- colnames(nData)
  tmp <- as.data.table(bind_rows(lapply(1:dataset_size, function(x) nData[1, ]))) %>%
    data.table::transpose() %>%
    dplyr::mutate_all(function(x) pick_and_replace(x, nrow(.), nData)) %>%
    transpose()
  colnames(tmp) <- namesOfCols
  tmp
}
datatable_neighbourhood2 <- function(dataset_size = 2000) {
  namesOfCols <- colnames(nData)
  tmp <- as.data.table(bind_rows(lapply(1:dataset_size, function(x) nData[1, ])))
  tmp <- data.table::transpose(tmp)
  tmp[, (names(tmp)) := lapply(.SD, function(x) pick_and_replace(x, nrow(tmp), nData))]
  tmp <- data.table::transpose(tmp)
  colnames(tmp) <- namesOfCols
  tmp
}
datatable_neighbourhood3 <- function(dataset_size = 2000) {
  namesOfCols <- colnames(nData)
  tmp <- as.data.table(rbindlist(lapply(1:dataset_size, function(x) nData[1, ])))
  tmp <- data.table::transpose(tmp)
  tmp[, (names(tmp)) := lapply(.SD, function(x) pick_and_replace(x, nrow(tmp), nData))]
  tmp <- data.table::transpose(tmp)
  colnames(tmp) <- namesOfCols
  tmp
}
simple_solution <- function(dataset_size = 2000) {
  rbindlist(lapply(1:dataset_size, function(x) {
    x <- as.data.table(pick_and_replace(nData[1, ], ncol(nData), nData))
    colnames(x) <- colnames(nData)
    x
  }))
}
apply_neighbourhood(dataset_size = 2000) {
  tmp <- apply(as.matrix(bind_rows(lapply(1:dataset_size, function(x) nData[1, ]))), 1,
        function(x) pick_and_replace(x, ncol(nData), nData))
  colnames(tmp) <- colnames(nData)
  tmp
}
datatable_neighbourhood4 <- function(dataset_size = 2000) {
  tmp <- as.data.table(rbindlist(lapply(1:dataset_size, function(x) nData[1, ])))
  for(k in 1:nrow(tmp)) {
    picked_var <- sample(1:ncol(tmp), 1)
    set(tmp, i = k, j = picked_var,
        sample(nData[, picked_var], 1))
  }
  tmp
}
# Compare performance
# microbenchmark(live_neighbourhood)
# microbenchmark(live_neighbourhood(), datatable_neighbourhood())
# microbenchmark(live_neighbourhood(100), simple_solution(100))
# microbenchmark(live_neighbourhood(100), datatable_neighbourhood2(100))
# microbenchmark(live_neighbourhood(100), datatable_neighbourhood3(100))
# microbenchmark(live_neighbourhood(), datatable_neighbourhood3())
# microbenchmark(simple_solution(100), datatable_neighbourhood3(100))
# microbenchmark(apply_neigbourhood(100), datatable_neighbourhood3(100))
# microbenchmark(live_neighbourhood(100), datatable_neighbourhood4(100))
# microbenchmark(datatable_neighbourhood3(100), datatable_neighbourhood4(100))
compare_time <- microbenchmark(live_neighbourhood(100), simple_solution(100), datatable_neighbourhood3(100),
               datatable_neighbourhood4(100))
compare_time
compare_time_big <- microbenchmark(live_neighbourhood(), simple_solution(),
                                   datatable_neighbourhood3(),
                                   datatable_neighbourhood4())
compare_time_big
