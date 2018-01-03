library(mlr)
library(live)
library(tidyverse)
library(MLExpRessoData)
library(randomForest)
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

explained_instance_number <- 1
n_it <- 500
unique_count <- vector("integer", n_it)
for(i in 1:n_it) {
  similar <- sample_locally(data = nData,
                            explained_instance = nData[explained_instance_number,],
                            explained_var = "survival_status",
                            size = 200)
  tmp <- similar$data
  unique_count[i] <- sum(unlist(lapply(1:nrow(tmp),
                    function(x) any(tmp[x, ] != nData[explained_instance_number, -1]))))

}
save(unique_count, file = "unique_count.rda")
hist(unique_count)
