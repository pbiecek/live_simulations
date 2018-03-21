### Libraries. ----
library(mlr)
library(live)
library(tidyverse)
library(MLExpRessoData)
library(knitr)
### Code from TCGA vignette. ----
set.seed(1)
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
nData <- BRCA[,c(2,2 + which(pvals < 0.001))]
nind1 <- sample(which(nData$survival_status == 1), 100, replace = TRUE)
nind2 <- sample(which(nData$survival_status == 0), 100, replace = TRUE)
nData <- nData[c(nind1, nind2),]
# Create large neighbourhood
similar <- sample_locally(data = nData,
                          explained_instance = nData[1, ],
                          explained_var = "survival_status",
                          size = 10000)
similar <- similar$data
# How many unique rows?
dim(unique(similar)) # 4869
# Counts
# similar_counts <- similar %>%
#   summarise_all(n_distinct) %>%
#   gather(name, n_dist) %>%
#   arrange(desc(n_dist))
#
# ndata_counts <- nData %>%
#   select(-survival_status) %>%
#   summarise_all(n_distinct) %>%
#   gather(name, n_dist_ndata) %>%
#   arrange(desc(n_dist_ndata))
#
# inner_join(similar_counts, ndata_counts, by = "name")
# Interesting variable: HTATSF1
hist(similar$HTATSF1)
p <- ncol(nData)
j <- which(colnames(similar) == "HTATSF1")
n_j <- n_distinct(nData[, "HTATSF1"])
# Compare empirical probabilities with theoretical
emp_props <- similar %>%
  select(HTATSF1) %>%
  group_by(HTATSF1) %>%
  summarise(how_many_emp = n()) %>%
  arrange(desc(how_many_emp)) %>%
  mutate(emp_prop = how_many_emp/10000)

comp_prop <- nData %>%
  select(HTATSF1) %>%
  group_by(HTATSF1) %>%
  summarise(how_many = n()) %>%
  arrange(desc(how_many)) %>%
  mutate(count_all = nrow(nData),
         theor_prop = ifelse(HTATSF1 == nData[1, "HTATSF1"],
                             (how_many/count_all + p - 1)/p,
                             (how_many/count_all)/p)) %>%
  arrange(desc(theor_prop)) %>%
  left_join(emp_props, by = "HTATSF1")
kable(comp_prop, digits = 5)
# Do probabilities sum to 1?
sum(comp_prop$theor_prop)
sum(comp_prop$emp_prop, na.rm = T)
# Distance from similar observations to explained instance.
no_surv <- select(nData, -survival_status)
# bind_rows(no_surv[1, ],
#           similar[1, ])
distances <- apply(as.matrix(similar), 1, function(x) sum((no_surv[1, ] - x)^2))
mean(distances)
hist(distances)

library(tidyverse)
ggplot(tibble(x = distances[distances < 950]), aes(x)) + geom_histogram() + theme_bw()
max(distances)
summary(distances)

count_id <- unlist(lapply(1:nrow(similar),
                          function(x)
                            !any(no_surv[1, ] != similar[x, ])), use.names = F)

njm <- unlist(lapply(1:ncol(no_surv), function(x)
  sum(no_surv[, x] == no_surv[1, x])), use.names = F)

cos <- sum(njm)/(nrow(no_surv)*ncol(no_surv)^2) + (ncol(no_surv) - 1)/ncol(no_surv)

sum((njm/nrow(no_surv))/ncol(no_surv)) # = sum pjm/p
sum(count_id)/10000 # proportion of rows identical to explained instance
