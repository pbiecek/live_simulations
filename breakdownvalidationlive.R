library(DALEX)
library(live)

data("apartments")

obs <- apartments[1, ]

rfw <- randomForest::randomForest(m2.price~., data = apartments)

new_obs <- unique(live::sample_locally2(apartments, apartments[1, ], "m2.price", 50)$data)

rf_expl <- explain(rfw, data = apartments, y = apartments$m2.price)

prediction_breakdown(rf_expl, obs[, -1]) -> sp1

prediction_breakdown(rf_expl, new_obs[1, ]) -> sp2

prediction_breakdown(rf_expl, new_obs[2, ]) -> sp3

prediction_breakdown(rf_expl, new_obs[5, ]) -> sp4

sp1$variable
sp2$variable

plot(sp1)
plot(sp2)
plot(sp3)
plot(sp4)
head(new_obs)


