
set.seed(100)
licznosci <- floor(1000*runif(5, 0, 1))
liczba_obserwacji <- sum(licznosci)
dzielnice <- c("Piękna", "Modna", "Spokojna", "Stara", "Nowa")
length(sample(dzielnice, liczba_obserwacji, T, licznosci/sum(licznosci)))
pietra <- (1:10)[sample(10, liczba_obserwacji, T)]
m2 <- runif(liczba_obserwacji, min = 25, max = 200)
pokoje <- round(m2/30) + (0:1)[sample(2, liczba_obserwacji,  T)]
rok_budowy <- floor(runif(liczba_obserwacji, 1920, max = 2015))
df <- data.frame(dzielnica = sample(dzielnice, liczba_obserwacji, T, licznosci/sum(licznosci)),
                 pietra = pietra,
                 wielkosc = m2,
                 n_pokoj = pokoje,
                 rok_budowy = rok_budowy)
ile_starych = sum(df$dzielnica == "Stara")
df <- mutate(df, rok_budowy = ifelse(dzielnica == "Stara" & runif(liczba_obserwacji) >  0.5,
                                      rok_budowy - 200,
                                      rok_budowy))
cena_m22 <- 6000 +
  600 * (abs(df$rok_budowy - 2000) < 20) +
  -10 * df$wielkosc +
  -100 * df$pietra +
  -50 * df$n_pokoj
cena_m23 <- 6000 +
  12 * (2018 - df$rok_budowy) +
  -10 * df$wielkosc +
  -100 * df$pietra +
  -50 * df$n_pokoj
df2 <- mutate(df, cena_m2 = cena_m22) %>%
  mutate(cena_m2 = ifelse(dzielnica == "Stara", cena_m23, cena_m2))
hist(df2$cena_m2)
plot(df2$rok_budowy, df2$cena_m2)

zbior_uczacy <- sample_frac(df2, size = 0.7)
zbior_testowy <- setdiff(df2, zbior_uczacy)

mlm <- lm(cena_m2 ~., data = zbior_uczacy)
mrf <- randomForest(cena_m2 ~., data = zbior_uczacy)
msv <- e1071::svm(cena_m2 ~., data = zbior_uczacy)

mean((zbior_testowy$cena_m2 - predict(mlm, zbior_testowy))^2)
mean((zbior_testowy$cena_m2 - predict(mrf, zbior_testowy))^2)
mean((zbior_testowy$cena_m2 - predict(msv, zbior_testowy))^2)

# hist((zbior_testowy$cena_m2 - predict(mrf, zbior_testowy))^2)
# max((zbior_testowy$cena_m2 - predict(mrf, zbior_testowy))^2)
# hist((zbior_testowy$cena_m2 - predict(mlm, zbior_testowy))^2)

plot(zbior_testowy$cena_m2, predict(mrf, zbior_testowy))
plot(zbior_testowy$cena_m2, predict(mlm, zbior_testowy))
plot(zbior_testowy$cena_m2, predict(msv, zbior_testowy))

nobs <- which.max((predict(mrf, zbior_testowy) - zbior_testowy$cena_m2)^2)
nobs2 <- which.max((predict(msv, zbior_testowy) - zbior_testowy$cena_m2)^2)
# Oba mają problem z tą samą obserwacją

# Jak zależy cena od roku budowy?
lm_expl <- DALEX::explain(mlm, data = zbior_testowy, y = "cena_m2")
rf_expl <- DALEX::explain(mrf, data = zbior_testowy, y = "cena_m2")
svm_expl <- DALEX::explain(msv, data = zbior_testowy, y = "cena_m2")
#
# jedna_zmienna <- DALEX::single_variable(lm_expl, "rok_budowy")
# jedna_zmienna_rf <- DALEX::single_variable(rf_expl, "rok_budowy")
# plot(jedna_zmienna, jedna_zmienna_rf)
#
predict(mrf, zbior_testowy[nobs, ])
predict(msv, zbior_testowy[nobs, ])
zbior_testowy[nobs, "cena_m2"]
max_res <- DALEX::single_prediction(rf_expl, zbior_testowy[nobs, ])
others <- head(which(zbior_testowy$dzielnica == "Stara"))
other_res <- DALEX::single_prediction(rf_expl, zbior_testowy[others[3], ])
max_res_svm <- DALEX::single_prediction(svm_expl, zbior_testowy[nobs, ])
plot(other_res)
plot(max_res)
plot(max_res_svm)
plot(max_res, max_res_svm)

max_res$variable_name
max_res$contribution/max_res_svm$contribution
max_res_svm$contribution/max_res$contribution
