set.seed(123456)

# Determining fitting ages and years 
years.fit <- 1960:2010
ages.fit <- 65:99

# Determining hold-out testing years
for_years.fit <- 2011:2020

ages.range <- (max(ages.fit)-min(ages.fit)+1)
years.range <- (max(years.fit)-min(years.fit)+1)

for_ages.range <- (max(ages.fit)-min(ages.fit)+1)
for_years.range <- (max(for_years.fit)-min(for_years.fit)+1)

## Fitting stochastic mortality models to the data

# Female
LC.fit.fem <- fit(LC, data = DNK.Fem, ages.fit = ages.fit, years.fit = years.fit)
CBD.fit.fem <- fit(CBD, data = DNK.Fem.Bin, ages.fit = ages.fit, years.fit = years.fit)
# RH has convergence issues (see Currie 2016)
RH.fit.fem <- fit(RH, data = DNK.Fem, ages.fit = ages.fit, years.fit = years.fit,
                  start.ax = LC.fit.fem$ax, start.bx = LC.fit.fem$bx, start.kt = LC.fit.fem$kt)
APC.fit.fem <- fit(APC, data = DNK.Fem, ages.fit = ages.fit, years.fit = years.fit)
M7.fit.fem <- fit(M7, data = DNK.Fem.Bin, ages.fit = ages.fit, years.fit = years.fit)
Plat.fit.fem <- fit(Plat, data = DNK.Fem, ages.fit = ages.fit, years.fit = years.fit)

# Male
LC.fit.male <- fit(LC, data = DNK.Male, ages.fit = ages.fit, years.fit = years.fit)
CBD.fit.male <- fit(CBD, data = DNK.Male.Bin, ages.fit = ages.fit, years.fit = years.fit)
# RH has convergence issues (see Currie 2016)
RH.fit.male <- fit(RH, data = DNK.Male, ages.fit = ages.fit, years.fit = years.fit,
                   start.ax = LC.fit.male$ax, start.bx = LC.fit.male$bx, start.kt = LC.fit.male$kt)
APC.fit.male <- fit(APC, data = DNK.Male, ages.fit = ages.fit, years.fit = years.fit)
M7.fit.male <- fit(M7, data = DNK.Male.Bin, ages.fit = ages.fit, years.fit = years.fit)
Plat.fit.male <- fit(Plat, data = DNK.Male, ages.fit = ages.fit, years.fit = years.fit)

# Evaluating the performance of the models

# Female
AIC(LC.fit.fem, CBD.fit.fem, RH.fit.fem, APC.fit.fem, M7.fit.fem, Plat.fit.fem)
BIC(LC.fit.fem, CBD.fit.fem, RH.fit.fem, APC.fit.fem, M7.fit.fem, Plat.fit.fem)

# Male
AIC(LC.fit.male, CBD.fit.male, RH.fit.male, APC.fit.male, M7.fit.male, Plat.fit.male)
BIC(LC.fit.male, CBD.fit.male, RH.fit.male, APC.fit.male, M7.fit.male, Plat.fit.male)

## Extracting fitted mortality rates from the models
# Female
m.LC.fem <- fitted(LC.fit.fem, type = "rates")
m.CBD.fem <- -log(1-fitted(CBD.fit.fem, type = "rates"))
m.RH.fem <- fitted(RH.fit.fem, type = "rates")
m.APC.fem <- fitted(APC.fit.fem, type = "rates")
m.M7.fem <- -log(1-fitted(M7.fit.fem, type = "rates"))
m.Plat.fem <- fitted(Plat.fit.fem, type = "rates")

# Male
m.LC.male <- fitted(LC.fit.male, type = "rates")
m.CBD.male <- -log(1-fitted(CBD.fit.male, type = "rates"))
m.RH.male <- fitted(RH.fit.male, type = "rates")
m.APC.male <- fitted(APC.fit.male, type = "rates")
m.M7.male <- -log(1-fitted(M7.fit.male, type = "rates"))
m.Plat.male <- fitted(Plat.fit.male, type = "rates")

## Calculating observed and fitted number of deaths

# Observed number of deaths
N.of.Deaths <- N.of.Deaths %>%
  filter(Year >= min(years.fit) & Year <= max(years.fit)) %>%
  filter(Age >= min(ages.fit) & Age <= max(ages.fit)) %>%
  select(-OpenInterval,-Total)

D.fem <- N.of.Deaths$Female
D.male <- N.of.Deaths$Male

#Fitted number of deaths
LC.D.fem <- m.LC.fem*DNK.Fem$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
CBD.D.fem <- m.CBD.fem*DNK.Fem$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
RH.D.fem <- m.RH.fem*DNK.Fem$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
APC.D.fem <- m.APC.fem*DNK.Fem$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
M7.D.fem <- m.M7.fem*DNK.Fem$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
Plat.D.fem <- m.Plat.fem*DNK.Fem$Ext[ages.fit+1,years.fit-min(DNK$year)+1]

LC.D.male <- m.LC.male*DNK.Male$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
CBD.D.male <- m.CBD.male*DNK.Male$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
RH.D.male <- m.RH.male*DNK.Male$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
APC.D.male <- m.APC.male*DNK.Male$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
M7.D.male <- m.M7.male*DNK.Male$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
Plat.D.male <- m.Plat.male*DNK.Male$Ext[ages.fit+1,years.fit-min(DNK$year)+1]

## Forecasting with mortality models

for.m.LC_fem <- forecast(LC.fit.fem, h = for_years.range, 
                         level = 95, kt.method = "iarima")
for.m.LC.fem <- for.m.LC_fem$rates

for.m.LC_male <- forecast(LC.fit.male, h = for_years.range,
                          level = 95, kt.method = "iarima")
for.m.LC.male <- for.m.LC_male$rates

for.m.CBD_fem <- forecast(CBD.fit.fem, h = for_years.range, 
                          level = 95, kt.method = "iarima")
for.m.CBD.fem <- for.m.CBD_fem$rates
for.m.CBD.fem <- -log(1-for.m.CBD.fem)

for.m.CBD_male <- forecast(CBD.fit.male, h = for_years.range, 
                           level = 95, kt.method = "iarima")
for.m.CBD.male <- for.m.CBD_male$rates
for.m.CBD.male <- -log(1-for.m.CBD.male)

for.m.RH_fem <- forecast(RH.fit.fem, h = for_years.range, 
                         level = 95, kt.method = "iarima")
for.m.RH.fem <- for.m.RH_fem$rates

for.m.RH_male <- forecast(RH.fit.male, h = for_years.range, 
                          level = 95, kt.method = "iarima")
for.m.RH.male <- for.m.RH_male$rates

for.m.APC_fem <- forecast(APC.fit.fem, h = for_years.range, 
                          level = 95, kt.method = "iarima")
for.m.APC.fem <- for.m.APC_fem$rates

for.m.APC_male <- forecast(APC.fit.male, h = for_years.range, 
                           level = 95, kt.method = "iarima")
for.m.APC.male <- for.m.APC_male$rates

for.m.M7_fem <- forecast(M7.fit.fem, h = for_years.range, 
                         level = 95, kt.method = "iarima")
for.m.M7.fem <- for.m.M7_fem$rates
for.m.M7.fem <- -log(1-for.m.M7.fem)

for.m.M7_male <- forecast(M7.fit.male, h = for_years.range, 
                          level = 95, kt.method = "iarima")
for.m.M7.male <- for.m.M7_male$rates
for.m.M7.male <- -log(1-for.m.M7.male)

for.m.Plat_fem <- forecast(Plat.fit.fem, h = for_years.range, 
                           level = 95, kt.method = "iarima")
for.m.Plat.fem <- for.m.Plat_fem$rates

for.m.Plat_male <- forecast(Plat.fit.male, h = for_years.range, 
                            level = 95, kt.method = "iarima")
for.m.Plat.male <- for.m.Plat_male$rates

## Calculating errors between observed and mortality models' mortality rates

# Training Period
m.obs.train.fem <- DNK.Fem$Dxt[ages.fit+1,years.fit-min(DNK$year)+1]/DNK.Fem$Ext[ages.fit+1,years.fit-min(DNK$year)+1]
m.obs.train.male <- DNK.Male$Dxt[ages.fit+1,years.fit-min(DNK$year)+1]/DNK.Male$Ext[ages.fit+1,years.fit-min(DNK$year)+1]

rmse.fit.LC.fem <- RMSE(m.LC.fem, m.obs.train.fem)
rmse.fit.CBD.fem <- RMSE(m.CBD.fem, m.obs.train.fem)
rmse.fit.RH.fem <- RMSE(m.RH.fem, m.obs.train.fem)
rmse.fit.APC.fem <- RMSE(m.APC.fem, m.obs.train.fem)
rmse.fit.M7.fem <- RMSE(m.M7.fem, m.obs.train.fem)
rmse.fit.Plat.fem <- RMSE(m.Plat.fem, m.obs.train.fem)

rmse.fit.LC.male <- RMSE(m.LC.male, m.obs.train.male)
rmse.fit.CBD.male <- RMSE(m.CBD.male, m.obs.train.male)
rmse.fit.RH.male <- RMSE(m.RH.male, m.obs.train.male)
rmse.fit.APC.male <- RMSE(m.APC.male, m.obs.train.male)
rmse.fit.M7.male <- RMSE(m.M7.male, m.obs.train.male)
rmse.fit.Plat.male <- RMSE(m.Plat.male, m.obs.train.male)

# Testing Period
m.obs.test.fem <- DNK.Fem$Dxt[ages.fit+1,for_years.fit-min(DNK$year)+1]/DNK.Fem$Ext[ages.fit+1,for_years.fit-min(DNK$year)+1]
m.obs.test.male <- DNK.Male$Dxt[ages.fit+1,for_years.fit-min(DNK$year)+1]/DNK.Male$Ext[ages.fit+1,for_years.fit-min(DNK$year)+1]

rmse.for.LC.fem <- RMSE(for.m.LC.fem, m.obs.test.fem)
rmse.for.CBD.fem <- RMSE(for.m.CBD.fem, m.obs.test.fem)
rmse.for.RH.fem <- RMSE(for.m.RH.fem, m.obs.test.fem)
rmse.for.APC.fem <- RMSE(for.m.APC.fem, m.obs.test.fem)
rmse.for.M7.fem <- RMSE(for.m.M7.fem, m.obs.test.fem)
rmse.for.Plat.fem <- RMSE(for.m.Plat.fem, m.obs.test.fem)

rmse.for.LC.male <- RMSE(for.m.LC.male, m.obs.test.male)
rmse.for.CBD.male <- RMSE(for.m.CBD.male, m.obs.test.male)
rmse.for.RH.male <- RMSE(for.m.RH.male, m.obs.test.male)
rmse.for.APC.male <- RMSE(for.m.APC.male, m.obs.test.male)
rmse.for.M7.male <- RMSE(for.m.M7.male, m.obs.test.male)
rmse.for.Plat.male <- RMSE(for.m.Plat.male, m.obs.test.male)

