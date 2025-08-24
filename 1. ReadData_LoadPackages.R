## Loading necessary R packages

# To deal and analyse with demographic data
lapply(c("demography", "StMoMo", "HMDHFDplus"), require, character.only = TRUE)

# To manipulate data
lapply(c("dplyr"), require, character.only = TRUE)

# To perform machine learning techniques and calculate errors
lapply(c("mlr", "MLmetrics", "stringr"), require, character.only = TRUE)

# To show and save error values in a table format
lapply("data.table", require, character.only = TRUE)

# To make several graphs
lapply(c("ggplot2", "patchwork"), require, character.only = TRUE)

# To prevent scientific numbers notation
options(scipen = 999)

# Reading the Data
DNK <- hmd.mx(country = "DNK", username = "ozerbakar@hacettepe.edu.tr", 
              password = "Ozer19871905*", label = "DENMARK")

# Transforming demography data into "StMoMo" data.
DNK.Fem <- StMoMoData(DNK, series = "female", type = "central")
DNK.Male <- StMoMoData(DNK, series = "male", type = "central")

DNK.Fem$Dxt <- round(DNK.Fem$Dxt)
DNK.Male$Dxt <- round(DNK.Male$Dxt)

DNK.Fem.Bin <- StMoMoData(DNK, series = "female", type = "initial")
DNK.Male.Bin <- StMoMoData(DNK, series = "male", type = "initial")

DNK.Fem.Bin$Dxt <- round(DNK.Fem.Bin$Dxt)
DNK.Male.Bin$Dxt <- round(DNK.Male.Bin$Dxt)

# The problem with this package is when we need to extract the number of deaths,
# it gives the result of the multiplication of the mortality rates and the exposures.
# These numbers are slightly different with the observed number of deaths. We can read
# the exact number of deaths with the package HMDHFDplus.

N.of.Deaths <- readHMDweb("DNK", "Deaths_1x1", "ozerbakar@hacettepe.edu.tr", 
                          "Ozer19871905*", fixup = TRUE)

# Creating Stochastic Mortality Models 
LC <- lc(link = "log")
CBD <- cbd(link = "logit")
RH <- rh(link = "log", cohortAgeFun = "1")
APC <- apc(link = "log")
M7 <- m7(link = "logit")

# Pre-defined Plat function is not included in "StMoMo" package
f.Plat <- function(x, ages) mean(ages) - x
constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages){
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  phiReg <- lm(gc ~ 1 + c + I(c ^ 2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c ^ 2
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t ^ 2 - 2 * xbar * t)
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x ^ 2
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x)
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}

Plat <- StMoMo(link = "log", staticAgeFun = TRUE,
               periodAgeFun = c("1", f.Plat), 
               cohortAgeFun = "1", 
               constFun = constPlat)
