
# RMSE of Mortality Models
Mort.Models <- c("LC","CBD","APC","RH","M7","Plat")

# Testing Period
RMSE.test_MM_OBS.fem <- c(rmse.for.LC.fem, rmse.for.CBD.fem, rmse.for.APC.fem, rmse.for.RH.fem, rmse.for.M7.fem, rmse.for.Plat.fem)
RMSE.test_MM_OBS.male <- c(rmse.for.LC.male,rmse.for.CBD.male,rmse.for.APC.male,rmse.for.RH.male, rmse.for.M7.male,rmse.for.Plat.male)

RMSE.test_MM_OBS.table <- data.table(Mort.Models, RMSE.test_MM_OBS.fem, RMSE.test_MM_OBS.male)
print(RMSE.test_MM_OBS.table)

# Training Period
RMSE.train_MM_OBS.fem <- c(rmse.fit.LC.fem, rmse.fit.CBD.fem, rmse.fit.APC.fem, rmse.fit.RH.fem, rmse.fit.M7.fem, rmse.fit.Plat.fem)
RMSE.train_MM_OBS.male <- c(rmse.fit.LC.male,rmse.fit.CBD.male,rmse.fit.APC.male,rmse.fit.RH.male,rmse.fit.M7.male,rmse.fit.Plat.male)

RMSE.train_MM_OBS.table <- data.table(Mort.Models, RMSE.train_MM_OBS.fem, RMSE.train_MM_OBS.male)
print(RMSE.train_MM_OBS.table)

fwrite(RMSE.train_MM_OBS.table, "RMSE Train Mortality Models.csv")
fwrite(RMSE.test_MM_OBS.table, "RMSE Test Mortality Models.csv")

# RMSE of ML Integrated Models 

# Testing Period
Mort.Models <- c("LC","CBD","APC","RH","M7","Plat")

min.test.rmse.DT.fem <- c(min.test.DT_LC.fem, min.test.DT_CBD.fem, min.test.DT_APC.fem, min.test.DT_RH.fem, min.test.DT_M7.fem, min.test.DT_Plat.fem)
min.test.rmse.DT.male <- c(min.test.DT_LC.male, min.test.DT_CBD.male, min.test.DT_APC.male, min.test.DT_RH.male, min.test.DT_M7.male, min.test.DT_Plat.male)

min.test.rmse.RF.fem <- c(min.test.RF_LC.fem, min.test.RF_CBD.fem, min.test.RF_APC.fem, min.test.RF_RH.fem, min.test.RF_M7.fem, min.test.RF_Plat.fem)
min.test.rmse.RF.male <- c(min.test.RF_LC.male, min.test.RF_CBD.male, min.test.RF_APC.male, min.test.RF_RH.male, min.test.RF_M7.male, min.test.RF_Plat.male)

min.test.rmse.GB.fem <- c(min.test.GB_LC.fem, min.test.GB_CBD.fem, min.test.GB_APC.fem, min.test.GB_RH.fem, min.test.GB_M7.fem, min.test.GB_Plat.fem)
min.test.rmse.GB.male <- c(min.test.GB_LC.male, min.test.GB_CBD.male, min.test.GB_APC.male, min.test.GB_RH.male, min.test.GB_M7.male, min.test.GB_Plat.male)

min.test.rmse.XGB.fem <- c(min.test.XGB_LC.fem, min.test.XGB_CBD.fem, min.test.XGB_APC.fem, min.test.XGB_RH.fem, min.test.XGB_M7.fem, min.test.XGB_Plat.fem)
min.test.rmse.XGB.male <- c(min.test.XGB_LC.male, min.test.XGB_CBD.male, min.test.XGB_APC.male, min.test.XGB_RH.male, min.test.XGB_M7.male, min.test.XGB_Plat.male)

min.test.rmse.table <- data.table(Mort.Models, min.test.rmse.DT.fem, min.test.rmse.DT.male,
                                  min.test.rmse.RF.fem, min.test.rmse.RF.male,
                                  min.test.rmse.GB.fem, min.test.rmse.GB.male,
                                  min.test.rmse.XGB.fem, min.test.rmse.XGB.male)
print(min.test.rmse.table)

# Training Period
Mort.Models <- c("LC","CBD","APC","RH","M7","Plat")

min.train.rmse.DT.fem <- c(min.train.DT_LC.fem, min.train.DT_CBD.fem, min.train.DT_APC.fem, min.train.DT_RH.fem, min.train.DT_M7.fem, min.train.DT_Plat.fem)
min.train.rmse.DT.male <- c(min.train.DT_LC.male, min.train.DT_CBD.male, min.train.DT_APC.male, min.train.DT_RH.male, min.train.DT_M7.male, min.train.DT_Plat.male)

min.train.rmse.RF.fem <- c(min.train.RF_LC.fem, min.train.RF_CBD.fem, min.train.RF_APC.fem, min.train.RF_RH.fem, min.train.RF_M7.fem, min.train.RF_Plat.fem)
min.train.rmse.RF.male <- c(min.train.RF_LC.male, min.train.RF_CBD.male, min.train.RF_APC.male, min.train.RF_RH.male, min.train.RF_M7.male, min.train.RF_Plat.male)

min.train.rmse.GB.fem <- c(min.train.GB_LC.fem, min.train.GB_CBD.fem, min.train.GB_APC.fem, min.train.GB_RH.fem, min.train.GB_M7.fem, min.train.GB_Plat.fem)
min.train.rmse.GB.male <- c(min.train.GB_LC.male, min.train.GB_CBD.male, min.train.GB_APC.male, min.train.GB_RH.male, min.train.GB_M7.male, min.train.GB_Plat.male)

min.train.rmse.XGB.fem <- c(min.train.XGB_LC.fem, min.train.XGB_CBD.fem, min.train.XGB_APC.fem, min.train.XGB_RH.fem, min.train.XGB_M7.fem, min.train.XGB_Plat.fem)
min.train.rmse.XGB.male <- c(min.train.XGB_LC.male, min.train.XGB_CBD.male, min.train.XGB_APC.male, min.train.XGB_RH.male, min.train.XGB_M7.male, min.train.XGB_Plat.male)

min.train.rmse.table <- data.table(Mort.Models, min.train.rmse.DT.fem, min.train.rmse.DT.male,
                                   min.train.rmse.RF.fem, min.train.rmse.RF.male,
                                   min.train.rmse.GB.fem, min.train.rmse.GB.male,
                                   min.train.rmse.XGB.fem, min.train.rmse.XGB.male)
print(min.train.rmse.table)

# Table 2. Female Minimum Test and Corresponding Train
Mort.Models <- c("LC","CBD","APC","RH","M7","Plat")

min.test.train.rmse.DT.fem <- c(min.test.train.DT_LC.fem, min.test.train.DT_CBD.fem, min.test.train.DT_APC.fem, min.test.train.DT_RH.fem, min.test.train.DT_M7.fem, min.test.train.DT_Plat.fem)
min.test.train.rmse.DT.male <- c(min.test.train.DT_LC.male, min.test.train.DT_CBD.male, min.test.train.DT_APC.male, min.test.train.DT_RH.male, min.test.train.DT_M7.male, min.test.train.DT_Plat.male)

min.test.train.rmse.RF.fem <- c(min.test.train.RF_LC.fem, min.test.train.RF_CBD.fem, min.test.train.RF_APC.fem, min.test.train.RF_RH.fem, min.test.train.RF_M7.fem, min.test.train.RF_Plat.fem)
min.test.train.rmse.RF.male <- c(min.test.train.RF_LC.male, min.test.train.RF_CBD.male, min.test.train.RF_APC.male, min.test.train.RF_RH.male, min.test.train.RF_M7.male, min.test.train.RF_Plat.male)

min.test.train.rmse.GB.fem <- c(min.test.train.GB_LC.fem, min.test.train.GB_CBD.fem, min.test.train.GB_APC.fem, min.test.train.GB_RH.fem, min.test.train.GB_M7.fem, min.test.train.GB_Plat.fem)
min.test.train.rmse.GB.male <- c(min.test.train.GB_LC.male, min.test.train.GB_CBD.male, min.test.train.GB_APC.male, min.test.train.GB_RH.male, min.test.train.GB_M7.male, min.test.train.GB_Plat.male)

min.test.train.rmse.XGB.fem <- c(min.test.train.XGB_LC.fem, min.test.train.XGB_CBD.fem, min.test.train.XGB_APC.fem, min.test.train.XGB_RH.fem, min.test.train.XGB_M7.fem, min.test.train.XGB_Plat.fem)
min.test.train.rmse.XGB.male <- c(min.test.train.XGB_LC.male, min.test.train.XGB_CBD.male, min.test.train.XGB_APC.male, min.test.train.XGB_RH.male, min.test.train.XGB_M7.male, min.test.train.XGB_Plat.male)

min.test.train.rmse.table <- data.table(Mort.Models, min.test.train.rmse.DT.fem, min.test.train.rmse.DT.male,
                                  min.test.train.rmse.RF.fem, min.test.train.rmse.RF.male,
                                  min.test.train.rmse.GB.fem, min.test.train.rmse.GB.male,
                                  min.test.train.rmse.XGB.fem, min.test.train.rmse.XGB.male)
print(min.test.train.rmse.table)

fwrite(min.train.rmse.table, "RMSE Train.csv")
fwrite(min.test.rmse.table, "RMSE Test.csv")
fwrite(min.test.train.rmse.table, "RMSE Min.Test.Train.csv")
