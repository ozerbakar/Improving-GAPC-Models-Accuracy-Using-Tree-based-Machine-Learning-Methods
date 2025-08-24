# Preparing the data for ML algorithms

# Female
D.LC.fem <- as.vector(as.matrix(LC.D.fem))
D.CBD.fem <- as.vector(as.matrix(CBD.D.fem))
D.RH.fem <- as.vector(as.matrix(RH.D.fem))
D.APC.fem <- as.vector(as.matrix(APC.D.fem))
D.M7.fem <- as.vector(as.matrix(M7.D.fem))
D.Plat.fem <- as.vector(as.matrix(Plat.D.fem))

# Male
D.LC.male <- as.vector(as.matrix(LC.D.male))
D.CBD.male <- as.vector(as.matrix(CBD.D.male))
D.RH.male <- as.vector(as.matrix(RH.D.male))
D.APC.male <- as.vector(as.matrix(APC.D.male))
D.M7.male <- as.vector(as.matrix(M7.D.male))
D.Plat.male <- as.vector(as.matrix(Plat.D.male))

Age <- rep(ages.fit, years.range)
Year <- rep(years.fit, each=ages.range)
Gender <- rep("f", ages.range*years.range)
Cohort <- Year-Age

Deaths.Fem <- cbind(Age, Gender, Year, Cohort, D.fem, 
                    D.LC.fem, D.CBD.fem, D.RH.fem, D.APC.fem, D.M7.fem, D.Plat.fem)
Gender <- rep("m", ages.range*years.range)

Deaths.Male <- cbind(Age, Gender, Year, Cohort, D.male, 
                    D.LC.male, D.CBD.male, D.RH.male, D.APC.male, D.M7.male, D.Plat.male)

Deaths <- as.data.frame(rbind(Deaths.Fem, Deaths.Male))
names(Deaths) <- c("Age", "Gender", "Year", "Cohort", "Observed","LC", "CBD",
                   "RH", "APC", "M7", "Plat")

Deaths$Age <- as.numeric(Deaths$Age)
Deaths$Gender <- as.factor(Deaths$Gender)
Deaths$Year <- as.numeric(Deaths$Year)
Deaths$Cohort <- as.numeric(Deaths$Cohort)
Deaths$Observed <- as.numeric(Deaths$Observed)
Deaths$LC <- as.numeric(Deaths$LC)
Deaths$CBD <- as.numeric(Deaths$CBD)
Deaths$RH <- as.numeric(Deaths$RH)
Deaths$APC <- as.numeric(Deaths$APC)
Deaths$M7 <- as.numeric(Deaths$M7)
Deaths$Plat <- as.numeric(Deaths$Plat)

# Converting Deaths to data.frame preventing the error of atomic vectors.
Deaths <- as.data.frame(Deaths)
Deaths$psi.LC <- Deaths$Observed/Deaths$LC
Deaths$psi.CBD <- Deaths$Observed/Deaths$CBD
Deaths$psi.RH <- Deaths$Observed/Deaths$RH
Deaths$psi.APC <- Deaths$Observed/Deaths$APC
Deaths$psi.M7 <- Deaths$Observed/Deaths$M7
Deaths$psi.Plat <- Deaths$Observed/Deaths$Plat
Deaths <- Deaths[-5:-11]

# Creating data frames
Deaths.LC <- Deaths[-6:-10]
Deaths.CBD <- Deaths[-5]
Deaths.CBD <- Deaths.CBD[-6:-9]
Deaths.RH <- Deaths[-5:-6]
Deaths.RH <- Deaths.RH[-6:-8]
Deaths.APC <- Deaths[-5:-7]
Deaths.APC <- Deaths.APC[-6:-7]
Deaths.M7 <- Deaths[-5:-8]
Deaths.M7 <- Deaths.M7[-6]
Deaths.Plat <- Deaths[-5:-9]

# Preparing the test data
Age.test <- rep(ages.fit, for_years.range)
Year.test <- rep(for_years.fit, each=ages.range)
Gender.test.f <- rep("f", ages.range*for_years.range)
Cohort.test <- Year.test-Age.test

Deaths.Fem.test <- cbind(Age.test, Gender.test.f, Year.test, Cohort.test)
Gender.test.m <- rep("m", ages.range*for_years.range)

Deaths.Male.test <- cbind(Age.test, Gender.test.m, Year.test, Cohort.test)

Deaths.test <- as.data.frame(rbind(Deaths.Fem.test, Deaths.Male.test))
names(Deaths.test) <- c("Age", "Gender", "Year", "Cohort")

Deaths.test$Age <- as.numeric(Deaths.test$Age)
Deaths.test$Gender <- as.factor(Deaths.test$Gender)
Deaths.test$Year <- as.numeric(Deaths.test$Year)
Deaths.test$Cohort <- as.numeric(Deaths.test$Cohort)
