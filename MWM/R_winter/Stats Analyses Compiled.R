#Analyses of Data
install.packages("rempsyc")
install.packages("heplots")
install.packages("lsr")
library(rempsyc)
library(heplots)
library(lsr)

#Plots for all 6 days together using the main table
DAY1TABLEx <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/Tests/2MCombined_StrategiesResultswCIPLFiltered1x.csv")

ggplot(data = TESTTABLE, aes(x = factor(X_Day),  CIPL_Scores, color = Strain)) + geom_boxplot()

ggplot(data = TESTTABLE, aes(x = factor(X_Day),  strategy, color = Strain)) + geom_boxplot()

#Full 2-14 month old analysis
TABLExF_Full <- read.csv("C:/Users/Moreau/Desktop/MWM Master Sheet.csv")

AnovaCIPLaov2_xF_Full <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExF_Full)
EtaCIPL_xF_Full <- etaSquared(AnovaCIPLaov2_xF_Full)

AnovaCIPLaov2_xF_Full <- aov(CIPL_Scores ~ Strain * X_Day * Age.months, data = TABLExF_Full)


AnovaCIPLaov2_xF_Full_Age <- aov(CIPL_Scores ~ Strain * Age.months, data = TABLExF_Full)

TABLExF_Full$Age.months <- as.factor(TABLExF_Full$Age.months)
tukey <- TukeyHSD(AnovaCIPLaov2_xF_Full_Age, "Age.months")
print(tukey)


TABLExF_Full <- TABLExF_Full %>%
  rename(Age.months = Age.months.)

#NEW - 2 month old cohort
TABLExA_2m <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/2 Months - Full/2MCombined_StrategiesResults_xA.csv")

TABLExR_2m <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/2 Months - Full/2MCombined_StrategiesResults_xR.csv")

AnovaCIPLaov2_xA_2m <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExA_2m)
EtaCIPL_xA_2m <- etaSquared(AnovaCIPLaov2_xA_2m)
AnovaStrataov2_xA_2m <- aov(strategy ~ Strain * X_Day, data = TABLExA_2m)
EtaStrat_xA_2m <- etaSquared(AnovaStrataov2_xA_2m)
summary(AnovaCIPLaov2_xA_2m)
print(EtaCIPL_xA_2m)
summary(AnovaStrataov2_xA_2m)
print(EtaStrat_xA_2m)


AnovaCIPLaov2_xR_2m <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExR_2m)
EtaCIPL_xR_2m <- etaSquared(AnovaCIPLaov2_xR_2m)
AnovaStrataov2_xR_2m <- aov(strategy ~ Strain * X_Day, data = TABLExR_2m)
EtaStrat_xR_2m <- etaSquared(AnovaStrataov2_xR_2m)
summary(AnovaCIPLaov2_xR_2m)
print(EtaCIPL_xR_2m)
summary(AnovaStrataov2_xR_2m)
print(EtaStrat_xA_2m)


Means_xA_2m <- aggregate(TABLExA_2m$CIPL_Scores, by = list(TABLExA_2m$Strain), FUN = mean)
print(Means_xA_2m)
Means_xR_2m <- aggregate(TABLExR_2m$CIPL_Scores, by = list(TABLExR_2m$Strain), FUN = mean)
print(Means_xR_2m)

MeansStrat_xA_2m <- aggregate(TABLExA_2m$strategy, by = list(TABLExA_2m$Strain), FUN = mean)
print(MeansStrat_xA_2m)
MeansStrat_xR_2m <- aggregate(TABLExR_2m$strategy, by = list(TABLExR_2m$Strain), FUN = mean)
print(MeansStrat_xR_2m)




#NEW - 6 month old cohort
TABLExA_6m <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/6 Months - Full/6MCombined_StrategiesResults_xA.csv")

TABLExR_6m <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/6 Months - Full/6MCombined_StrategiesResults_xR.csv")

AnovaCIPLaov2_xA_6m <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExA_6m)
EtaCIPL_xA_6m <- etaSquared(AnovaCIPLaov2_xA_6m)
AnovaStrataov2_xA_6m <- aov(strategy ~ Strain * X_Day, data = TABLExA_6m)
EtaStrat_xA_6m <- etaSquared(AnovaStrataov2_xA_6m)
summary(AnovaCIPLaov2_xA_6m)
print(EtaCIPL_xA_6m)
summary(AnovaStrataov2_xA_6m)
print(EtaStrat_xA_6m)


AnovaCIPLaov2_xR_6m <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExR_6m)
EtaCIPL_xR_6m <- etaSquared(AnovaCIPLaov2_xR_6m)
AnovaStrataov2_xR_6m <- aov(strategy ~ Strain * X_Day, data = TABLExR_6m)
EtaStrat_xR_6m <- etaSquared(AnovaStrataov2_xR_6m)
summary(AnovaCIPLaov2_xR_6m)
print(EtaCIPL_xR_6m)
summary(AnovaStrataov2_xR_6m)
print(EtaStrat_xA_6m)


Means_xA_6m <- aggregate(TABLExA_6m$CIPL_Scores, by = list(TABLExA_6m$Strain), FUN = mean)
print(Means_xA_6m)
Means_xR_6m <- aggregate(TABLExR_6m$CIPL_Scores, by = list(TABLExR_6m$Strain), FUN = mean)
print(Means_xR_6m)

MeansStrat_xA_6m <- aggregate(TABLExA_6m$strategy, by = list(TABLExA_6m$Strain), FUN = mean)
print(MeansStrat_xA_6m)
MeansStrat_xR_6m <- aggregate(TABLExR_6m$strategy, by = list(TABLExR_6m$Strain), FUN = mean)
print(MeansStrat_xR_6m)




#NEW - 9 month old cohort
TABLExA_9m <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/9 Months - Full/9MCombined_StrategiesResults_xA.csv")

TABLExR_9m <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/9 Months - Full/9MCombined_StrategiesResults_xR.csv")

AnovaCIPLaov2_xA_9m <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExA_9m)
EtaCIPL_xA_9m <- etaSquared(AnovaCIPLaov2_xA_9m)
AnovaStrataov2_xA_9m <- aov(strategy ~ Strain * X_Day, data = TABLExA_9m)
EtaStrat_xA_9m <- etaSquared(AnovaStrataov2_xA_9m)
summary(AnovaCIPLaov2_xA_9m)
print(EtaCIPL_xA_9m)
summary(AnovaStrataov2_xA_9m)
print(EtaStrat_xA_9m)


AnovaCIPLaov2_xR_9m <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExR_9m)
EtaCIPL_xR_9m <- etaSquared(AnovaCIPLaov2_xR_9m)
AnovaStrataov2_xR_9m <- aov(strategy ~ Strain * X_Day, data = TABLExR_9m)
EtaStrat_xR_9m <- etaSquared(AnovaStrataov2_xR_9m)
summary(AnovaCIPLaov2_xR_9m)
print(EtaCIPL_xR_9m)
summary(AnovaStrataov2_xR_9m)
print(EtaStrat_xA_9m)


Means_xA_9m <- aggregate(TABLExA_9m$CIPL_Scores, by = list(TABLExA_9m$Strain), FUN = mean)
print(Means_xA_9m)
Means_xR_9m <- aggregate(TABLExR_9m$CIPL_Scores, by = list(TABLExR_9m$Strain), FUN = mean)
print(Means_xR_9m)

MeansStrat_xA_9m <- aggregate(TABLExA_9m$strategy, by = list(TABLExA_9m$Strain), FUN = mean)
print(MeansStrat_xA_9m)
MeansStrat_xR_9m <- aggregate(TABLExR_9m$strategy, by = list(TABLExR_9m$Strain), FUN = mean)
print(MeansStrat_xR_9m)




#NEW - 14 month old cohort
TABLExA_14m <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/14 Months - Full/14MCombined_StrategiesResults_xA.csv")

TABLExR_14m <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/14 Months - Full/14MCombined_StrategiesResults_xR.csv")

AnovaCIPLaov2_xA_14m <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExA_14m)
EtaCIPL_xA_14m <- etaSquared(AnovaCIPLaov2_xA_14m)
AnovaStrataov2_xA_14m <- aov(strategy ~ Strain * X_Day, data = TABLExA_14m)
EtaStrat_xA_14m <- etaSquared(AnovaStrataov2_xA_14m)
summary(AnovaCIPLaov2_xA_14m)
print(EtaCIPL_xA_14m)
summary(AnovaStrataov2_xA_14m)
print(EtaStrat_xA_14m)


AnovaCIPLaov2_xR_14m <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExR_14m)
EtaCIPL_xR_14m <- etaSquared(AnovaCIPLaov2_xR_14m)
AnovaStrataov2_xR_14m <- aov(strategy ~ Strain * X_Day, data = TABLExR_14m)
EtaStrat_xR_14m <- etaSquared(AnovaStrataov2_xR_14m)
summary(AnovaCIPLaov2_xR_14m)
print(EtaCIPL_xR_14m)
summary(AnovaStrataov2_xR_14m)
print(EtaStrat_xA_14m)


Means_xA_14m <- aggregate(TABLExA_14m$CIPL_Scores, by = list(TABLExA_14m$Strain), FUN = mean)
print(Means_xA_14m)
Means_xR_14m <- aggregate(TABLExR_14m$CIPL_Scores, by = list(TABLExR_14m$Strain), FUN = mean)
print(Means_xR_14m)

MeansStrat_xA_14m <- aggregate(TABLExA_14m$strategy, by = list(TABLExA_14m$Strain), FUN = mean)
print(MeansStrat_xA_14m)
MeansStrat_xR_14m <- aggregate(TABLExR_14m$strategy, by = list(TABLExR_14m$Strain), FUN = mean)
print(MeansStrat_xR_14m)




