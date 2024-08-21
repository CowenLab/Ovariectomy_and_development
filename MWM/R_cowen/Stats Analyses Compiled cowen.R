#Analyses of Data
#install.packages("rempsyc")
#install.packages("heplots")
#install.packages("lsr")
library(tidyverse)
library(rempsyc)
library(heplots)
library(lsr)

#Plots for all 6 days together using the main table
#DAY1TABLEx <- read.csv("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/Results/Tests/2MCombined_StrategiesResultswCIPLFiltered1x.csv")

#ggplot(data = TESTTABLE, aes(x = factor(X_Day),  CIPL_Scores, color = Strain)) + geom_boxplot()

#ggplot(data = TESTTABLE, aes(x = factor(X_Day),  strategy, color = Strain)) + geom_boxplot()

#Full 2-14 month old analysis
filename = 'C:/Users/cowen/Documents/GitHub/Ovariectomy_and_development/MWM/MWM Master Sheet.csv'
TABLExF_Full <- read.csv(filename)
TABLExF_Full$Strain   = factor(TABLExF_Full$Strain)
TABLExF_Full$strategy_cat = factor(TABLExF_Full$name)
TABLExF_Full$day_cat  = factor(TABLExF_Full$X_Day)
TABLExF_Full$age_mo   = TABLExF_Full$Age.months.
TABLExF_Full$animalID = factor(TABLExF_Full$X_TargetID)

TABLExF_Full$is_thigmotaxis = (TABLExF_Full$strategy == 1)*1
TABLExF_Full$is_circling    = (TABLExF_Full$strategy == 2)*1
TABLExF_Full$is_random_path = (TABLExF_Full$strategy == 3)*1
TABLExF_Full$is_scanning    = (TABLExF_Full$strategy == 4)*1
TABLExF_Full$is_chaining    = (TABLExF_Full$strategy == 5)*1
TABLExF_Full$is_directed_search = (TABLExF_Full$strategy == 6)*1
TABLExF_Full$is_corrected_path  = (TABLExF_Full$strategy == 7)*1
TABLExF_Full$is_direct_path     = (TABLExF_Full$strategy == 8)*1 
TABLExF_Full$is_perseverance    = (TABLExF_Full$strategy == 9)*1

hist( TABLExF_Full$strategy )

#TABLExF_Full[TABLExF_Full$strategy == 8,"strategy_cat"]

mean(TABLExF_Full$is_thigmotaxis,na.rm = T)
TB <- TABLExF_Full %>% group_by(day_cat, Strain, animalID, Age.months.) %>% summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance) )
plot(TB$mn_thig)
plot(TB$mn_dir_path )
plot(TB$mn_dir_path + TB$mn_cor_path )
TB$perf = TB$mn_dir_path + TB$mn_cor_path
ggplot(data = TB, aes(x = day_cat, perf, color = Strain)) + geom_violin() + geom_jitter() + facet_wrap(~Age.months.)


boxplot(CIPL_Scores ~ Strain * name, data = TABLExF_Full)
boxplot(is_thigmotaxis ~ Strain * name, data = TABLExF_Full)



AnovaCIPLaov2_xF_Full <- aov(CIPL_Scores ~ Strain * X_Day, data = TABLExF_Full)
EtaCIPL_xF_Full <- etaSquared(AnovaCIPLaov2_xF_Full)

AnovaCIPLaov2_xF_Full <- aov(CIPL_Scores ~ Strain * X_Day * Age.months, data = TABLExF_Full)


TABLExF_Full$Age.months <- as.factor(TABLExF_Full$Age.months)
AnovaCIPLaov2_xF_Full_Age <- aov(CIPL_Scores ~ Strain * Age.months, data = TABLExF_Full)
tukey <- TukeyHSD(AnovaCIPLaov2_xF_Full_Age, "Age.months")
print(tukey)

boxplot(CIPL_Scores ~ Strain * strategy, data = TABLExF_Full)


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




