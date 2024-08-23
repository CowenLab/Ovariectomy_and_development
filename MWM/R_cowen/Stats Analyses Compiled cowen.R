#Analyses of Data
#install.packages("rempsyc")
#install.packages("heplots")
#install.packages("lsr")
library(tidyverse)
library(rempsyc)
library(heplots)
library(lsr)
# Strategy groupings...

# Konsolaki 2016: Here, we performed the same analysis using a programmable software (BIOBSERVE), and subsequently combined these strategies into three groups: escape strategies
# (thigmotaxis and random search); local strategies (scanning, chaining) and global strategies (focal search, directed swimming).
# The percent of the trajectory preference in each trial was calculated and repeated measures ANOVA was performed for statistical evaluation of the data

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

# Create a within-day trial column.
TABLExF_Full$trial_num = TABLExF_Full$X_Trial
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 6 & TABLExF_Full$trial_num <= 12 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 6 & TABLExF_Full$trial_num <= 12 ]-6
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 12 & TABLExF_Full$trial_num <= 18 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 12 & TABLExF_Full$trial_num <= 18 ]-12
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 18 & TABLExF_Full$trial_num <= 25 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 18 & TABLExF_Full$trial_num <= 25 ]-18
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 25 & TABLExF_Full$trial_num <= 31 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 25 & TABLExF_Full$trial_num <= 31 ]-25
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 31 & TABLExF_Full$trial_num <= 38 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 31 & TABLExF_Full$trial_num <= 38 ]-31

# Eliminate the probe trials from analysis as we are not using them as we don't have enough data for now.
# 
MWM <- subset(TABLExF_Full, trial_num < 7)
#MWM <- subset(TABLExF_Full, trial_num > 3 & trial_num < 7) # Limit to just trials 5 and 6 when they know things.

MWM$is_thigmotaxis = (MWM$strategy == 1)*1
MWM$is_circling    = (MWM$strategy == 2)*1
MWM$is_random_path = (MWM$strategy == 3)*1
MWM$is_scanning    = (MWM$strategy == 4)*1
MWM$is_chaining    = (MWM$strategy == 5)*1
MWM$is_directed_search = (MWM$strategy == 6)*1
MWM$is_corrected_path  = (MWM$strategy == 7)*1
MWM$is_direct_path     = (MWM$strategy == 8)*1 
MWM$is_perseverance    = (MWM$strategy == 9)*1

TB$perf = TB$mn_dir_path + TB$mn_corrected

ggplot(data = MWM, aes(x = trial_num, CIPL_Scores, color = Strain)) + geom_point() + facet_wrap(~day_cat)

hist( MWM$strategy )

mean(MWM$is_thigmotaxis,na.rm = T)
mean(MWM$is_directed_search,na.rm = T)

TB <- MWM %>% group_by(day_cat, Strain, animalID, Age.months.) %>% summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , mn_corrected = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), mn_CIPL = mean(CIPL_Scores) )

# Strategies from the Konsolaki 2016 paper.
TB$escape = TB$mn_thig + TB$mn_rnd
TB$local = TB$mn_scan + TB$mn_chain
TB$global = TB$mn_dir_path + TB$mn_corrected # Directed search is not included.
# Other metrics.
TB$perf = TB$global-TB$escape
TB$allocentric = TB$mn_dir_path + TB$mn_corrected + TB$mn_direct

# Look at the effect of DAY on performance treating each Strain (OVX vs. Sham)
ggplot(data = TB, aes(x = day_cat, y = escape, color = Strain)) + geom_violin() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~Age.months.)
ggplot(data = TB, aes(x = day_cat, y = local, color = Strain)) + geom_violin() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~Age.months.)
ggplot(data = TB, aes(x = day_cat, y = global, color = Strain)) + geom_violin() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~Age.months.)
ggplot(data = TB, aes(x = day_cat, y = perf, color = Strain)) + geom_violin() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~Age.months.)
ggplot(data = TB, aes(x = day_cat, y = allocentric, color = Strain)) + geom_violin() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~Age.months.)
ggplot(data = TB, aes(x = day_cat, y = mn_CIPL, color = Strain)) + geom_violin() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~Age.months.)

# What correlates the best with CIPL?
ggplot(data = TB, aes(x =perf, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth()
ggplot(data = TB, aes(x =allocentric, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth()
ggplot(data = TB, aes(x =escape, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth()

# Is there a general effect of age?
ggplot(data = TB, aes(x = factor(Age.months.), y = mn_CIPL, color = Strain)) + geom_violin() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~day_cat)
ggplot(data = TB, aes(x = factor(Age.months.), y = perf, color = Strain)) + geom_violin() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~day_cat)
# Same question but use a regression/scatter plot. Should be a correlation.
ggplot(data = TB, aes(x =Age.months., y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~day_cat) +  geom_smooth()


# ###################

ggplot(data = TB, aes(x = day_cat, global, color = Strain)) + geom_boxplot() + geom_point(position=position_jitterdodge(dodge.width=0.9))  + facet_wrap(~Age.months.)
ggplot(data = TB, aes(x = Age.months., global, color = Strain)) + geom_boxplot() + geom_jitter() + facet_wrap(~day_cat)

ggplot(data = TB, aes(x = Age.months., global, color = Strain)) + geom_jitter() + facet_wrap(~day_cat)


boxplot(CIPL_Scores ~ Strain * name, data = MWM)
boxplot(is_thigmotaxis ~ Strain * name, data = MWM)


AnovaCIPLaov2_xF_Full <- aov(CIPL_Scores ~ Strain * X_Day, data = MWM)
EtaCIPL_xF_Full <- etaSquared(AnovaCIPLaov2_xF_Full)
