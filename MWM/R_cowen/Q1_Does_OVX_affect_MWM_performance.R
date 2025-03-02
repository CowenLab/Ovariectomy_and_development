###########################################################
# Does OVX impact water maze performance?
###########################################################
# Cowen - 2025 based on Gabriel's curation of the main data table.
###########################################################
#install.packages("tidyverse")
#install.packages("rempsyc")
#install.packages("heplots")
#install.packages("lsr")
#install.packages("dplyr")  
#install.packages("ggthemes")
rm(list = ls()) # this clears variables in case any are already loaded. 
library(rempsyc)
library(heplots)
library(lsr)
library(dplyr)  
library(tidyverse)
library(ggthemes)
theme_set(theme_classic(base_size = 16))
#theme_set(theme_minimal())

# NOTE
# 9 mo are the only ones that had both probes.
# ("24" = Trial before probe, "25" Probe trial, "37" Trial before probe after reversal, "38" last probe after removing platform))
custom_colors <- c(OVX = "#9f044d", SHAM = "#000000") 
marker_color <- "#b5b2b3"

# LOAD AND PROCESS DATA
MWM <- read.csv('C:/Users/cowen/Documents/GitHub/Ovariectomy_and_development/MWM/MWM Master Sheet.csv')

#First Table Setup
# This is done because trials were labeled by Gabriel sequentially regardless of day so that trial 20 on day 1 
# would result in day 2 starting with trial 21.
MWM$trial_num = MWM$X_Trial
MWM$trial_num[MWM$trial_num > 6 & MWM$trial_num <= 12 ] = MWM$trial_num[MWM$trial_num > 6 & MWM$trial_num <= 12 ]-6
MWM$trial_num[MWM$trial_num > 12 & MWM$trial_num <= 18 ] = MWM$trial_num[MWM$trial_num > 12 & MWM$trial_num <= 18 ]-12
MWM$trial_num[MWM$trial_num > 18 & MWM$trial_num <= 25 ] = MWM$trial_num[MWM$trial_num > 18 & MWM$trial_num <= 25 ]-18
MWM$trial_num[MWM$trial_num > 25 & MWM$trial_num <= 31 ] = MWM$trial_num[MWM$trial_num > 25 & MWM$trial_num <= 31 ]-25
MWM$trial_num[MWM$trial_num > 31 & MWM$trial_num <= 38 ] = MWM$trial_num[MWM$trial_num > 31 & MWM$trial_num <= 38 ]-31

#MWM <- subset(TABLExF_Full, trial_num < 7)
# This is unnecessary as there is no trial 8 or larger, but a failsafe just in case
#MWM <- subset(TABLExF_Full, trial_num < 8)

MWM$Strain   = factor(MWM$Strain,levels = c('SHAM', 'OVX'))
MWM$strategy_cat = factor(MWM$name)
MWM$day_cat  = factor(MWM$X_Day)
MWM$age_mo   = MWM$Age.months.
MWM$animalID = factor(MWM$X_TargetID) # age_animal_month
MWM$Trial = factor(MWM$X_Trial) # This is the original trial that ignores day boundaries to be continuous until end of experiment.

#MWM$time.in.s.quadrant # South is the first goal quadrant and then after the reversal the North becomes the goal. 
#MWM$time.in.n.quadrant
#MWM$time.in.w.quadrant
#MWM$time.in.e.quadrant
# Determine the proportion of time in a quadrant.
S = MWM$time.in.e.quadrant + MWM$time.in.w.quadrant+ MWM$time.in.n.quadrant+ MWM$time.in.s.quadrant
MWM$time.in.e.quadrant.norm = MWM$time.in.e.quadrant/S
MWM$time.in.w.quadrant.norm = MWM$time.in.w.quadrant/S
MWM$time.in.n.quadrant.norm = MWM$time.in.n.quadrant/S
MWM$time.in.s.quadrant.norm = MWM$time.in.s.quadrant/S

MWM$time.in.s.minus.n.norm = MWM$time.in.s.quadrant.norm - MWM$time.in.n.quadrant.norm

MWM$strategy_cat = factor(MWM$strategy)
MWM$strategy_cat <- recode(MWM$strategy_cat, "1" = "thig", "2" = "circ", "3" = "rand", "4" = "scan", "5" = "chain", "6" = "dir_search", "7" = "corpth", "8" = "dirpath", "9" = "persev")
table(MWM$strategy_cat)

# This coding is useful for calculating the percent of time each strategy is used.
# Need to *1 to convert to a number instead of a logical.
MWM$is_thigmotaxis = (MWM$strategy == 1)*1
MWM$is_circling    = (MWM$strategy == 2)*1
MWM$is_random_path = (MWM$strategy == 3)*1
MWM$is_scanning    = (MWM$strategy == 4)*1
MWM$is_chaining    = (MWM$strategy == 5)*1
MWM$is_directed_search = (MWM$strategy == 6)*1
MWM$is_corrected_path  = (MWM$strategy == 7)*1
MWM$is_direct_path     = (MWM$strategy == 8)*1
MWM$is_perseverance    = (MWM$strategy == 9)*1

MWM$is_allocentric  = MWM$is_direct_path + MWM$is_directed_search + MWM$is_corrected_path
MWM$is_escape       = MWM$is_thigmotaxis + MWM$is_circling + MWM$is_random_path

#Main Table Setup. Goal is to group by day.
# This really does nothing right now other than alter variable names. for simplicity we should just stick with MWM.
#MWM_DAY <- MWM %>% group_by(Strain, animalID, Age.months., day_cat, Trial, time.in.s.quadrant, time.in.n.quadrant, time.in.w.quadrant, time.in.e.quadrant) %>% summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), mn_CIPL = mean(CIPL_Scores))
# Do we want to just average across all trils in a day or just for the last few trials?
#NOTE: GraphTable01b <- MWM %>% group_by(Strain, animalID, Age.months., day_cat, Trial, Time_In_ASouthGoal_Quadrant, Time_In_RNorthGoal_Quadrant, Time_In_West_Quadrant, Time_In_East_Quadrant) 
titstr = 'alltrials'
TMP <- subset(MWM,trial_num < 7) # do this if you only want to compute performance based on the last trials. Ignore the few trial 7 as these are probe trials.
titstr = 'trials56'
TMP <- subset(MWM, trial_num > 4 & trial_num < 7) # do this if you only want to compute performance based on the last trials. Ignore the few trial 7 as these are probe trials.
titstr = 'trials12'
TMP <- subset(MWM, trial_num < 3)  # do this if you only want to compute performance based on the first 2 trials.
#titstr = 'trial7' # This is for analyzing the probe trial only.
#TMP <- subset(MWM, trial_num == 7)  # do this if you only want to compute performance based on the first 2 trials.


MWM_DAY <- TMP %>% group_by(animalID, Age.months., day_cat, Strain) %>% 
  summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , 
            mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , 
            mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), 
            mn_CIPL = mean(CIPL_Scores), mn_allocentric = mean(is_allocentric), mn_escape = mean(is_escape),
            mn_TIE =  mean(time.in.e.quadrant.norm), mn_TIW = mean(time.in.w.quadrant.norm),mn_TIN = mean(time.in.n.quadrant.norm),
            mn_TIS = mean(time.in.s.quadrant.norm), mn_TNmS = mean(time.in.s.minus.n.norm))

# CIPL
ggplot(MWM_DAY, aes( x = day_cat, y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  facet_wrap(~Age.months.) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr)

# mn_TNmS
ggplot(MWM_DAY, aes( x = day_cat, y= mn_TNmS, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  facet_wrap(~Age.months.) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr)


# Allocentric - only really shows up for all trials since in 2 trials, you can only get values of 0, .5, and 1 at the most.
ggplot(MWM_DAY, aes( x = day_cat, y= mn_allocentric, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  facet_wrap(~Age.months.) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr)

# Proportion of time in the south quadrant
ggplot(MWM_DAY, aes( x = day_cat, y= mn_TIS, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  facet_wrap(~Age.months.) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr)

# Focus and do a t-test on day 4 and then again on day 5
#MWM_DAY4_month <- subset(MWM_DAY, Age.months. ==2 & day_cat == 4) 
#titstr2= paste('2mo day 4', titstr)
#MWM_DAY4_month <- subset(MWM_DAY, Age.months. == 6 & day_cat == 4) 
#titstr2= paste('6mo day 4', titstr)
MWM_DAY4_month <- subset(MWM_DAY, Age.months. == 9 & day_cat == 4) 
titstr2= paste('9mo day 4', titstr)
#MWM_DAY4_month <- subset(MWM_DAY, Age.months. == 14 & day_cat == 4) 
#titstr2= paste('14mo day 4', titstr)

t.test(mn_CIPL ~ Strain, data = MWM_DAY4_month)

ggplot(MWM_DAY4_month, aes( x = day_cat, y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 3.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr2)

# Focus and do a t-test on day 5 and then again on day 5
MWM_DAY5_month <- subset(MWM_DAY, Age.months. == 9 & day_cat == 5) 
titstr2= paste('9mo day 5', titstr)
t.test(mn_CIPL ~ Strain, data = MWM_DAY5_month)
#t.test(mn_allocentric ~ Strain, data = MWM_DAY5_month)
#t.test(mn_TNmS ~ Strain, data = MWM_DAY5_month)

ggplot(MWM_DAY5_month, aes( x = day_cat, y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 3.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr2)


ggplot(MWM_DAY5_month, aes( x = day_cat, y= mn_TNmS, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 3.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr2)


# Pull out just trial 7 as this is the reversal during the first half of training.
# NOTE: the mean day table does not include this trial.
# STOPPED HERE_ where to pick up coding.
#MWM_DAY4_month <- subset(MWM_DAY, Age.months. == 9 & trial_num == 7) 


# WS anova is probably overkill as we really only care about day 4 and day 5 and these are separate hypotheses 
# so I think they can be treated independently.
# Focus and do an wsANOVA - the south is the first goal and then after reversal it's the north.

MWM_DAY_month <- subset(MWM_DAY, Age.months. == 9) 
titstr2= paste('9mo ', titstr)

ggplot(MWM_DAY_month, aes( x = day_cat, y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr2)

# Within subject ANOVA. see https://www.r-bloggers.com/2025/02/two-way-repeated-measures-anova-in-r/
model.aov <- aov(mn_CIPL ~ Strain * day_cat + Error(animalID/(Strain * day_cat)),data = MWM_DAY_month)
summary(model.aov)

#model.aov <- aov(mn_CIPL ~ Strain * day_cat + Error(animalID/(day_cat)),data = MWM_DAY_month)
#summary(model.aov)

# Definitions Part Deux (The definitions were flawed, by keeping both goal 
# quadrants as separate variables, the plots could not easily be generated in a way 
# that it drew from the correct variable at a given time only)
GraphTable01$Total_Time_Goal = GraphTable01$Time_In_ASouthGoal_Quadrant + GraphTable01$Time_In_RNorthGoal_Quadrant + GraphTable01$Time_In_East_Quadrant + GraphTable01$Time_In_West_Quadrant

GraphTable01$ProportionAcquisitionGoal = (GraphTable01$Time_In_ASouthGoal_Quadrant / GraphTable01$Total_Time_Goal)
GraphTable01$ProportionReversalGoal = GraphTable01$Time_In_RNorthGoal_Quadrant / GraphTable01$Total_Time_Goal


#Check Values
GraphTable01[GraphTable01$Total_Time_Goal > 60, ]
sum(GraphTable01$Total_Time_Goal > 60, na.rm = TRUE)
GraphTable01$Total_Time_Goal[GraphTable01$Total_Time_Goal > 60]


#Test, Creating a New Variable to ensure reversal goal quadrant is correct
# STephen: I do not understand this - will need to work it out.
# recall that triuals 37, 38 are the reversal
GraphTable01 <- GraphTable01 %>%
  mutate(TimeInGoalAR = case_when(
    !Trial %in% c(37, 38) ~ Time_In_ASouthGoal_Quadrant,
    Trial %in% c(37, 38) ~ Time_In_RNorthGoal_Quadrant,
    TRUE ~ NA_real_  # Ensures numeric output
  ))

#Definitions Part Trois
GraphTable01$ProportionGoal = (GraphTable01$TimeInGoalAR / GraphTable01$Total_Time_Goal)


#Plot with corrected value pulling across all ages for specific trials
# Thhis is probabaly not necessary and not quite clear.
#ggplot(GraphTable01 %>% filter(Age.months. %in% c("2", "6", "9", "14"), day_cat %in% c("4", "5", "6"), Trial %in% c(25, 26, 37 )), 
#       aes(x = day_cat, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#000000", "#9f044d")) +
#  labs(title = "Time in Goal Quadrant (Trials: 24, 25, & 37 where applicable)",
#       x = "Day",
#       y = "Time in Goal Quadrant (s)") +
#  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))


#Probe Only Plot - 9M which is the only cohort with full A and R Probe Data for all animals (T25 and T38) (14M is missing 8 animals on Reversal)

ggplot(GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("24", "25", "37", "38")), 
       aes(x = Trial, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#000000", "#9f044d")) +
  labs(title = "Probe Comparison 9M - Time in Goal Quadrant (Trials: 24, 25, 37 & 38)",
       x = "Trial",
       y = "Proportion Time in Goal Quadrant") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))

T1 = GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("24"))
T2 = GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("25"))

delta = T2$mn_CIPL - T1$mn_CIPL
#delta = T2$ProportionGoal - T1$ProportionGoal

M = data.frame(T1$Strain, delta)

ggplot(M, aes( x = T1.Strain, y= delta, fill =  T1.Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) 

T1 = GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("25"))
T2 = GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("26"))

delta = T2$mn_CIPL - T1$mn_CIPL 
#delta = T2$ProportionGoal - T1$ProportionGoal

M = data.frame(T1$Strain, delta)

ggplot(M, aes( x = T1.Strain, y= delta, fill =  T1.Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) 






ggplot(GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("25", "38")), 
       aes(x = Trial, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#000000", "#9f044d")) +
  labs(title = "Probe Comparison 9M - Time in Goal Quadrant (Trials: 25 vs 38)",
       x = "Trial",
       y = "Proportion time in Goal Quadrant") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))


#Stats 9M - Probe comparing AProbe vs RProbe
ProbeStats9M <- GraphTable01 %>%
  filter(Age.months. == "9", Trial %in% c(25, 38))
TTestProbe9M <- t.test(ProportionGoal ~ Trial, data = ProbeStats9M)
print(TTestProbe9M)

ProbeStats9M$Trial <- factor(ProbeStats9M$Trial, levels = c(25, 38))
ProbeStats9M$Strain <- factor(ProbeStats9M$Strain) 

AnovaProbe9M <- aov(ProportionGoal ~ Trial * Strain, data = ProbeStats9M)

summary(AnovaProbe9M)


#Stats 14M - Probe comparison 
ProbeStats14M <- GraphTable01 %>%
  filter(Age.months. == "14", Trial %in% c(25, 38))

ProbeStats14M$Trial <- factor(ProbeStats14M$Trial, levels = c(25, 38))
ProbeStats14M$Strain <- factor(ProbeStats14M$Strain) 

AnovaProbe14M <- aov(ProportionGoal ~ Trial * Strain, data = ProbeStats14M)

summary(AnovaProbe14M)


#Trial 25 vs 26 Specifically
ggplot(GraphTable01 %>% filter(Age.months. %in% c("2", "6", "9", "14"), Trial %in% c("25", "26")), 
       aes(x = Trial, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#000000", "#9f044d")) +
  labs(title = "Probe Comparison Across Age - Proportion of Time in Goal Quadrant (Trials: 25 & 26)",
       x = "Trial",
       y = "Proportion of Time in Goal Quadrant") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))

#9M Probe Vs Aftermath Plot
ggplot(GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("25", "26")), 
       aes(x = Trial, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#000000", "#9f044d")) +
  labs(title = "Probe Comparison 9M - Proportion of Time in Goal Quadrant (Trials: 25 & 26)",
       x = "Trial",
       y = "Proportion of Time in Goal Quadrant") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))

#Stats 9M Probe vs Aftermath
ProbeStats9M_2 <- GraphTable01 %>%
  filter(Age.months. == "9", Trial %in% c(25, 26))
TTestProbe9M_2 <- t.test(ProportionGoal ~ Trial, data = ProbeStats9M_2)
print(TTestProbe9M_2)

ProbeStats9M_2$Trial <- factor(ProbeStats9M_2$Trial, levels = c(25, 26))
ProbeStats9M_2$Strain <- factor(ProbeStats9M_2$Strain) 

AnovaProbe9M_2 <- aov(ProportionGoal ~ Trial * Strain, data = ProbeStats9M_2)

summary(AnovaProbe9M_2)

#Across Age Plots
ggplot(GraphTable01 %>% filter(Age.months. %in% c("2", "6", "9", "14"),day_cat %in% c("5"), Strain %in% c("SHAM", "OVX")), 
       aes(x = day_cat, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#9f044d", "#000000")) +
  labs(title = "Time in Goal Quadrant (SHAM vs OVX)",
       x = "Day",
       y = "Proportion of Time in Goal Quadrant") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))

#Sham Across Age
ggplot(GraphTable01 %>% filter(Age.months. %in% c("2", "6", "9", "14"),day_cat %in% c("5"), Strain %in% c("SHAM")), 
       aes(x = day_cat, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#9f044d", "#000000")) +
  labs(title = "Time in Goal Quadrant (SHAM)",
       x = "Day",
       y = "Proportion of Time in Goal Quadrant") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))


#Stats Using Wilcoxon Method Instead
# Testing Time in Goal for Trial 25 Only
GraphTable01_Trial25 <- GraphTable01 %>%
  filter(Trial == 25)

# Perform Wilcoxon tests per age
Wilcox_Trial25_2M <- wilcox.test(TimeInGoalAR ~ Strain, data = GraphTable01_Trial25 %>% filter(Age.months. == "2"), exact = FALSE)
Wilcox_Trial25_6M <- wilcox.test(TimeInGoalAR ~ Strain, data = GraphTable01_Trial25 %>% filter(Age.months. == "6"), exact = FALSE)
Wilcox_Trial25_9M <- wilcox.test(TimeInGoalAR ~ Strain, data = GraphTable01_Trial25 %>% filter(Age.months. == "9"), exact = FALSE)
Wilcox_Trial25_14M <- wilcox.test(TimeInGoalAR ~ Strain, data = GraphTable01_Trial25 %>% filter(Age.months. == "14"), exact = FALSE)

# Results
Wilcox_Trial25_Results <- data.frame(
  Combination = c(
    "Trial 25, 2M, Strain",
    "Trial 25, 6M, Strain",
    "Trial 25, 9M, Strain",
    "Trial 25, 14M, Strain"
  ),
  p.value = c(
    Wilcox_Trial25_2M$p.value,
    Wilcox_Trial25_6M$p.value,
    Wilcox_Trial25_9M$p.value,
    Wilcox_Trial25_14M$p.value
  ),
  stringsAsFactors = FALSE
)

# Holm correction
Wilcox_Trial25_Results$adjusted_p.value <- p.adjust(Wilcox_Trial25_Results$p.value, method = "holm")

# Print results
print(Wilcox_Trial25_Results)

# Separated Results
Wilcox_Trial25_2M
Wilcox_Trial25_6M
Wilcox_Trial25_9M
Wilcox_Trial25_14M



#Strategy Analysis - GraphTable02 (Filtering things for the Goal Time made things not work for this so I just have 2 separate tables)
#Plots
# Lumping all trials together?
GraphTable02 <- MWM %>% group_by(Strain, animalID, Age.months., day_cat) %>% summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), mn_CIPL = mean(CIPL_Scores))

GraphTable02$Allocentric = GraphTable02$mn_dir_path + GraphTable02$mn_cor_path + GraphTable02$mn_direct

GraphTable02$Strain <- factor(GraphTable02$Strain, levels = c("SHAM", "OVX"))

ggplot(GraphTable02 %>% filter(Age.months. %in% c("2", "6", "9", "14"),day_cat %in% c("5")), 
       aes(x = day_cat, y = Allocentric, fill = Strain)) + geom_boxplot() + geom_point(aes(color = Strain), position=position_jitterdodge(dodge.width=0.9)) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#9f044d", "#000000")) +
  labs(title = "Allocentric Strategies on Day 5",
       x = "Day",
       y = "Proportion of Allocentric Strategies") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))


#Stats Using Wilcoxon Test
GraphTable02_Day5 <- subset(GraphTable02, day_cat != "5")

#Allocentric Strategy
# Age 2m, Day 5, Strain
Age_2_Day_5_Strain <- GraphTable02 %>%
  filter(Age.months. == "2", day_cat == "5")
Wilcox_Age_2_Day_5_Strain <- wilcox.test(Allocentric ~ Strain, data = Age_2_Day_5_Strain, exact = FALSE)

# Age 6m, Day 5, Strain
Age_6_Day_5_Strain <- GraphTable02 %>%
  filter(Age.months. == "6", day_cat == "5")
Wilcox_Age_6_Day_5_Strain <- wilcox.test(Allocentric ~ Strain, data = Age_6_Day_5_Strain, exact = FALSE)

# Age 9m, Day 5, Strain
Age_9_Day_5_Strain <- GraphTable02 %>%
  filter(Age.months. == "9", day_cat == "5")
Wilcox_Age_9_Day_5_Strain <- wilcox.test(Allocentric ~ Strain, data = Age_9_Day_5_Strain, exact = FALSE)

# Age 14m, Day 5, Strain
Age_14_Day_5_Strain <- GraphTable02 %>%
  filter(Age.months. == "14", day_cat == "5")
Wilcox_Age_14_Day_5_Strain <- wilcox.test(Allocentric ~ Strain, data = Age_14_Day_5_Strain, exact = FALSE)


Wilcox_Age_Strain_Results <- data.frame(
  Combination = c(
    "Age 2, Day 5, Strain",
    "Age 6, Day 5, Strain",
    "Age 9, Day 5, Strain",
    "Age 14, Day 5, Strain"),
  p.value = c(
    Wilcox_Age_2_Day_5_Strain$p.value,
    Wilcox_Age_6_Day_5_Strain$p.value,
    Wilcox_Age_9_Day_5_Strain$p.value,
    Wilcox_Age_14_Day_5_Strain$p.value),
  stringsAsFactors = FALSE)

# Holm correction
Wilcox_Age_Strain_Results$adjusted_p.value <- p.adjust(Wilcox_Age_Strain_Results$p.value, method = "holm")

# Print results
print(Wilcox_Age_Strain_Results)

#Separated Results
Wilcox_Age_2_Day_5_Strain
Wilcox_Age_6_Day_5_Strain
Wilcox_Age_9_Day_5_Strain
Wilcox_Age_14_Day_5_Strain

#Stats Across Age
AnovaStrategyAge <- aov(Allocentric ~ Age.months. * Strain, data = GraphTable02)

summary(AnovaStrategyAge)

