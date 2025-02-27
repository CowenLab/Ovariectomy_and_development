#New MWM Plots  and Analysis

#All Packages
#install.packages("tidyverse")
#install.packages("rempsyc")
#install.packages("heplots")
#install.packages("lsr")
#install.packages("dplyr")  
library(rempsyc)
library(heplots)
library(lsr)
library(dplyr)  
library(tidyverse)

TABLExF_Full <-- read.csv('C:/Users/cowen/Documents/GitHub/Ovariectomy_and_development/MWM/MWM Master Sheet.csv')

#First Table Setup
TABLExF_Full$trial_num = TABLExF_Full$X_Trial
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 6 & TABLExF_Full$trial_num <= 12 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 6 & TABLExF_Full$trial_num <= 12 ]-6
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 12 & TABLExF_Full$trial_num <= 18 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 12 & TABLExF_Full$trial_num <= 18 ]-12
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 18 & TABLExF_Full$trial_num <= 25 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 18 & TABLExF_Full$trial_num <= 25 ]-18
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 25 & TABLExF_Full$trial_num <= 31 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 25 & TABLExF_Full$trial_num <= 31 ]-25
TABLExF_Full$trial_num[TABLExF_Full$trial_num > 31 & TABLExF_Full$trial_num <= 38 ]   = TABLExF_Full$trial_num[TABLExF_Full$trial_num > 31 & TABLExF_Full$trial_num <= 38 ]-31

#TB9 <- subset(TABLExF_Full, trial_num < 7)
TB9 <- subset(TABLExF_Full, trial_num < 8)

TB9$Strain   = factor(TB9$Strain)
TB9$strategy_cat = factor(TB9$name)
TB9$day_cat  = factor(TB9$X_Day)
TB9$age_mo   = TB9$Age.months.
TB9$animalID = factor(TB9$X_TargetID)
TB9$Day_Group_2 = factor(TB9$Day_Group_2)
TB9$Trial = factor(TB9$X_Trial)

TB9$Time_In_ASouthGoal_Quadrant = factor(TB9$time.in.s.quadrant)
TB9$Time_In_RNorthGoal_Quadrant = factor(TB9$time.in.n.quadrant)
TB9$Time_In_West_Quadrant = factor(TB9$time.in.w.quadrant)
TB9$Time_In_East_Quadrant = factor(TB9$time.in.e.quadrant)

TB9$is_thigmotaxis = (TB9$strategy == 1)*1
TB9$is_circling    = (TB9$strategy == 2)*1
TB9$is_random_path = (TB9$strategy == 3)*1
TB9$is_scanning    = (TB9$strategy == 4)*1
TB9$is_chaining    = (TB9$strategy == 5)*1
TB9$is_directed_search = (TB9$strategy == 6)*1
TB9$is_corrected_path  = (TB9$strategy == 7)*1
TB9$is_direct_path     = (TB9$strategy == 8)*1
TB9$is_perseverance    = (TB9$strategy == 9)*1

#custom_colors <- c("OVX" = "purple", "SHAM" = "orange") 
custom_colors <- c(OVX = "#9f044d", SHAM = "#000000") 


#Main Table Setup
GraphTable01 <- TB9 %>% group_by(Strain, animalID, Age.months., day_cat, Trial, Time_In_ASouthGoal_Quadrant, Time_In_RNorthGoal_Quadrant, Time_In_West_Quadrant, Time_In_East_Quadrant) %>% summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), mn_CIPL = mean(CIPL_Scores))


#Definitions
GraphTable01$Allocentric = GraphTable01$mn_dir_path + GraphTable01$mn_cor_path + GraphTable01$mn_direct
GraphTable01$Escape = GraphTable01$mn_thig + GraphTable01$mn_circ + GraphTable01$mn_rnd

#Convert back...
GraphTable01$Time_In_RNorthGoal_Quadrant <- as.numeric(as.character(GraphTable01$Time_In_RNorthGoal_Quadrant))
GraphTable01$Time_In_ASouthGoal_Quadrant <- as.numeric(as.character(GraphTable01$Time_In_ASouthGoal_Quadrant))
GraphTable01$Time_In_East_Quadrant <- as.numeric(as.character(GraphTable01$Time_In_East_Quadrant))
GraphTable01$Time_In_West_Quadrant <- as.numeric(as.character(GraphTable01$Time_In_West_Quadrant))

#Definitions Part Deux (The definitions were flawed, by keeping both goal quadrants as separate variables, the plots could not easily be generated in a way that it drew from the correct variable at a given time only)
GraphTable01$Total_Time_Goal = GraphTable01$Time_In_ASouthGoal_Quadrant + GraphTable01$Time_In_RNorthGoal_Quadrant + GraphTable01$Time_In_East_Quadrant + GraphTable01$Time_In_West_Quadrant

GraphTable01$ProportionAcquisitionGoal = (GraphTable01$Time_In_ASouthGoal_Quadrant / GraphTable01$Total_Time_Goal)
GraphTable01$ProportionReversalGoal = GraphTable01$Time_In_RNorthGoal_Quadrant / GraphTable01$Total_Time_Goal


#Check Values
GraphTable01[GraphTable01$Total_Time_Goal > 60, ]
sum(GraphTable01$Total_Time_Goal > 60, na.rm = TRUE)
GraphTable01$Total_Time_Goal[GraphTable01$Total_Time_Goal > 60]


#Test, Creating a New Variable to ensure reversal goal quadrant is correct
GraphTable01 <- GraphTable01 %>%
  mutate(TimeInGoalAR = case_when(
    !Trial %in% c(37, 38) ~ Time_In_ASouthGoal_Quadrant,
    Trial %in% c(37, 38) ~ Time_In_RNorthGoal_Quadrant,
    TRUE ~ NA_real_  # Ensures numeric output
  ))

#Definitions Part Trois
GraphTable01$ProportionGoal = (GraphTable01$TimeInGoalAR / GraphTable01$Total_Time_Goal)

#Ordering Groups Correctly
GraphTable01$Strain <- factor(GraphTable01$Strain, levels = c("SHAM", "OVX"))

#Plot with corrected value pulling across all ages for specific trials
ggplot(GraphTable01 %>% filter(Age.months. %in% c("2", "6", "9", "14"),day_cat %in% c("4", "5", "6"), Trial %in% c("25", "26","37")), 
       aes(x = day_cat, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#000000", "#9f044d")) +
  labs(title = "Time in Goal Quadrant (Trials: 24, 25, & 37 where applicable)",
       x = "Day",
       y = "Time in Goal Quadrant (s)") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))


#Probe Only Plot - 9M which is the only cohort with full A and R Probe Data for all animals (T25 and T38) (14M is missing 8 animals on Reversal)

ggplot(GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("24", "25","37", "38")), 
       aes(x = Trial, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#000000", "#9f044d")) +
  labs(title = "Probe Comparison 9M - Time in Goal Quadrant (Trials: 24, 25, 37 & 38)",
       x = "Day",
       y = "Time in Goal Quadrant (s)") +
  theme_minimal() + facet_wrap(~ Age.months., nrow = 1) + theme(plot.title = element_text(hjust = 0.5))

ggplot(GraphTable01 %>% filter(Age.months. %in% c("9"), Trial %in% c("25", "38")), 
       aes(x = Trial, y = ProportionGoal, fill = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + geom_point(aes(color = Strain), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c("#000000", "#9f044d")) +
  labs(title = "Probe Comparison 9M - Time in Goal Quadrant (Trials: 25 vs 38)",
       x = "Trial",
       y = "Time in Goal Quadrant (s)") +
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
GraphTable02 <- TB9 %>% group_by(Strain, animalID, Age.months., day_cat) %>% summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), mn_CIPL = mean(CIPL_Scores))

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