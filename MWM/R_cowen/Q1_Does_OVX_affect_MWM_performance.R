###########################################################
# Does OVX impact water maze performance?
#
# TODO: 
# How are the probe trials assessed - should it not be the number of traversals over the target?
# Generate statistics 
# Konsolaki uses a Box-Cox transform (EM) to make data normal -seems overkill, but should look into. in MASS, called boxcox()
#
# Include the probability for each strategy. This is in the 'confidence' value.
#
# There are a tone of NAs in latency to goal - why? > 60? I think latency is not somethign we can analyze - it's not calculated completely.
###########################################################
# Cowen - 2025 based on Gabriel's curation of the main data table.
###########################################################

rm(list = ls()) # this clears variables in case any are already loaded. 
library(rempsyc)
library(heplots)
library(lsr)
library(dplyr)  
library(tidyverse)
library(ggthemes)
library(ggsignif)

#library(ggnewscale)

theme_set(theme_classic(base_size = 16))
#theme_set(theme_minimal(base_size = 16))
plot_MWM1 <- function(TBL,vbl,ylab_txt, titstr){
  ggplot(TBL, aes( x = TBL$day_cat, y= vbl,  colour = TBL$Strain, shape =TBL$Strain)) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 1.8, position = position_dodge(width=0.9)) +
    scale_color_manual(values = custom_colors) + 
    scale_shape_manual(values = c(21,22)) + 
    geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), size = 2.2, alpha = 0.65, stroke = 1) + 
    #geom_dotplot(position = position_dodge(width=0.9),
    #             binaxis='y', 
  #               stackdir='center', 
  #               dotsize = 1.4) + 
    facet_wrap(~TBL$Age.months.) + 
    ylab(ylab_txt) +
    theme(legend.position="none")+ 
    geom_signif(comparisons = list(c("INTACT", "OVX")), map_signif_level=TRUE)+
    ggtitle(titstr)
}


plot_MWM_box <- function(TBL,vbl,ylab_txt, titstr){
  
  #ggplot(MWM_DAY, aes( x = day_cat, y= mn_TSmN, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  #  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  #  facet_wrap(~Age.months.) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  #  ggtitle(titstr)
    
  ggplot(TBL, aes( x = TBL$day_cat, y= vbl,  colour = TBL$Strain, shape =TBL$Strain, fill =TBL$Strain)) + 
    geom_boxplot(position = position_dodge(width = 0.9)) + 
    geom_point(position = position_jitterdodge(jitter.width = 0.22, dodge.width = 0.9), size = 2.2, alpha = 0.65, stroke = 1) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .4, size = 1.8, position = position_dodge(width=0.9)) +
    scale_color_manual(values = c(marker_color, marker_color)) + 
    scale_shape_manual(values = c(21,22)) + 
    scale_fill_manual(values = custom_colors) +
    facet_wrap(~TBL$Age.months.) + 
    ylab(ylab_txt) +
    theme(legend.position="none")+ 
    ggtitle(titstr)

}

plot_MWM_violin <- function(TBL,vbl,ylab_txt, titstr){
  ggplot(TBL, aes( x = TBL$day_cat, y= vbl,  colour = TBL$Strain, shape =TBL$Strain)) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 1.8, position = position_dodge(width=0.9)) +
    scale_color_manual(values = custom_colors) +scale_shape_manual(values = c(21,22)) + geom_violin() + 
    geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), size = 2.2, alpha = 0.65, stroke = 1) + 
    facet_wrap(~TBL$Age.months.) + ylab(ylab_txt) +
    theme(legend.position="none")+ 
    ggtitle(titstr)
}

# NOTE
# 9 mo are the only ones that had both probes.
# ("24" = Trial before probe, "25" Probe trial, "37" Trial before probe after reversal, "38" last probe after removing platform))
custom_colors <- c(INTACT = "#000000",OVX = "#9f044d") 
custom_colors2 <- c("#000000","#9f044d") 
marker_color <- "#b5b2b3"
#marker_color <- "white"

# LOAD AND PROCESS DATA
MWM <- read.csv('C:/Users/cowen/Documents/GitHub/Ovariectomy_and_development/MWM/MWM Master Sheet.csv')
# Rename SHAM to INTACT
MWM$Strain <- recode(MWM$Strain, SHAM = 'INTACT')

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

MWM$Strain   = factor(MWM$Strain,levels = c('INTACT', 'OVX'))
MWM$strategy_cat = factor(MWM$name)
MWM$day_cat  = factor(MWM$X_Day)
MWM$age_mo_cat  = factor(MWM$Age.months.)
MWM$animalID = factor(MWM$X_TargetID) # age_animal_month
MWM$Trial = factor(MWM$X_Trial) # This is the original trial that ignores day boundaries to be continuous until end of experiment.

#MWM$time.in.s.quadrant # South is the first goal quadrant and then after the reversal the North becomes the goal. 
#MWM$time.in.n.quadrant
#MWM$time.in.w.quadrant
#MWM$time.in.e.quadrant
# Determine the proportion of time in a quadrant.
S = MWM$time.in.e.quadrant + MWM$time.in.w.quadrant + MWM$time.in.n.quadrant + MWM$time.in.s.quadrant
MWM$time.in.e.quadrant.norm = 100*MWM$time.in.e.quadrant/S
MWM$time.in.w.quadrant.norm = 100*MWM$time.in.w.quadrant/S
MWM$time.in.n.quadrant.norm = 100*MWM$time.in.n.quadrant/S
MWM$time.in.s.quadrant.norm = 100*MWM$time.in.s.quadrant/S

MWM$time.in.s.minus.n.norm = MWM$time.in.s.quadrant.norm - MWM$time.in.n.quadrant.norm

MWM$strategy_cat = factor(MWM$strategy)
MWM$strategy_cat <- recode(MWM$strategy_cat, "1" = "thig", "2" = "circ", "3" = "rand", "4" = "scan", "5" = "chain", "6" = "dir_search", "7" = "corpth", "8" = "dirpath", "9" = "persev")
colnames(MWM)[colnames(MWM) == 'X1'] <- 'thig_conf'
colnames(MWM)[colnames(MWM) == 'X2'] <- 'circ_conf'
colnames(MWM)[colnames(MWM) == 'X3'] <- 'rand_conf'
colnames(MWM)[colnames(MWM) == 'X4'] <- 'scan_conf'
colnames(MWM)[colnames(MWM) == 'X5'] <- 'chain_conf'
colnames(MWM)[colnames(MWM) == 'X6'] <- 'dir_search_conf'
colnames(MWM)[colnames(MWM) == 'X7'] <- 'correc_conf'
colnames(MWM)[colnames(MWM) == 'X8'] <- 'dir_path_conf'
colnames(MWM)[colnames(MWM) == 'X9'] <- 'persev_conf'

MWM$allocentric_conf = MWM$dir_path_conf + MWM$dir_search_conf + MWM$correc_conf # X7 = corrected path
MWM$escape_conf = MWM$thig_conf + MWM$circ_conf + MWM$rand_conf # X7 = corrected path

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



ggplot(MWM) + aes( x = day_cat, y= CIPL_Scores, fill = Strain, color=Strain) +  
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 2.5,position = position_dodge(width=0.9)) +
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = custom_colors)  + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9),shape=21, size= 1.5, alpha = 0.8) +
  scale_fill_manual(values = c('white', 'white'))+
  facet_wrap(~Age.months.)  +
  theme(legend.position="none")+ 
  ggtitle('All Trails (trial treated as the subj.)')


ggplot(MWM) + aes( x = day_cat, y= dir_path_conf, fill = Strain, color=Strain) +  
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 2.5,position = position_dodge(width=0.9)) +
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = custom_colors)  + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9),shape=21, size= 1.5, alpha = 0.8) +
  scale_fill_manual(values = c('white', 'white'))+
  facet_wrap(~Age.months.)  +
  theme(legend.position="none")+ 
  ggtitle('All Trails (trial treated as the subj.)')


ggplot(MWM) + aes( x = day_cat, y= CIPL_Scores, fill = Strain, color=Strain) +  
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 2.5,position = position_dodge(width=0.9)) +
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = custom_colors)  + 
  geom_dotplot(position = position_dodge(width=0.9),
               binaxis='y', 
               stackdir='center', 
               dotsize = .5) + 
  scale_fill_manual(values = c('white', 'white'))+
  facet_wrap(~Age.months.)  +
  theme(legend.position="none")+ 
  ggtitle('All Trails (trial treated as the subj.)')


ggplot(MWM, aes( x = day_cat, y= CIPL_Scores , fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  scale_fill_manual(values = custom_colors) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = 0.8 ) + 
  facet_wrap(~Age.months.) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('All Trails (trial treated as the subj.)')


ggplot(MWM, aes( x = day_cat, y= goal.crossings , fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  facet_wrap(~Age.months.) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('All Trails (trial treated as the subj.)')


ggplot(subset(MWM,trial_num ==1), aes( x = day_cat, y= CIPL_Scores , fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  facet_wrap(~Age.months.) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('Trial 1 only (trial treated as the subj.)')

ggplot(MWM, aes( x = day_cat, y= latency.to.goal , fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  facet_wrap(~Age.months.) + scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('All Trails (trial treated as the subj.)')
# Acquisition = days 1-4, Reversal = days 5 and 6

#Main Table Setup. Goal is to group by day.
# This really does nothing right now other than alter variable names. for simplicity we should just stick with MWM.
#MWM_DAY <- MWM %>% group_by(Strain, animalID, Age.months., day_cat, Trial, time.in.s.quadrant, time.in.n.quadrant, time.in.w.quadrant, time.in.e.quadrant) %>% summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), mn_CIPL = mean(CIPL_Scores))
# Do we want to just average across all trils in a day or just for the last few trials?
#NOTE: GraphTable01b <- MWM %>% group_by(Strain, animalID, Age.months., day_cat, Trial, Time_In_ASouthGoal_Quadrant, Time_In_RNorthGoal_Quadrant, Time_In_West_Quadrant, Time_In_East_Quadrant) 
#titstr = 'alltrials'
#TMP <- subset(MWM,trial_num < 7) # do this if you only want to compute performance based on the last trials. Ignore the few trial 7 as these are probe trials.
titstr = 'days1to4 alltrials'
TMP <- subset(MWM,trial_num < 7 & X_Day < 5) # do this if you only want to compute performance based on the last trials. Ignore the few trial 7 as these are probe trials.
#titstr = 'trials56'
#TMP <- subset(MWM, trial_num > 4 & trial_num < 7) # do this if you only want to compute performance based on the last trials. Ignore the few trial 7 as these are probe trials.
#titstr = 'trials12'
#TMP <- subset(MWM, trial_num < 3)  # do this if you only want to compute performance based on the first 2 trials.
#titstr = 'trial 1'
#TMP <- subset(MWM, trial_num == 1)  # do this if you only want to compute performance based on the first 2 trials.
#titstr = 'trial7' # This is for analyzing the probe trial only.
#TMP <- subset(MWM, trial_num == 7)  # do this if you only want to compute performance based on the first 2 trials.


MWM_DAY <- TMP %>% group_by(animalID, Age.months., day_cat, Strain) %>% 
  summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , 
            mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , 
            mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), 
            mn_thig_c = mean(thig_conf), mn_circ_c = mean(circ_conf) , mn_rnd_c = mean(rand_conf) , 
            mn_scan_c = mean(scan_conf) , mn_chain_c = mean(chain_conf) , mn_direct_c = mean(dir_search_conf) , 
            mn_cor_path_c = mean(correc_conf ), mn_dir_path_c = mean(dir_path_conf), mn_persev_c = mean(persev_conf), 
            mn_allocentric_conf  = mean(allocentric_conf ), mn_escape_conf  = mean(escape_conf ),
            mn_CIPL = mean(CIPL_Scores), mn_allocentric = mean(is_allocentric), sum_allocentric = sum(is_allocentric), mn_escape = mean(is_escape),
            mn_TIE =  mean(time.in.e.quadrant.norm), mn_TIW = mean(time.in.w.quadrant.norm),mn_TIN = mean(time.in.n.quadrant.norm),
            mn_TIS = mean(time.in.s.quadrant.norm), mn_TSmN = mean(time.in.s.minus.n.norm), mn_latency = mean(latency.to.goal),
            sm_goal_cross = sum(goal.crossings))

MWM_DAY_COMBINE_MICE <- TMP %>% group_by( Age.months., day_cat, Strain) %>% 
  summarize( sum_allocentric = sum(is_allocentric), sum_escape = sum(is_escape))


MWM_DAY_COMBINE_MICE_PROBE4 <- subset(MWM, Probe == TRUE & day_cat == 4 & trial_num == 7) %>% group_by( Age.months., Strain) %>% 
  summarize( sum_allocentric = sum(is_allocentric), sum_escape = sum(is_escape), sum_goal_crossings = sum(goal.crossings), mn_CIPL = mean(CIPL_Scores))

MWM_DAY_COMBINE_MICE_PROBE6 <- subset(MWM, Probe == TRUE & day_cat == 6 & trial_num == 7) %>% group_by( Age.months., Strain) %>% 
  summarize( sum_allocentric = sum(is_allocentric), sum_escape = sum(is_escape),sum_goal_crossings = sum(goal.crossings),mn_CIPL = mean(CIPL_Scores))


MWM_DAY_tr12 <- subset(MWM, trial_num < 3)  %>% group_by(animalID, Age.months., day_cat, Strain) %>% 
  summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , 
            mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , 
            mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), 
            mn_CIPL = mean(CIPL_Scores), mn_allocentric = mean(is_allocentric), sum_allocentric = sum(is_allocentric), mn_escape = mean(is_escape),
            mn_TIE =  mean(time.in.e.quadrant.norm), mn_TIW = mean(time.in.w.quadrant.norm),mn_TIN = mean(time.in.n.quadrant.norm),
            mn_TIS = mean(time.in.s.quadrant.norm), mn_TSmN = mean(time.in.s.minus.n.norm), mn_latency = mean(latency.to.goal),
            sm_goal_cross = sum(goal.crossings))

MWM_DAY_tr56 <- subset(MWM, trial_num > 4 & trial_num < 7 )  %>% group_by(animalID, Age.months., day_cat, Strain) %>% 
  summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , 
            mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , 
            mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), 
            mn_CIPL = mean(CIPL_Scores), mn_allocentric = mean(is_allocentric), sum_allocentric = sum(is_allocentric), mn_escape = mean(is_escape),
            mn_TIE =  mean(time.in.e.quadrant.norm), mn_TIW = mean(time.in.w.quadrant.norm),mn_TIN = mean(time.in.n.quadrant.norm),
            mn_TIS = mean(time.in.s.quadrant.norm), mn_TSmN = mean(time.in.s.minus.n.norm), mn_latency = mean(latency.to.goal),
            sm_goal_cross = sum(goal.crossings))

MWM_DAY_tr56$LearnCIPL =  MWM_DAY_tr56$mn_CIPL - MWM_DAY_tr12$mn_CIPL
MWM_DAY_tr56$LearnTimeInSouth =  MWM_DAY_tr56$mn_TIS - MWM_DAY_tr12$mn_TIS

# CIPL: 
plot_MWM1(MWM_DAY,MWM_DAY$mn_CIPL,'CIPL', titstr )
plot_MWM1(MWM_DAY,MWM_DAY$mn_allocentric_conf,'mn_allocentric_conf', titstr )
plot_MWM1(MWM_DAY,MWM_DAY$mn_escape_conf,'mn_escape_conf', titstr )
#plot_MWM1(MWM_DAY,MWM_DAY$mn_direct_c,'mn_direct_c', titstr )
#plot_MWM1(MWM_DAY,MWM_DAY$mn_dir_path_c,'mn_dir_path_c', titstr )
#plot_MWM_violin(MWM_DAY,MWM_DAY$mn_CIPL,'mn_CIPL', titstr )
#plot_MWM_box(MWM_DAY,MWM_DAY$mn_CIPL,'CIPL', titstr )
# number of goal crosses. 
plot_MWM1(MWM_DAY,MWM_DAY$sm_goal_cross,'goal crosses',titstr )

# Show the learning within each day - difference between the last 2 and first 2 trials. Proportion of time in quadrant.
plot_MWM1(MWM_DAY_tr56,MWM_DAY_tr56$LearnCIPL,'LearnCIPL',titstr )

# Show the learning within each day - difference between the last 2 and first 2 trials. CIP
plot_MWM1(MWM_DAY_tr56,MWM_DAY_tr56$LearnTimeInSouth,'LearnTimeInSouth',titstr )

# Correlation between CIPL and other scores.
# What correlates the best with CIPL?
ggplot(data = MWM_DAY, aes(x =mn_allocentric, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth() + 
  scale_color_manual(values = custom_colors) + ggtitle(paste (titstr,'Correlation between measures'))
ggplot(data = MWM_DAY, aes(x =mn_escape, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth() + 
  scale_color_manual(values = custom_colors) + ggtitle(paste (titstr,'Correlation between measures'))
ggplot(data = MWM_DAY, aes(x =mn_TIS, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth() + 
  scale_color_manual(values = custom_colors) + ggtitle(paste (titstr,'Correlation between measures'))

#ggplot(data = TB, aes(x =escape, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth()


# mn_TSmN
plot_MWM1(MWM_DAY,MWM_DAY$mn_TSmN,'mn_TSmN',titstr )
plot_MWM_box(MWM_DAY,MWM_DAY$mn_TSmN,'mn_TSmN',titstr )

# Allocentric - only really shows up for all trials since in 2 trials, you can only get values of 0, .5, and 1 at the most
plot_MWM_box(MWM_DAY,MWM_DAY$mn_allocentric,'mn_allocentric',titstr )


# Proportion of time in the south quadrant

plot_MWM1(MWM_DAY,MWM_DAY$mn_TIS,'mn_TIS',titstr )

# sum of allocentric and escape using MWM_DAY_COMBINE_MICE
ggplot(MWM_DAY_COMBINE_MICE, aes( x = day_cat, y= sum_allocentric, fill =  Strain, colour = Strain)) + 
  geom_col(position = position_dodge(width = 0.9), color = 'black') + scale_fill_manual(values = custom_colors) + facet_wrap(~Age.months.)  +
  ggtitle(titstr)

ggplot(MWM_DAY_COMBINE_MICE, aes( x = day_cat, y= sum_escape, fill =  Strain, colour = Strain)) + 
  geom_col(position = position_dodge(width = 0.9), color = 'black') + scale_fill_manual(values = custom_colors) + facet_wrap(~Age.months.) +
  ggtitle(titstr)

ggplot(MWM_DAY_COMBINE_MICE, aes( x = day_cat, y= sum_allocentric/sum_escape, fill =  Strain, colour = Strain)) + 
  geom_col(position = position_dodge(width = 0.9), color = 'black') + scale_fill_manual(values = custom_colors) + facet_wrap(~Age.months.) +
  ggtitle(titstr)

ggplot(MWM_DAY_COMBINE_MICE_PROBE4, aes( x = Strain, y= sum_escape, fill =  Strain, colour = Strain)) + 
  geom_col(position = position_dodge(width = 0.9), color = 'black') + scale_fill_manual(values = custom_colors) + facet_wrap(~Age.months.)  +
  ggtitle(titstr)

ggplot(MWM_DAY_COMBINE_MICE_PROBE6, aes( x = Strain, y= sum_allocentric, fill =  Strain, colour = Strain)) + 
  geom_col(position = position_dodge(width = 0.9), color = 'black') + scale_fill_manual(values = custom_colors) + facet_wrap(~Age.months.)  +
  ggtitle(titstr)

# sum allocentric but just for probe trials.

# FIGURE 1: One graph of ONLY age for the wild type INTACT
MWM_INTACT <- subset(MWM_DAY, Strain == 'INTACT') 
plot_MWM1(MWM_INTACT,MWM_INTACT$mn_CIPL,'mn_CIPL',paste(titstr, ' INTACT only') )
#plot_MWM_box(MWM_INTACT,MWM_INTACT$mn_CIPL,'mn_CIPL',titstr )


# FIGURE 1: One graph of ONLY age for the wild type INTACT
plot_MWM1(MWM_INTACT,MWM_INTACT$mn_TIS,'mn_TIS',paste(titstr, ' INTACT only') )

# Now restrict to just Day 4
MWM_INTACT_Day4 <- subset(MWM_DAY, Strain == 'INTACT' & day_cat == 4) 

ggplot(MWM_INTACT_Day4, aes( x = factor(Age.months.), y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(paste(titstr, 'Day 4 INTACT only'))

# INTACT ONLY: Now restrict to just Day 5 Reversal
MWM_INTACT_Day5 <- subset(MWM_DAY, Strain == 'INTACT' & day_cat == 5)
ggplot(MWM_INTACT_Day5, aes( x = factor(Age.months.), y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(paste(titstr, 'Day 5 (Reversal) INTACT only'))
# Focus Day 4 and then again on day 5
MWM_DAY4 <- subset(MWM_DAY,day_cat == 4) 
ggplot(MWM_DAY4, aes( x = factor(Age.months.), y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(paste(titstr, 'Day 4'))
MWM_DAY5 <- subset(MWM_DAY,day_cat == 5) 
ggplot(MWM_DAY5, aes( x = factor(Age.months.), y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(paste(titstr, 'Day 5'))

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


# Pull out just trial 7 as this is the PROBE during the first half of training.
# NOTE: the mean day table does not include this trial.
MWM_DAY4_probe = subset(MWM, Probe == TRUE & day_cat == 4 & trial_num == 7)

ggplot(MWM_DAY4_probe, aes( x = age_mo_cat, y= CIPL_Scores, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 3.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('PROBE DAY 4')

ggplot(MWM_DAY4_probe, aes( x = age_mo_cat, y= time.in.s.quadrant.norm , fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 3.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('PROBE DAY 4')

ggplot(MWM_DAY4_probe, aes( x = age_mo_cat, y= latency.to.goal , fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 3.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('PROBE DAY 4')

# DAY 6 PROBE

MWM_DAY6_probe = subset(MWM, Probe == TRUE & day_cat == 6 & trial_num == 7)

ggplot(MWM_DAY6_probe, aes( x = age_mo_cat, y= CIPL_Scores, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 3.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('PROBE DAY 6')

ggplot(MWM_DAY6_probe, aes( x = age_mo_cat, y= time.in.n.quadrant.norm , fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 3.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle('PROBE DAY 6')

# WS anova is probably overkill as we really only care about day 4 and day 5 and these are separate hypotheses 
# so I think they can be treated independently.
# Focus and do an wsANOVA - the south is the first goal and then after reversal it's the north.

MWM_DAY_month <- subset(MWM_DAY, Age.months. == 9) 
titstr2= paste('9mo ', titstr)

ggplot(MWM_DAY_month, aes( x = day_cat, y= mn_CIPL, fill =  Strain, colour = Strain)) + geom_boxplot(position = position_dodge(width = 0.9)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), size = 1.8, alpha = .7 ) + 
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = c(marker_color, marker_color)) + 
  ggtitle(titstr2)

# PROBE TRIALS: There are some probe trials - trial 7 on day 4 (X_Trial 25) and trial 7 on Day 6 (X_Trial = 38)   
# Stats 9M - Probe comparing AProbe vs RProbe



# All below may not be necessary - check 
# Within subject ANOVA. see https://www.r-bloggers.com/2025/02/two-way-repeated-measures-anova-in-r/
model.aov <- aov(mn_CIPL ~ Strain * day_cat + Error(animalID/(Strain * day_cat)),data = MWM_DAY_month)
summary(model.aov)
