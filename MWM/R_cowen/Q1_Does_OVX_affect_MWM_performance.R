###########################################################
# Does OVX impact water maze performance?
#
# TODO: 
# How are the probe trials assessed - should it not be the number of traversals over the target?
# Generate statistics 
# Konsolaki uses a Box-Cox transform (EM) to make data normal -seems overkill, but should look into. in MASS, called boxcox()
#
# Include the probability for each strategy. This is in the 'confidence' value.
# TODO: Add time_in_goal to the original MWM table and plot for all days instead of 1-4 and 5-6 separately.
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
library(effectsize)

# library(rstatix)
# library(stringr)
# install.packages('datarium')
# library(lme4)
# library(lmerTest)
#library(ggnewscale)

theme_set(theme_classic(base_size = 16))
#theme_set(theme_minimal(base_size = 16))
custom_colors <- c(INTACT = "#000000",OVX = "#9f044d") 
custom_colors2 <- c("#000000","#9f044d") 
marker_color <- "#b5b2b3"
#marker_color <- "white"



data_dir = 'C:/Users/cowen/Documents/GitHub/Ovariectomy_and_development/MWM/'
code_dir ='C:/Users/cowen/Documents/GitHub/Ovariectomy_and_development/MWM/R_cowen/'
save_dir = 'C:/Temp/'

# Load the data...
source(paste0(code_dir,'Load_MWM_Data.R'))

# Define the plot functions.

plot_MWM1 <- function(TBL,vbl,ylab_txt, titstr){
  # Assumes that all 6 days are plotted.
  ggplot(TBL, aes( x = TBL$day_cat, y= vbl,  colour = TBL$Strain, shape =TBL$Strain)) + 
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 1.4, position = position_dodge(width=0.9)) +
    scale_color_manual(values = custom_colors) + 
    scale_shape_manual(values = c(21,22)) + 
    geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), size = 1.4, alpha = 0.25, stroke = 1) + 
    facet_wrap(~TBL$Age.months., nrow = 1) + 
    ylab(ylab_txt) + xlab('Day') +
    theme(legend.position="none") + # , strip.background = element_blank() to get rid of the box around the headers.
    #geom_signif(comparisons = list(c("INTACT", "OVX")), map_signif_level=TRUE, test = 't.test')+
    ggtitle(titstr)
}

MWM_stats_1to4 <- function(TBL,vbl){
  #vbl = 'mn_CIPL'
  months = sort(unique(TBL$Age.months.)) 
  for(iMonth in months){
    print('----------')
    print(paste('MONTH: ' , iMonth))
    print('----------')
    df = subset(TBL, Age.months. == iMonth)
    mod_str = as.formula(paste(vbl,'~ Strain * day_cat + Error(animalID/(Strain * day_cat))'))
    mod <- aov(mod_str,data = df )
    print(mod_str)
    print(summary(mod))
    print(eta_squared( mod))
    mod_str2 = as.formula(paste(vbl,'~ Strain '))
    dfd = subset(TBL, Age.months. == iMonth & day_cat == 3)
    print('')
    print(paste('ttest d3 pbonf =', t.test(mod_str2,dfd)$p.value*2))
    
    dfd = subset(TBL, Age.months. == iMonth & day_cat == 4)
    print(paste('ttest d4 pbonf =', t.test(mod_str2,dfd)$p.value*2))
    
  }
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



ggplot(MWM) + aes( x = day_cat, y= CIPL_Scores, fill = Strain, color=Strain) +  
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 2.5,position = position_dodge(width=0.9)) +
  scale_fill_manual(values = custom_colors) + scale_color_manual(values = custom_colors)  + 
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9),shape=21, size= 1.5, alpha = 0.8) +
  scale_fill_manual(values = c('white', 'white'))+
  facet_wrap(~Age.months.)  +
  theme(legend.position="none")+ 
  ggtitle('All Trails (trial treated as the subj.)')


ggplot(MWM) + aes( x = day_cat, y= entropy_strat, fill = Strain, color=Strain) +  
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
group_MWM_by_day <- function(TBL){
  MWM_DAY <- TBL %>% group_by(animalID, Age.months., day_cat, Strain) %>% 
    summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , 
              mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , 
              mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), 
              mn_thig_c = mean(thig_conf), mn_circ_c = mean(circ_conf) , mn_rnd_c = mean(rand_conf) , 
              mn_scan_c = mean(scan_conf) , mn_chain_c = mean(chain_conf) , mn_direct_c = mean(dir_search_conf) , 
              mn_cor_path_c = mean(correc_conf ), mn_dir_path_c = mean(dir_path_conf), mn_persev_c = mean(persev_conf), 
              mn_allocentric_conf  = mean(allocentric_conf ), mn_escape_conf  = mean(escape_conf ),
              mn_CIPL = mean(CIPL_Scores), mn_allocentric = mean(is_allocentric), sum_allocentric = sum(is_allocentric), mn_escape = mean(is_escape),
              mn_TIE =  mean(time.in.e.quadrant.norm), mn_TIW = mean(time.in.w.quadrant.norm),mn_TIN = mean(time.in.n.quadrant.norm),
              mn_TIS = mean(time.in.s.quadrant.norm), mn_TIG = mean(time.in.goal.zone.norm),mn_TSmN = mean(time.in.s.minus.n.norm), mn_latency = mean(latency.to.goal),
              sm_goal_cross = sum(goal.crossings), mn_goal_cross = mean(goal.crossings), mn_entropy = mean(entropy_strat) )
  MWM_DAY$animalID <- factor(MWM_DAY$animalID) # gets rid of empty factors
  MWM_DAY$day_cat <- factor(MWM_DAY$day_cat) # gets rid of empty factors
  MWM_DAY$Strain <- factor(MWM_DAY$Strain) # gets rid of empty factors
  MWM_DAY
}

MWM_DAY1to6_allT <- group_MWM_by_day(subset(MWM,trial_num < 7 & X_Day < 7)) # titstr = 'days1to4 alltrials'
MWM_DAY1to4_allT <- group_MWM_by_day(subset(MWM,trial_num < 7 & X_Day < 5)) # titstr = 'days1to4 alltrials'
MWM_DAY1to4_T12 <- group_MWM_by_day(subset(MWM,trial_num < 3 & X_Day < 5)) # titstr = 'days1to4 alltrials'
MWM_DAY1to4_T56 <- group_MWM_by_day(subset(MWM,trial_num > 4 & trial_num < 7 & X_Day < 5)) # titstr = 'days1to4 alltrials'

MWM_DAY4_Probe <- group_MWM_by_day(subset(MWM, Probe == TRUE & day_cat == 4 & trial_num == 7)) 
MWM_DAY6_Probe <- group_MWM_by_day(subset(MWM, Probe == TRUE & day_cat == 6 & trial_num == 7)) 

MWM_DAY56_allT <- group_MWM_by_day(subset(MWM,trial_num < 7 & X_Day > 4 & X_Day < 7 )) 


# MWM_DAY <- TMP %>% group_by(animalID, Age.months., day_cat, Strain) %>% 
#   summarize(mn_thig = mean(is_thigmotaxis), mn_circ = mean(is_circling) , mn_rnd = mean(is_random_path) , 
#             mn_scan = mean(is_scanning) , mn_chain = mean(is_chaining) , mn_direct = mean(is_directed_search) , 
#             mn_cor_path = mean(is_corrected_path ), mn_dir_path = mean(is_direct_path), mn_persev = mean(is_perseverance), 
#             mn_thig_c = mean(thig_conf), mn_circ_c = mean(circ_conf) , mn_rnd_c = mean(rand_conf) , 
#             mn_scan_c = mean(scan_conf) , mn_chain_c = mean(chain_conf) , mn_direct_c = mean(dir_search_conf) , 
#             mn_cor_path_c = mean(correc_conf ), mn_dir_path_c = mean(dir_path_conf), mn_persev_c = mean(persev_conf), 
#             mn_allocentric_conf  = mean(allocentric_conf ), mn_escape_conf  = mean(escape_conf ),
#             mn_CIPL = mean(CIPL_Scores), mn_allocentric = mean(is_allocentric), sum_allocentric = sum(is_allocentric), mn_escape = mean(is_escape),
#             mn_TIE =  mean(time.in.e.quadrant.norm), mn_TIW = mean(time.in.w.quadrant.norm),mn_TIN = mean(time.in.n.quadrant.norm),
#             mn_TIS = mean(time.in.s.quadrant.norm), mn_TSmN = mean(time.in.s.minus.n.norm), mn_latency = mean(latency.to.goal),
#             sm_goal_cross = sum(goal.crossings), mn_entropy = mean(entropy_strat) )

# MWM_DAY$animalID <- factor(MWM_DAY$animalID) # gets rid of empty factors
# MWM_DAY$day_cat <- factor(MWM_DAY$day_cat) # gets rid of empty factors
# MWM_DAY$Strain <- factor(MWM_DAY$Strain) # gets rid of empty factors

MWM_DAY_COMBINE_MICE <- subset(MWM,trial_num < 7 & X_Day < 5) %>% group_by( Age.months., day_cat, Strain) %>% 
  summarize( sum_allocentric = sum(is_allocentric), sum_escape = sum(is_escape))


MWM_DAY_COMBINE_MICE_PROBE4 <- subset(MWM, Probe == TRUE & day_cat == 4 & trial_num == 7) %>% group_by( Age.months., Strain) %>% 
  summarize( sum_allocentric = sum(is_allocentric), sum_escape = sum(is_escape), sum_goal_crossings = sum(goal.crossings), 
             mn_CIPL = mean(CIPL_Scores))

MWM_DAY_COMBINE_MICE_PROBE6 <- subset(MWM, Probe == TRUE & day_cat == 6 & trial_num == 7) %>% group_by( Age.months., Strain) %>% 
  summarize( sum_allocentric = sum(is_allocentric), sum_escape = sum(is_escape),sum_goal_crossings = sum(goal.crossings),mn_CIPL = mean(CIPL_Scores))


# General approach to stats:
# If there is a main effect, then it is legit to do post hoc. If no main effect, no post hoc.

# CIPL: For sure in paper 
plot_MWM1(MWM_DAY1to6_allT,MWM_DAY1to6_allT$mn_CIPL,'CIPL', 'Days 1-6' )
ggsave(paste0(save_dir,"CIPLd1to6.svg"), device = "svg", width = 8, height = 4, units = "in")
# Stats for days 1-4 only.
MWM_stats_1to4(MWM_DAY1to4_allT,'mn_CIPL')

plot_MWM1(MWM_DAY1to6_allT,MWM_DAY1to6_allT$mn_TIG,'Time in Goal %', 'Days 1-6' )
ggsave(paste0(save_dir,"Time_in_goal_perc_d1to6.svg"), device = "svg", width = 8, height = 4, units = "in")
MWM_stats_1to4(MWM_DAY1to4_allT,'mn_TIG')

# Mean allocentric: For sure in paper 
plot_MWM1(MWM_DAY1to6_allT,MWM_DAY1to6_allT$mn_allocentric_conf,'mn_allocentric_conf', titstr )
ggsave(paste0(save_dir,"AlloProb_d1to6.svg"), device = "svg", width = 8, height = 4, units = "in")
MWM_stats_1to4(MWM_DAY1to4_allT,'mn_allocentric_conf')
#plot_MWM1(MWM_DAY1to4_allT,MWM_DAY1to4_allT$mn_dir_path_c,'mn_dir_path_c', titstr )
#plot_MWM1(MWM_DAY1to4_allT,MWM_DAY1to4_allT$mn_direct_c,'mn_direct_c', titstr )
#plot_MWM1(MWM_DAY1to4_allT,MWM_DAY1to4_allT$mn_cor_path_c,'mn_cor_path_c', titstr )\
# Mean escape: not really necessary.
plot_MWM1(MWM_DAY1to4_allT,MWM_DAY1to4_allT$mn_escape_conf,'mn_escape_conf', titstr )
MWM_stats_1to4(MWM_DAY1to4_allT,'mn_escape_conf')

# mn_entropy ~ Strain * day_cat + Error(animalID/(Strain * day_cat))
plot_MWM1(MWM_DAY1to6_allT,MWM_DAY1to6_allT$mn_entropy,'Entropy', titstr )
MWM_stats_1to4(MWM_DAY1to6_allT,'mn_entropy')

ggsave(paste0(save_dir,"Entropy.svg"), device = "svg", width = 8, height = 4, units = "in")



# mn_TSmN: Strong effect here. Confirms above so not sure if it adds to the story.
# plot_MWM1(MWM_DAY1to4_allT,MWM_DAY1to4_allT$mn_TSmN,'mn_TSmN',titstr )
# 
# ggsave(paste0(save_dir,"TimeSpentInTargetD1to4.svg"), device = "svg", width = 4, height = 5, units = "in")
# MWM_stats_1to4(MWM_DAY,'mn_TSmN')

# Time spent just in the goal quadrant.
#plot_MWM1(MWM_DAY1to6_allT,MWM_DAY1to6_allT$mn_TIS,'mn_TIS', titstr )

#plot_MWM1(MWM_DAY56_allT,MWM_DAY56_allT$mn_TIN,'mn_TIN', 'Reversal' )


# MWM_DAY1to4_T56 and T12 - effect there but also when all trials so not much added with just focusing on the last trials or the first trials
plot_MWM1(MWM_DAY1to4_T12,MWM_DAY1to4_T12$mn_CIPL,'CIPL', 'trials 1 and 2' )
plot_MWM1(MWM_DAY1to4_T56,MWM_DAY1to4_T56$mn_CIPL,'CIPL', 'trials 5 and 6' )


#plot_MWM1(MWM_DAY,MWM_DAY$mn_direct_c,'mn_direct_c', titstr )
#plot_MWM1(MWM_DAY,MWM_DAY$mn_dir_path_c,'mn_dir_path_c', titstr )
#plot_MWM_violin(MWM_DAY,MWM_DAY$mn_CIPL,'mn_CIPL', titstr )
#plot_MWM_box(MWM_DAY,MWM_DAY$mn_CIPL,'CIPL', titstr )
# number of goal crosses. 

plot_MWM1(MWM_DAY4_Probe,MWM_DAY4_Probe$sm_goal_cross,'goal crosses','Goal cross day 4 probe' )
ggsave(paste0(save_dir,"GOAL CROSSINGS DAY 4 PROBE.svg"), device = "svg", width = 6, height = 3, units = "in")

print(paste('ttest pbonf =', t.test(sm_goal_cross~Strain,data = subset(MWM_DAY4_Probe, Age.months. == 2))$p.value))
print(paste('ttest pbonf =', t.test(sm_goal_cross~Strain,data = subset(MWM_DAY4_Probe, Age.months. == 6))$p.value))
print(paste('ttest pbonf =', t.test(sm_goal_cross~Strain,data = subset(MWM_DAY4_Probe, Age.months. == 9))$p.value))
print(paste('ttest pbonf =', t.test(sm_goal_cross~Strain,data = subset(MWM_DAY4_Probe, Age.months. == 14))$p.value))


dfd = subset(TBL, Age.months. == iMonth & day_cat == 4)
print(paste('ttest d4 pbonf =', t.test(mod_str2,dfd)$p.value*2))

plot_MWM1(MWM_DAY6_Probe,MWM_DAY6_Probe$sm_goal_cross,'goal crosses','Goal cross day 6 probe' )
ggsave(paste0(save_dir,"GOAL CROSSINGS DAY 6 PROBE.svg"), device = "svg", width = 3, height = 3, units = "in")

#plot_MWM1(MWM_DAY6_Probe,MWM_DAY6_Probe$mn_TIN,'TIME IN target','TIN day 6 probe' )
#ggsave(paste0(save_dir,"Time in goal DAY 6 PROBE.svg"), device = "svg", width = 3, height = 3, units = "in")
#plot_MWM1(MWM_DAY4_Probe,MWM_DAY4_Probe$mn_TIN,'TIME IN target quadrant','TIN day 4 probe' )
#ggsave(paste0(save_dir,"Time in goal DAY 6 PROBE.svg"), device = "svg", width = 6, height = 3, units = "in")
#plot_MWM1(MWM_DAY1to4_allT,MWM_DAY1to4_allT$sm_goal_cross,'goal crosses',titstr )
#plot_MWM1(MWM_DAY1to4_allT,MWM_DAY1to4_allT$mn_goal_cross,'goal crosses',titstr )
#plot_MWM1(MWM_DAY1to4_allT,MWM_DAY1to4_allT$mn_goal_cross,'goal crosses',titstr )



# FIGURE 1: One graph of ONLY age for the wild type INTACT
MWM_INTACT <- subset(MWM_DAY1to4_allT, Strain == 'INTACT') 
plot_MWM1(MWM_INTACT,MWM_INTACT$mn_CIPL,'mn_CIPL',paste(titstr, ' INTACT only') )
ggsave(paste0(save_dir,"INTACT ONLYd1to4.svg"), device = "svg", width = 4, height = 5, units = "in")
MWM_INTACT <- subset(MWM_DAY56_allT, Strain == 'INTACT') 
plot_MWM1(MWM_INTACT,MWM_INTACT$mn_CIPL,'mn_CIPL','d56 INTACT only' )
ggsave(paste0(save_dir,"INTACT ONLYd56.svg"), device = "svg", width = 4, height = 5, units = "in")



MWM_INTACT <- subset(MWM_DAY1to6_allT, Strain == 'INTACT') 
plot_MWM1(MWM_INTACT,MWM_INTACT$mn_CIPL,'mn_CIPL','INTACT only' )
ggsave(paste0(save_dir,"INTACT ALL DAYS.svg"), device = "svg", width = 4, height = 5, units = "in")


#plot_MWM_box(MWM_INTACT,MWM_INTACT$mn_CIPL,'mn_CIPL',titstr )


# Show the learning within each day - difference between the last 2 and first 2 trials. CIP
#plot_MWM1(MWM_DAY_tr56,MWM_DAY_tr56$LearnTimeInSouth,'LearnTimeInSouth',titstr )

# Correlation between CIPL and other scores.
# What correlates the best with CIPL?
# ggplot(data = MWM_DAY1to4_allT, aes(x =mn_allocentric, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth() + 
#   scale_color_manual(values = custom_colors) + theme(legend.position="none") + ggtitle(paste (titstr,'Correlation between measures'))

ggplot(data = MWM_DAY1to4_allT, aes(x =mn_allocentric_conf, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth() + 
  scale_color_manual(values = custom_colors) + theme(legend.position="none") + ggtitle(paste (titstr,'Correlation between measures'))
ggsave(paste0(save_dir,"CIPLtoALLO_Corr.svg"), device = "svg", width = 5, height = 5, units = "in")
# 
# ggplot(data = MWM_DAY1to4_allT, aes(x =mn_escape, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth() + 
#   scale_color_manual(values = custom_colors) + theme(legend.position="none") + ggtitle(paste (titstr,'Correlation between measures'))
# ggplot(data = MWM_DAY1to4_allT, aes(x =mn_TIS, y = mn_CIPL, color = Strain)) + geom_point()+ facet_wrap(~Age.months.) +  geom_smooth() + 
#   scale_color_manual(values = custom_colors) + theme(legend.position="none") + ggtitle(paste (titstr,'Correlation between measures'))

# PROBE TRIALS
ggplot(MWM_DAY4_Probe, aes( x = Strain, y= mn_allocentric_conf, colour = Strain, shape =Strain)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 1.4, position = position_dodge(width=0.9)) +
  scale_color_manual(values = custom_colors) + 
  scale_shape_manual(values = c(21,22)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), size = 1.4, alpha = 0.25, stroke = 1) + 
  theme(legend.position="none") + # , strip.background = element_blank() to get rid of the box around the headers.
  geom_signif(comparisons = list(c("INTACT", "OVX")), map_signif_level=TRUE, test = 't.test')+
  facet_wrap(~Age.months.,nrow  = 1,ncol = 4) + 
  ggtitle('Probe Day 4')

for (age in sort(unique(MWM_DAY4_Probe$Age.months.))){
  s = subset(MWM_DAY4_Probe,Age.months. == age )
  print(paste('age',age))
  print(t.test(mn_allocentric_conf ~ Strain, data = s)$p.value)
}
ggsave(paste0(save_dir,"Probe_Day4.svg"), device = "svg", width = 7, height = 5, units = "in")

############## DAY 6

ggplot(MWM_DAY6_Probe, aes( x = Strain, y= mn_allocentric_conf, colour = Strain, shape =Strain)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .8, size = 1.4, position = position_dodge(width=0.9)) +
  scale_color_manual(values = custom_colors) + 
  scale_shape_manual(values = c(21,22)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), size = 1.4, alpha = 0.25, stroke = 1) + 
  theme(legend.position="none") + # , strip.background = element_blank() to get rid of the box around the headers.
  geom_signif(comparisons = list(c("INTACT", "OVX")), map_signif_level=TRUE, test = 't.test')+
  facet_wrap(~Age.months.,nrow  = 1,ncol = 4) + 
  ggtitle('Probe Day 6')

for (age in sort(unique(MWM_DAY6_Probe$Age.months.))){
  s = subset(MWM_DAY6_Probe,Age.months. == age )
  print(paste('age',age))
  print(t.test(mn_allocentric_conf ~ Strain, data = s)$p.value)
}
ggsave(paste0(save_dir,"Probe_Day6.svg"), device = "svg", width = 3.5, height = 5, units = "in")


# REVERSAL

plot_MWM1(MWM_DAY56_allT,MWM_DAY56_allT$mn_CIPL,'mn_CIPL', 'Reversal' )
ggsave(paste0(save_dir,"Reversal.svg"), device = "svg", width = 4, height = 5, units = "in")

plot_MWM1(MWM_DAY56_allT,MWM_DAY56_allT$mn_allocentric_conf,'mn', 'Reversal' )
plot_MWM1(MWM_DAY56_allT,MWM_DAY56_allT$mn_TIS,'mn_TIS', 'Reversal' )

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
vbl = 'mn_CIPL'
months = sort(unique(MWM_DAY$Age.months.)) 
for(iMonth in months){
  print(paste('MONTH: ' , iMonth))
  df = subset(MWM_DAY, Age.months. == iMonth)
  mod <- aov(mn_CIPL ~ Strain * day_cat + Error(animalID/(Strain * day_cat)),data = df )
  print(summary(mod))
  print('----------')
}

vbl = 'mn_CIPL'
months = sort(unique(MWM_DAY$Age.months.)) 
for(iMonth in months){
  print(paste('MONTH: ' , iMonth))
  df = subset(MWM_DAY, Age.months. == iMonth)
  mod_str = as.formula(paste(vbl,'~ Strain * day_cat + Error(animalID/(Strain * day_cat))'))
  mod <- aov(mod_str,data = df )
  print(summary(mod))
  print('----------')
}

