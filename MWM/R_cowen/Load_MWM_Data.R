# LOAD AND PROCESS DATA
MWM <- read.csv(paste0(data_dir,'MWM Master Sheet.csv'))
# Rename SHAM to INTACT
MWM$Strain <- recode(MWM$Strain, SHAM = 'INTACT')
MWM$X_TargetID <- str_trim(MWM$X_TargetID)

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

entropy <- function(p) {
  z <- -p * log2(p +  .Machine$double.eps)    # Compute terms
  sum(z)
}
MWM$entropy_strat = MWM$X1*NaN
for (i in 1:nrow(MWM)) {
  MWM$entropy_strat[i]  <- entropy(MWM[i, 11:19])
}


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

