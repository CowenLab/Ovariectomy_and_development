#RTrack Analyses
#Main
#install.packages("Rtrack")
library(Rtrack)


#Bulk Processing Combined - 2 Months Full - 266 is Empty or too short
#G:\Dropbox\Foldershare\Src\R\MWM
filename <- 'G:/Dropbox/Foldershare/Src/R/MWM/MWMSimplified/R Files/ExperimentCombined2MFull.xlsx'
data_dir <- 'G:/Dropbox/Foldershare/Src/R/MWM/MWMSimplified/N2MonthsFull/NData'

Experiment2M = Rtrack::read_experiment(filename, data.dir = data_dir)
#In read_path(paste0(data.dir, as.character(experiment.data[i, "_TrackFile"])),  :
#               No valid path data was extracted from the file 'Madhavan2m_Days5-6 - Test 266.csv'. There may be a problem with the track file or the track may just be empty.

#Variable Types Check
Experiment2M$summary.variables

StrategiesCombined2M = Rtrack::call_strategy(Experiment2M$metrics)
head(StrategiesCombined2M$calls)

Rtrack::plot_strategies(StrategiesCombined2M, experiment = Experiment2M, factor = "_TargetID", exclude.probe = FALSE)

Rtrack::plot_strategies(StrategiesCombined2M, experiment = Experiment2M, factor = "Strain", exclude.probe = FALSE)

pdf(file = "Results/MWM_Strategy call confirmation Combined Full w Probe - 2M Full.pdf", height = 4)
for (i in 1:length(Experiment2M$metrics)) {Rtrack::plot_path(Experiment2M$metrics[[i]], title = paste(Experiment2M$metrics[[i]]$id, StrategiesCombined2M$calls[i, "name"]))}
dev.off()

pdf(file = "Results/MWM_Strategy call confirmation Combined Full w Probe Density - 2M Full.pdf", height = 4)
for (i in 1:length(Experiment2M$metrics)) {Rtrack::plot_density(Experiment2M$metrics[[i]], col = colorRampPalette(c("purple", "green", "orange"))(256), title = paste(Experiment2M$metrics[[i]]$id, StrategiesCombined2M$calls[i, "name"]))}
dev.off()

pdf(file = "Results/MWM_Strategy plots Combined Full w Probe - 2M Full.pdf", height = 4)
Rtrack::plot_strategies(StrategiesCombined2M, experiment = Experiment2M, factor = "_TargetID", exclude.probe = FALSE)
dev.off()

pdf(file = "Results/MWM_Strategy plots Combined Full Strain - 2M Full.pdf", height = 4)
Rtrack::plot_strategies(StrategiesCombined2M, experiment = Experiment2M, factor = "Strain", exclude.probe = FALSE)
dev.off()

# File Extraction
Rtrack::export_results(Experiment2M, file = "Results/2 Months - Full/2MCombined_Results.xlsx")

Rtrack::export_results(Experiment2M, StrategiesCombined2M, file = "Results/2 Months - Full/2MCombined_StrategiesResults.xlsx")




#Bulk Processing Combined 6 Month : Track  is empty or too short
Experiment6M = Rtrack::read_experiment("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/ExperimentCombined6mFull.xlsx", data.dir = "C:/Users/Moreau/Desktop/WaterMazeAnalysis/N6Months/NData")

StrategiesCombined6M = Rtrack::call_strategy(Experiment6M$metrics)
head(StrategiesCombined6M$calls)

Rtrack::plot_strategies(StrategiesCombined6M, experiment = Experiment6M, factor = "_TargetID", exclude.probe = FALSE)

Rtrack::plot_strategies(StrategiesCombined6M, experiment = Experiment6M, factor = "Strain", exclude.probe = FALSE)

Rtrack::plot_variable("path.length", experiment = Experiment6M, factor = "Strain", exclude.probe = FALSE, lwd = 1.5)

pdf(file = "Results/MWM_Strategy call confirmation Combined w Probe - 6M Full.pdf", height = 4)
for (i in 1:length(Experiment6M$metrics)) {Rtrack::plot_path(Experiment6M$metrics[[i]], title = paste(Experiment6M$metrics[[i]]$id, StrategiesCombined6M$calls[i, "name"]))}
dev.off()

pdf(file = "Results/MWM_Strategy call confirmation Combined w Probe Density - 6M Full.pdf", height = 4)
for (i in 1:length(Experiment6M$metrics)) {Rtrack::plot_density(Experiment6M$metrics[[i]], col = colorRampPalette(c("purple", "green", "orange"))(256), title = paste(Experiment6M$metrics[[i]]$id, StrategiesCombined6M$calls[i, "name"]))}
dev.off()

# File Extraction
Rtrack::export_results(Experiment6M, file = "Results/6 Months - Full/6MCombined_Results.xlsx")

Rtrack::export_results(Experiment6M, StrategiesCombined6M, file = "Results/6 Months - Full/6MCombined_StrategiesResults.xlsx")




#Bulk Processing Combined 9 Month : Track 236 is empty or too short
Experiment9M = Rtrack::read_experiment("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/ExperimentCombined9MFull.xlsx", data.dir = "C:/Users/Moreau/Desktop/WaterMazeAnalysis/N9Months/NData")

StrategiesCombined9M = Rtrack::call_strategy(Experiment9M$metrics)
head(StrategiesCombined9M$calls)

Rtrack::plot_strategies(StrategiesCombined9M, experiment = Experiment9M, factor = "_TargetID", exclude.probe = FALSE)

Rtrack::plot_strategies(StrategiesCombined9M, experiment = Experiment9M, factor = "Strain", exclude.probe = FALSE)

Rtrack::plot_variable("path.length", experiment = Experiment9M, factor = "Strain", exclude.probe = FALSE, lwd = 1.5)

pdf(file = "Results/MWM_Strategy call confirmation Combined w Probe - 9M Full2.pdf", height = 4)
for (i in 1:length(Experiment9M$metrics)) {Rtrack::plot_path(Experiment9M$metrics[[i]], title = paste(Experiment9M$metrics[[i]]$id, StrategiesCombined9M$calls[i, "name"]))}
dev.off()

pdf(file = "Results/MWM_Strategy call confirmation Combined w Probe Density - 9M Full.pdf", height = 4)
for (i in 1:length(Experiment9M$metrics)) {Rtrack::plot_density(Experiment9M$metrics[[i]], col = colorRampPalette(c("purple", "green", "orange"))(256), title = paste(Experiment9M$metrics[[i]]$id, StrategiesCombined9M$calls[i, "name"]))}
dev.off()

pdf(file = "Results/MWM_Strategy plots Combined w Probe - 9M Full.pdf", height = 4)
Rtrack::plot_strategies(StrategiesCombined9M, experiment = Experiment9M, factor = "_TargetID", exclude.probe = FALSE)
dev.off()

pdf(file = "Results/MWM_Strategy plots Combined Strain - 9M Full.pdf", height = 4)
Rtrack::plot_strategies(StrategiesCombined9M, experiment = Experiment9M, factor = "Strain", exclude.probe = FALSE)
dev.off()

# File Extraction
Rtrack::export_results(Experiment9M, file = "Results/9 Months - Full/9MCombined_Results.xlsx")

Rtrack::export_results(Experiment9M, StrategiesCombined9M, file = "Results/9 Months - Full/9MCombined_StrategiesResults.xlsx")




#Bulk Processing Combined 14 Month : Track 57 is empty or too short
Experiment14M = Rtrack::read_experiment("C:/Users/Moreau/Desktop/WaterMazeAnalysis/R Files/ExperimentCombined14mFull.xlsx", data.dir = "C:/Users/Moreau/Desktop/WaterMazeAnalysis/N14Months/NData")

StrategiesCombined14M = Rtrack::call_strategy(Experiment14M$metrics)
head(StrategiesCombined14M$calls)

Rtrack::plot_strategies(StrategiesCombined14M, experiment = Experiment14M, factor = "_TargetID", exclude.probe = FALSE)

Rtrack::plot_strategies(StrategiesCombined14M, experiment = Experiment14M, factor = "Strain", exclude.probe = FALSE)

Rtrack::plot_variable("path.length", experiment = Experiment14M, factor = "Strain", exclude.probe = FALSE, lwd = 1.5)

pdf(file = "Results/MWM_Strategy call confirmation Combined w Probe - 14M Full.pdf", height = 4)
for (i in 1:length(Experiment14M$metrics)) {Rtrack::plot_path(Experiment14M$metrics[[i]], title = paste(Experiment14M$metrics[[i]]$id, StrategiesCombined14M$calls[i, "name"]))}
dev.off()

pdf(file = "Results/MWM_Strategy call confirmation Combined w Probe Density - 14M Full.pdf", height = 4)
for (i in 1:length(Experiment14M$metrics)) {Rtrack::plot_density(Experiment14M$metrics[[i]], col = colorRampPalette(c("purple", "green", "orange"))(256), title = paste(Experiment14M$metrics[[i]]$id, StrategiesCombined14M$calls[i, "name"]))}
dev.off()

pdf(file = "Results/MWM_Strategy plots Combined w Probe - 14M Full.pdf", height = 4)
Rtrack::plot_strategies(StrategiesCombined14M, experiment = Experiment14M, factor = "_TargetID", exclude.probe = FALSE)
dev.off()

pdf(file = "Results/MWM_Strategy plots Combined Strain - 14M Full.pdf", height = 4)
Rtrack::plot_strategies(StrategiesCombined14M, experiment = Experiment14M, factor = "Strain", exclude.probe = FALSE)
dev.off()

# File Extraction
Rtrack::export_results(Experiment14M, file = "Results/14 Months - Full/14MCombined_Results.xlsx")

Rtrack::export_results(Experiment14M, StrategiesCombined14M, file = "Results/14 Months - Full/14MCombined_StrategiesResults.xlsx")

