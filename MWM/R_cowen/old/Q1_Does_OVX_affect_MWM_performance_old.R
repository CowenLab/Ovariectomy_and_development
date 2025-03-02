rm(list = ls()) # this clears variables in case any are already loaded. 
library(tidyverse)
TBL <- read.csv('G:\\Dropbox\\Foldershare\\!Documents\\!Journal Articles\\In Progress\\Watermaze Hormones F344 Lalitha 2024\\Data\\2MCombined_StrategiesResults.csv')
ggplot(data = TBL, aes(x = factor(X_Trial),  path.length, color = Strain)) + geom_boxplot()
ggplot(data = TBL, aes(x = X_Trial, path.length, color = Strain)) + geom_point()
ggplot(data = TBL, aes(x = X_Trial, strategy, color = Strain)) + geom_jitter()

