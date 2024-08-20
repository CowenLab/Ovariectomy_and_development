# see tutorials here https://rupertoverall.net/Rtrack/articles/Rtrack_tutorials_page.html
#install.packages("Rtrack")
library(Rtrack)
library(XML)

data <- xmlParse("G:\\Dropbox\\Foldershare\\!Documents\\!Journal Articles\\In Progress\\Watermaze Hormones F344 Lalitha 2024\\R\\030223 Madhavan_NSF-NSC_2month for CIPL.xml")
xml_data <- xmlToList(data)

#>     Restoring archived experiment.
#>     Processing tracks.
strategies = Rtrack::call_strategy(experiment$metrics)
Rtrack::plot_strategies(strategies, experiment = experiment, factor = "Strain",
                        exclude.probe = TRUE)

dir_name = 'E:\\Dropbox\\Foldershare\\!Documents\\!Journal Articles\\In Progress\\Watermaze Hormones F344 Lalitha 2024\\R'


# Don't think it will read .szd - need to separate into a few files.
# https://github.com/rupertoverall/Rtrack/issues/6
# For this to work you need to 
# Get the .szd file.
# Open it in ANymaze and export the experiment as an .xml file. I downloaded the demo version waith a 1 month license - seemed to work fine.
# ALSO export each Test as a csv. BEFORE doing this though, go to Protocol-->Reports-->test_data_report and set the output preferences to seconds instead of HH:MM:SS so that it imports into Rmaze.
#  --- note: these csv have time in HH:MM:SS format which will not work with the current version of Rtrack and need converrsion - this was hanled in a bug report linked above.
# # This gives you the track data for each trial (x,y) but not the data for the experiment.
# The experiment data file is constructed by hand and is described here.. https://rupertoverall.net/Rtrack/articles/Rtrack_MWM_analysis.html
# I might be able to set up matlab code to do this.
# YOU will also need an ARENA file for each arena that lists the escape zone and water maze dimensions. It's a simple file. I imagine it's in the experiment xml file generated but would have to be extracted.
# arena = Rtrack::read_arena("MWM_example/Arena_SW.txt")
#
#fname = '030223 Madhavan_NSF-NSC_2month for CIPL.szd' this is the original .szd file  but not usable but needed so that you can epxport all the data into csv and xml files.
track_fname = 'E:\\Dropbox\\Foldershare\\!Documents\\!Journal Articles\\In Progress\\Watermaze Hormones F344 Lalitha 2024\\R\\Tests_with_time_in_sec\\030223 Madhavan_NSF-NSC_2month for CIPL - Test 20.csv'

# Seems to have anymaze.csv.
identify_track_format(filename = track_fname)
#✔ This track seems to be in the format 'raw.nh.csv'.
#[1] "raw.nh.csv"
#experiment = Rtrack::read_experiment(track_fname)




Several columns are required, these all must begin with an underscore ’_’:
  
  _TrackID is a unique identifier for each track. The easiest way to do this is just write “Track_1” in the first cell and drag to fill the whole column using Excel’s autofill feature.
_TargetID is a unique identifier for each subject. Here you should put the animal ID tags, blinded patient IDs or whatever identifies the subjects.
_Day indicates the day of the experiment. Ideally, use numbers (e.g. 1 for the first experimental day).
_Trial indicates the trial number. Typically there will be multiple trials per day, but this is not necessary. The field is still required even if a one-trial-per-day paradigm is used.
_Arena is the name of the arena description file that applies to this track. This is a file path and is relative to the project directory (which is defined by project.dir in the read_experiment function. See the note on relative paths below.
                                                                                                                                           _TrackFile is the name of the arena description file that applies to this track. This is also a file path and is relative to the data directory (which is defined by data.dir in the read_experiment function. See the note on relative paths below.
                                                                                                                                                                                                                                                                                            _TrackFileFormat is the format in which the raw track data is stored. See the package documentation (run ?Rtrack::identify_track_format) for a list of the supported file formats.
supported.formats = c(
  "raw.csv",
  "raw.csv2",
  "raw.tab",
  "raw.free.csv",
  "raw.free.csv2",
  "raw.free.tab",
  "raw.nh.csv",
  "raw.nh.csv2",
  "raw.nh.tab",
  "ethovision.xt.excel",
  "ethovision.xt.csv",
  "ethovision.xt.csv2",
  "ethovision.3.csv",
  "ethovision.3.csv2",
  "actimetrics.watermaze.csv",
  "topscan.txt",
  "anymaze.csv",
  "dsnt.wmdat",
  "tracker.2.dat"
)