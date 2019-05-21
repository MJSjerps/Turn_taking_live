a_FixToGaze  <- function(Experiment) {
  
rawDataFolder = paste(analysisFolder, "01_PreProcInput/", Experiment, "/", sep = "")
print("Generating Gaze report:")

# read in condition information
Cond_key = read.table(paste(analysisFolder, "02_Datafiles/", Experiment, "_ConditionKeyAll.txt", sep = ""), header = T, sep = "\t")
PartList = unique(Cond_key$Participant)

# read the raw fixations table and rename the idenifier of a mislabeled participant
fixations_raw = read.table(paste(rawDataFolder, Experiment, "_Fix_Combined_INPUT.txt", sep = ""), header = T, sep = "\t")
fixations_raw$RECORDING_SESSION_LABEL = gsub("3828t", "3828", fixations_raw$RECORDING_SESSION_LABEL)
fixations_raw = fixations_raw[is.element(fixations_raw$RECORDING_SESSION_LABEL, PartList), ]
#test = fixations_raw[fixations_raw$RECORDING_SESSION_LABEL == "\032", ]
#test

# generate a vector "count" on which we can sort if necessary
fixations_raw[,c("RECORDING_SESSION_LABEL", "TRIAL_INDEX", "CURRENT_FIX_INDEX")] %<>% lapply(function(x) as.numeric(as.character(x)))  
fixations_raw = fixations_raw[order(fixations_raw$RECORDING_SESSION_LABEL, fixations_raw$TRIAL_INDEX, fixations_raw$CURRENT_FIX_INDEX),]
count = 1: nrow(fixations_raw) 
fixations_raw$count = count
#plot(fixations_raw$count)

# calculate a proper fixation run total time 
fixations_raw[,c("CURRENT_FIX_END", "CURRENT_FIX_RUN_SIZE", "CURRENT_FIX_RUN_INDEX", "CURRENT_FIX_START")] %<>% lapply(function(x) as.numeric(as.character(x)))
fixations_raw$CURRENT_FIX_INTEREST_AREA_LABEL = as.character(fixations_raw$CURRENT_FIX_INTEREST_AREA_LABEL)
fixations_raw$Gaze_End = as.numeric(as.character(0))
fixations_raw$Gaze_End[count] = fixations_raw$CURRENT_FIX_END[(count + (fixations_raw$CURRENT_FIX_RUN_SIZE - fixations_raw$CURRENT_FIX_RUN_INDEX))]
fixations_raw$RUN_DUR = ((fixations_raw$Gaze_End - fixations_raw$CURRENT_FIX_START)+ 1)
fixations_raw$RECORDING_SESSION_LABEL = as.character(fixations_raw$RECORDING_SESSION_LABEL)
setnames(fixations_raw, "CURRENT_FIX_START", "Gaze_Start")

# in order to analyse gazes per display rather than individual saccades, only keep first saccade of every gaze in dataset 
Gazes_1 = fixations_raw[is.element(fixations_raw$CURRENT_FIX_RUN_INDEX, c(1)), ]

# Then exclude all runs of less than 80 ms
Gazes_1$RUN_DUR = as.numeric(as.character(Gazes_1$RUN_DUR))
#plot(density(Gazes_1$RUN_DUR, na.rm = TRUE, adjust = 0.001), xlim = c(0, 2000))
#plot(Gazes_1$RUN_DUR, ylim = c(0, 1000))

allGazes = nrow(Gazes_1)
Gazes_2 = Gazes_1[Gazes_1$RUN_DUR > 80, ]
realGazes = nrow(Gazes_2)
1 - (realGazes/allGazes) # indicates the proportion that is thrown away


# merge with condition information
Gazes_3 <- merge(Cond_key, Gazes_2,  by.x = c("Participant", "Trial"), by.y = c("RECORDING_SESSION_LABEL", "TRIAL_INDEX"))

# I recode all looks to object that occur before the object is on screen (i.e., anticipatory looks to interest area) as "Anticipation"
Gazes_3$IA_Label = Gazes_3$CURRENT_FIX_INTEREST_AREA_LABEL
Gazes_3$IA_Label[ !is.element(Gazes_3$IA_Label, c('RECTANGLE_INTERESTAREA ', 'Whole_Screen ')) &
                    (Gazes_3$Gaze_Start <= (Gazes_3$Scenestart - Gazes_3$Trialstart))] = 'Anticipation'


#GazesROI <- data.frame(table(Gazes_3$IA_Label))




# reduce number of columns
Gazes_3 = Gazes_3[c("Participant", "Trial", "Display", "Task", "Trialstart", "Recstart", "Scenestart", "Gaze_Start", "Gaze_End", "count", "IA_Label")]
Gazes_3 = Gazes_3[order(Gazes_3$count), ]

write.table(Gazes_3, file = paste(analysisFolder, "02_Datafiles/", Experiment, "_GazeReport.txt", sep = ""), append = FALSE, quote = FALSE, row.names = FALSE, sep = "\t", na = "")

}