c_TapsPerPart_Datatables <- function(Experiment, align_flag) {
  
  ##### Local environment: ######
  rawDataFolder = paste(analysisFolder, "01_PreProcInput/", Experiment, "/", sep = "")
  
# read in speech and tap data
speech_data_1 = read.table(paste(rawDataFolder, Experiment, "_SpeechData_INPUT.txt", sep = ""), header = T, sep = "")
speech_data_2 = speech_data_1[c("Participant", "Display", "S1W4e", "S2s", "S2W4e", "S1correct", "S2correct", "TurnGap")]
tap_data_1 = read.table(paste(analysisFolder, "02_Datafiles/", Experiment, "_TapReport.txt", sep = ""), header = T, sep = "", strip.white = FALSE)

nSubj = length(unique(speech_data_2$Participant))

# merge with taps and sort rows again
tap_speech_1 = merge(tap_data_1, speech_data_2, by.x = c("Participant", "Display"), by.y = c("Participant", "Display"))
tap_speech_1 = tap_speech_1[order(tap_speech_1$Participant, tap_speech_1$Trial, tap_speech_1$RecTime),]

# Add info on part of trial (Baseline, Sp1 or Sp2 window. "irrelevant" is before/after windows of interest), data saved in 02_TapSpeech_INT.txt
tap_speech_2 = c_addPartsInfo(tap_speech_1, align_flag)

# remove irrelevant window from tap_speech data, we keep SO (NA) trials in dataset for now. It is removed later.  
tap_speech_3 = tap_speech_2[is.element(tap_speech_2$Parts, c("Base_Part", "Sp1_Part", "Sp2_Part", "End_Part")), ]

# calculate taps per pp per picture per part
Part_Effects_1 = aggregate(tap_speech_3$Tap, by = list(tap_speech_3$Participant, tap_speech_3$Display, tap_speech_3$Task, tap_speech_3$Parts), FUN = "sum" ) 
colnames(Part_Effects_1) = c("Participant", "Display", "Task", "Parts", "NObserv")

# read in a list of pp/picture/cond combinations and merge with a list of relevant parts (necesarry to make sure all trials/parts are represtented)
Cond_Key = read.table(paste(analysisFolder, "02_Datafiles/", Experiment, "_ConditionKeyAll.txt", sep = ""),header = T, sep = "")
Cond_Key$Match = 1
Part_List = data.frame(Match = 1, Parts = c("Base_Part", "Sp1_Part", "Sp2_Part", "End_Part"))
Cond_Key_Parts <- merge(Cond_Key, Part_List, by.x = c("Match"), by.y = c("Match"))
Cond_Key_Parts = Cond_Key_Parts[c("Participant", "Display", "Task", "Parts")]
#with(Cond_Key_Parts, tapply(Match, list(Participant, Parts), sum))

# merging Part_Effects_1 with Cond_Key_Parts  (i.e, to make sure all trials and parts are represented )
Part_Effects_2 <- merge(Part_Effects_1, Cond_Key_Parts, by.x = c("Participant", "Display", "Task", "Parts"), by.y = c("Participant", "Display", "Task", "Parts"), all.y = TRUE)
Part_Effects_2$NObserv[is.na(Part_Effects_2$NObserv)] = 0  # replace na's with 0  
#with(Part_Effects_2, tapply(NObserv, list(Participant, Task, Parts), sum))

# check if all trials and all parts are represented  
if(nrow(Part_Effects_2) != (length(unique(Part_Effects_2$Participant)) * 90 * 4)) {print("-----#### ERROR: not all trials covered #####--------")}



# calculate S1 and S2 turn durations so that we can compute the tapping rate (taps per second)
turn_Info = unique(subset(tap_speech_3, select = c(Participant, Display, S1W4e, S2W4e, S2correct)))
turn_Info$Sp1PartDur = turn_Info$S1W4e - 4
turn_Info$Sp2PartDur = turn_Info$S2W4e - turn_Info$S1W4e
turn_Info_2 = unique(subset(turn_Info, select = c(Participant, Display, Sp1PartDur, Sp2PartDur, S2correct))) 
#write.table(turn_Info_2, file = paste(interFolder, "turn_Info_2.txt", sep = ""))

# merge turn duration info with tap info per part  
Part_Effects_3 = merge(Part_Effects_2, turn_Info_2, by.x = c("Participant", "Display"), by.y = c("Participant", "Display"))

# calculate part duration (baseline is always 2 seconds)
Part_Effects_3$Part_Dur = as.numeric(as.character(99))
Part_Effects_3$Part_Dur[(Part_Effects_3$Parts == "Base_Part") ] = 2
Part_Effects_3$Part_Dur[(Part_Effects_3$Parts == "Sp1_Part") ] = Part_Effects_3$Sp1PartDur[(Part_Effects_3$Parts == "Sp1_Part") ]
Part_Effects_3$Part_Dur[(Part_Effects_3$Parts == "Sp2_Part") ] = Part_Effects_3$Sp2PartDur[(Part_Effects_3$Parts == "Sp2_Part") ]

# calculate tap rate  
Part_Effects_3$Tap_Rate = Part_Effects_3$NObserv / Part_Effects_3$Part_Dur
Part_Effects_3 = Part_Effects_3[c("Participant", "Display", "Task", "Parts", "Tap_Rate", "S2correct")]
Part_Effects_3$Exp = Experiment


return(Part_Effects_3)

}