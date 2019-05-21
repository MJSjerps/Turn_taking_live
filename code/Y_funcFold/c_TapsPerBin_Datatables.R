c_TapsPerBin_Datatables <- function(align_flag, Experiment) {
  
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

# Add info on part of trial (Baseline, Sp1 or Sp2 window. "irrelevant" is before/after windows of interest), 
tap_speech_2 = c_addPartsInfo(tap_speech_1, align_flag)

# calculate taps per pp per picture per part
Bin_Effects_1 = aggregate(tap_speech_2$Tap, by = list(tap_speech_2$Participant, tap_speech_2$Display, tap_speech_2$Task, tap_speech_2$SpAlignedBins), FUN = "sum" ) 
colnames(Bin_Effects_1) = c("Participant", "Display", "Task", "SpAlignedBins", "NObserv")

# read in a list of pp/picture/cond combinations and merge with a list of relevant parts (necesarry to make sure all trials/parts are represtented)
Cond_Key = read.table(paste(analysisFolder, "02_Datafiles/", Experiment, "_ConditionKeyAll.txt", sep = ""),header = T, sep = "")
Cond_Key$Match = 1
Bin_List = data.frame(Match = 1, Bins = seq(from = -10, to = 8, by = 0.5))
Cond_Key_Bins <- merge(Cond_Key, Bin_List, by.x = c("Match"), by.y = c("Match"))
Cond_Key_Bins = Cond_Key_Bins[c("Participant", "Display", "Task", "Bins")]

# merging Bin_Effects_1 with Cond_Key_Bins  (i.e, to make sure all trials and bins are represented )
Bin_Effects_2 <- merge(Bin_Effects_1, Cond_Key_Bins, by.x = c("Participant", "Display", "Task", "SpAlignedBins"), by.y = c("Participant", "Display", "Task", "Bins"), all.y = TRUE)
Bin_Effects_2$NObserv[is.na(Bin_Effects_2$NObserv)] = 0  # replace na's with 0  


# merge turn duration info with tap info per part  
turn_Info = unique(subset(tap_speech_2, select = c(Participant, Display, S2correct)))
Bin_Effects_3 = merge(Bin_Effects_2, turn_Info, by.x = c("Participant", "Display"), by.y = c("Participant", "Display"))

# calculate tap rate  
Bin_Effects_3$Tap_Rate = Bin_Effects_3$NObserv * 2
Bin_Effects_3 = Bin_Effects_3[c("Participant", "Display", "Task", "SpAlignedBins", "Tap_Rate", "S2correct")]
Bin_Effects_3$Exp = Experiment

# read in baseline table and merge with tapping data:
BaselineTapRate = read.table(paste(analysisFolder, "02_Datafiles/BaseTapRate.txt", sep = ""), header = T, sep = "")
BaselineTapRate$Parts = as.numeric(as.character(-99))
setnames(BaselineTapRate, "Parts", "SpAlignedBins")
Bin_Effects_4 = rbind(Bin_Effects_3, BaselineTapRate[BaselineTapRate$Exp == Experiment,])


return(Bin_Effects_4)

}