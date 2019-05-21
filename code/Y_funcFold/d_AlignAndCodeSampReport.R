d_AlignAndCodeSampReport <- function(Samp_Report_1, align_flag, Experiment) {
  
  #Samp_Report_1 = Samp_Report
  Cond_key = read.table(paste(analysisFolder, "02_Datafiles/", Experiment, "_ConditionKeyAll.txt", sep = ""), header = T, sep = "\t")
  Speech_Data = read.table(paste(analysisFolder, "01_PreProcInput/", Experiment, "/", Experiment, "_SpeechData_INPUT.txt", sep = ""), header = T, sep = "\t")
  CondAndTiming <- merge(Cond_key, Speech_Data,  by.x = c("Participant", "Display"), by.y = c("Participant", "Display"))
  CondAndTiming = CondAndTiming[c("Participant", "Display", "Task", "Trialstart", "Recstart", "S1W4e", "S2W4e", "S2s", "S2correct")]
    
  Samp_Report_2 <- merge(CondAndTiming, Samp_Report_1,  by.x = c("Participant", "Display"), by.y = c("Participant", "Display"))
  Samp_Report_2 = Samp_Report_2[order(Samp_Report_2$count), ]
  Samp_Report_2[,c("Trialstart", "Recstart", "S1W4e", "S2s", "Time")] %<>% lapply(function(x) as.numeric(as.character(x)))
  
  Samp_Report_2$Recdelay_2 = (Samp_Report_2$Recstart - Samp_Report_2$Trialstart)/1000 # the annotated recordings started later than the eye-tracking timing report
  
  if (align_flag == "ConfOffset"){
  # allign start gaze to end of condefederate's turn    
    Samp_Report_2$Aligned_Gaze = round((Samp_Report_2$Time/1000) - (Samp_Report_2$S1W4e + Samp_Report_2$Recdelay_2), 1)
  }
  if (align_flag == "ParticOnset"){
  # allign start gaze to onset of participant's turn 
    Samp_Report_2$Aligned_Gaze = round((Samp_Report_2$Time/1000) - (Samp_Report_2$S2s + Samp_Report_2$Recdelay_2), 1)
  }
  if (align_flag == "RecOnset"){
    # allign start gaze to onset of the sound recording 
    Samp_Report_2$Aligned_Gaze = round((Samp_Report_2$Time/1000) - (Samp_Report_2$Recdelay_2), 1)
  }
  
 return(Samp_Report_2) 
}