c_addPartsInfo <- function(tap_speech_1, align_flag) {

# make sure variables are in the correct format
  #tap_speech_1[,c("S1s", "S1W1s", "S1W4s", "S1W4e", "S2s", "S2W1s", "S2W4e")] %<>% lapply(function(x) as.numeric(as.character(x)))
  tap_speech_1[,c("S1W4e", "S2s", "S2W4e")] %<>% lapply(function(x) as.numeric(as.character(x)))
  
# convert RecTime to seconds + divide in bins of 0.5 s (relative to S1W4e (S1W4e = 0) and/or S2s)
  tap_speech_1$RecTime = tap_speech_1$RecTime/1000
  
# round to nearest 0.5 bin  
  if (align_flag ==  "ConfOffset"){
  tap_speech_1$SpAlignedBins = round((tap_speech_1$RecTime - tap_speech_1$S1W4e) * 2)/2 }
  if (align_flag ==  "ParticOnset"){
  tap_speech_1$SpAlignedBins = round((tap_speech_1$RecTime - tap_speech_1$S2s) * 2)/2 }

# add column with part labels
  # irrelevant : anything before or after windows of interest
  # baseline : taps during 2s before display (so last 2s of 4s blank)  
  # S1 : confederate; taps after appearance display (at 4s) but before the end of S1 (including onset latency S1)
  # S2 : participant; taps after the end of S1 but before the end of S2 (including onset latency S2)
  
  tap_speech_1$Parts[tap_speech_1$Task == "TS"] = "irrelevant"
  tap_speech_1$Parts[ (tap_speech_1$RecTime >= 2) & (tap_speech_1$RecTime < 4) ] = "Base_Part"  # 
  tap_speech_1$Parts[(tap_speech_1$RecTime >= 4) & ((tap_speech_1$RecTime - tap_speech_1$S1W4e) < 0) ] = "Sp1_Part"
  tap_speech_1$Parts[((tap_speech_1$RecTime - tap_speech_1$S1W4e) >= 0) & ((tap_speech_1$RecTime - tap_speech_1$S2W4e) < 0) ] = "Sp2_Part"
  tap_speech_1$Parts[((tap_speech_1$RecTime - tap_speech_1$S2W4e) >= 0)] = "End_Part"
  tap_speech_1$Parts = as.character(tap_speech_1$Parts)
  

  tap_speech_2 = tap_speech_1[c("Participant", "Display", "Trial", "Task", "Tap", "S1correct", "S2correct", "S1W4e", "S2W4e", "SpAlignedBins",  "Parts")]


  
  
# write out intermediate step
  #write.table(tap_speech_1, file = paste(analysisFolder, "02_Datafiles/", Experiment, "_TapSpAligned.txt", sep = ""), append = FALSE, quote = FALSE, row.names = FALSE, sep = "\t", na = "NA")
  
  
  return(tap_speech_2)
}
