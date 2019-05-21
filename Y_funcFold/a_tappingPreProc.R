a_tappingPreProc <- function(Experiment) {
  

  ##### Local environment:
  rawDataFolder = paste(analysisFolder, "01_PreProcInput/", Experiment, "/", sep = "")
  
  dataFolder = paste(analysisFolder, "02_Datafiles/",  sep = "")

  ### READ IN DATA AND SELECT TRIALS WITH ACTUAL TAPS
  
# read in file and change 1 column name  
  tapping_data = read.table(paste(rawDataFolder, Experiment, "_TapsRaw_INPUT.txt", sep = ""),header = T, sep = "\t")      # 1 line = 1 tap, only TS trials
  setnames(tapping_data, old = 'Subject', new = 'Participant')
    
# only keep trials 1-100, only keep lines with an actual button press (1-4)
  tapping_data_2 = tapping_data[(tapping_data$Trial >= 1) & (tapping_data$Trial <= 100), ]
  tapping_data_2$Tap[(tapping_data_2$Button == '1')|(tapping_data_2$Button == '2')|(tapping_data_2$Button == '3')|(tapping_data_2$Button == '4')] = 1
  tapping_data_2 = subset(tapping_data_2, (tapping_data_2$Tap == 1))

### MERGE TAPPING DATA (ONLY TS CONDITION) WITH TRIAL INFO (ALL TRIALS) ###

# read in condition key
  Cond_Key = read.table(paste(dataFolder, Experiment, "_ConditionKeyAll.txt", sep = ""), header = T, sep = "\t")      # list with picture info
 
# merge trial info (all trials) and picture info with tapping_data (only trials with taps); tapping_data_3 should have more rows
  tapping_data_3 <- merge(tapping_data_2, Cond_Key, by.x = c("Participant", "Trial"), by.y = c("Participant", "Trial"), all = TRUE)
 
# remove practice pictures (31-35)
  tapping_data_4 = tapping_data_3[is.element(tapping_data_3$Display, c(6:30, 36:100)) , ]

# add and select variables
  tapping_data_4$RecTime = tapping_data_4$Time - tapping_data_4$Recstart    # tap time alligned with start recording
  tapping_data_4$PrevRT = tapping_data_4$Time - tapping_data_4$Prev_time    # time between same button press
  tapping_data_4 = tapping_data_4[,(names(tapping_data_4) %in% c("Participant", "Trial", "Display", "Task", "PicGroup", "Button", "Tap", "RecTime", "PrevRT"))]   
 
  
# make sure variables are in the correct format
  tapping_data_4[,c("Participant", "Display", "Task", "Button")] %<>% lapply(function(x) as.character(x))  
  tapping_data_4[,c("Trial", "RecTime", "PrevRT")] %<>% lapply(function(x) as.numeric(as.character(x)))
  
# determine order of tasks and add info to dataset
  FirstCond <- (subset(tapping_data_4, (tapping_data_4$Trial == 6))) 
  FirstCond <- FirstCond[, c("Participant", "Task")]
  FirstCond <- unique(FirstCond)
  names(FirstCond) <- c("Participant", "FirstTask")
  tapping_data_5 = merge(tapping_data_4, FirstCond, by.x = c("Participant"), by.y = c("Participant"))  
 
  
  
### CHECK IF BUTTON PRESSES WERE CORRECT ###
 
# sort data by Subject, then Trial, then RecTime
  tapping_data_6 = tapping_data_5[order(tapping_data_5$Participant, tapping_data_5$Trial, tapping_data_5$RecTime),]
 
# write previous button + previous pp  
  tapping_data_6$PrevButton <- c(NA, tapping_data_6[1:(nrow(tapping_data_6)-1), "Button"])   # shift one cell down
  tapping_data_6$PrevPP <- c(0, tapping_data_6[1:(nrow(tapping_data_6)-1), "Participant"])
  tapping_data_6$PrevTask <- c(0, tapping_data_6[1:(nrow(tapping_data_6)-1), "Task"])
  
# only compare buttons pressed by same participant and in same condition
  tapping_data_6$PrevButton[(tapping_data_6$Participant != tapping_data_6$PrevPP)] = 0
  tapping_data_6$PrevButton[(tapping_data_6$Task != tapping_data_6$PrevTask)] = 0
  
# check tapping pattern (1324) based on button combinations (e.g if button = 1, previous button should be 4), mark taps with 1 (correct) or 0 (false)
  tapping_data_6$Corr = 0
  tapping_data_6$Corr[
    ((tapping_data_6$Button == 1) & (tapping_data_6$PrevButton == 4)) | 
      ((tapping_data_6$Button == 3) & (tapping_data_6$PrevButton == 1)) | 
      ((tapping_data_6$Button == 2) & (tapping_data_6$PrevButton == 3)) | 
      ((tapping_data_6$Button == 4) & (tapping_data_6$PrevButton == 2)) | 
      (tapping_data_6$PrevButton == 0)] = 1
  
  
# first tap op pp/TS condition correct
# no tapping in SO condition
  tapping_data_6$Corr[tapping_data_6$Task == 'SO'] = "NA"



### FILTER TAPS ###    
  
# select TS/SO condition
  tapping_data_TS <- subset(tapping_data_6, tapping_data_6$Task == "TS")
  tapping_data_SO <- subset(tapping_data_6, tapping_data_6$Task == "SO")
  
# check 400ms criterium (3000 could be different number, only to zoom in on first part of the data)
  #jpeg(file = paste(plotFolder, "/03_Taps_repetions_all.jpeg", sep = ""))
  plotAllTaps <- plot(density(x = tapping_data_TS$PrevRT[(tapping_data_TS$PrevRT < 3000) & (tapping_data_TS$PrevRT > 0)] , adjust = 0.001, na.rm = TRUE))
  #dev.off()
  
# keep only the "valid" taps (i.e., taps that were repeated too fast/too slow  are removed)+ calculate proportion taps removed
  #tapping_data_TS$PrevRT[tapping_data_TS$PrevRT == NA] = 99999
  
  tapping_data_TS_2 = subset(tapping_data_TS, (tapping_data_TS$PrevRT < 13000 & tapping_data_TS$PrevRT > 400))
  print(paste(Experiment, ": ",round((1-nrow(tapping_data_TS_2)/nrow(tapping_data_TS))*100,1), "% invalid taps removed", sep = ""))
  
# remove incorrect taps + calculate proportion remaining taps
  tapping_data_TS_3 = tapping_data_TS_2[(tapping_data_TS_2$Corr == 1), ]
  Taps_RepOK_Corr = (nrow(tapping_data_TS_3)/nrow(tapping_data_TS_2))*100
  print(paste(Experiment, ": ", round(100-Taps_RepOK_Corr,1), "% incorrect taps removed", sep = ""))

# remove taps (timing individual tap) outside of recording window + calculate proportion remaining taps
  tapping_data_TS_4 = tapping_data_TS_3[(tapping_data_TS_3$RecTime >= 0) & (tapping_data_TS_3$RecTime < 13000), ]
  Taps_OKwithinWindow = (nrow(tapping_data_TS_4) / nrow(tapping_data_TS_3))*100  

# total proportion taps removed
  #Taps_totalInvalid = (1- nrow(tapping_data_TS_4)/nrow(tapping_data_TS))*100

# new distribution:
  #jpeg(file = paste(plotFolder, "/03_Taps_repetions_valid.jpeg", sep = ""))
 lines(density(x = tapping_data_TS_4$PrevRT, adjust = 0.001, na.rm = TRUE), col = 'red')
  #dev.off()
  
# combine selected TS trials again with SO trials and sort again by trial. 
  #tapping_data_7 = rbind(tapping_data_TS_4, tapping_data_SO)
 tapping_data_7 = tapping_data_TS_4[c("Participant", "Trial", "Display", "Task", "PicGroup", "Tap", "RecTime")]
 tapping_data_7 = tapping_data_7[order(tapping_data_7$Participant, tapping_data_7$Trial, tapping_data_7$RecTime),]
  
  write.table(tapping_data_7, file = paste(analysisFolder, "02_Datafiles/", Experiment, "_TapReport.txt", sep = ""), append = FALSE, quote = FALSE, row.names = FALSE, sep = "\t", na = "NA")
  

  #return(tapping_data_7)
  
}
