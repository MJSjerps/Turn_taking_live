a_mergeConditionKeys <- function(Experiment) {
  

  ##### Local environment:
  rawDataFolder = paste(analysisFolder, "01_PreProcInput/", Experiment, "/", sep = "")
 
  ##### merge files:
  
  PicGroups = read.table(paste(rawDataFolder, Experiment, "_PicGroups_INPUT.txt", sep = ""), header = T, sep = "\t")      # list with picture info
 
  Cond_Key1 = read.table(paste(rawDataFolder, Experiment, "_ConditionKey_INPUT.txt", sep = ""), header = T, sep = "\t")    # trial info; 1 line = 1 trial
  setnames(Cond_Key1, old = 'Subject', new = 'Participant')
  Cond_Key1$Display = gsub(".wav", "", Cond_Key1$Recname)    
  Cond_Key2 = merge(Cond_Key1, PicGroups, by.x = c("Display"), by.y = c("Display"))
  
  
  ### To obtain the Task condition, I need a not-so-elegant workaround: I load the tapping data and figure out which "PicGroup" is associated with taps
  # read in file  
  tapping_data = read.table(paste(rawDataFolder, Experiment, "_TapsRaw_INPUT.txt", sep = ""),header = T, sep = "\t")      # 1 line = 1 tap, only TS trials
  setnames(tapping_data, old = 'Subject', new = 'Participant')
  tapping_data_2 = tapping_data[(tapping_data$Trial >= 1) & (tapping_data$Trial <= 100), ] # for some reason there is a trial 0 in the logfile
  tapping_data_2$Tap[(tapping_data_2$Button == '1')|(tapping_data_2$Button == '2')|(tapping_data_2$Button == '3')|(tapping_data_2$Button == '4')] = 1
  tapping_data_3 = subset(tapping_data_2, (tapping_data_2$Tap == 1))
  tapping_data_4 <- merge(tapping_data_3, Cond_Key2, by.x = c("Participant", "Trial"), by.y = c("Participant", "Trial"), all = TRUE)
  
  # create overview of  taps/picture set/participant
  Tap_trials1 <- data.frame(table(tapping_data_4$Participant, tapping_data_4$PicGroup, tapping_data_4$Tap))
  colnames(Tap_trials1) = list("Participant", "PicGroup", "Tap", "Nobserv")
  Tap_trials2 <- subset(Tap_trials1, (Tap_trials1$PicGroup != "C"))
  Tap_trials2$Task = "SO"
  Tap_trials2$Task[Tap_trials2$Nobserv > 100] = "TS" # I don't use 0 because participants may have accidentally pressed a button
  Tap_trials3 = Tap_trials2[c("Participant", "PicGroup", "Task")]
  
  ### and merge the now obtained column with the condition key
  Cond_Key3 = merge(Cond_Key2, Tap_trials3, by.x = c("Participant", "PicGroup"), by.y = c("Participant", "PicGroup"))
  
  Cond_Key4 <- Cond_Key3[c("Participant", "Trial", "Display", "PicGroup", "Task", "Trialstart", "Recstart", "Scenestart")]
  
  write.table(Cond_Key4, file = paste(analysisFolder, "02_Datafiles/", Experiment, "_ConditionKeyAll.txt", sep = ""), append = FALSE, quote = FALSE, row.names = FALSE, sep = "\t", na = "NA")
  
  
## un-hash to check numbers
#Cond_Key4$Control = 1
#a = (with(Cond_Key4, tapply(Control, list(Display, Participant), sum)))
  
  
}
