a_GazeSampReport <- function(Experiment) {
 
  ## First set parameters for generating the sample report:
  frequency = 10 ## sample report frequency
  trial_samples = ((15)/(1/frequency)) # number of samples per trial (trial is 15 sec long)
  
  
# read in eye-tracking data alligned to speech
  Gaze_live = read.table(paste(analysisFolder, "02_Datafiles/", Experiment, "_GazeReport.txt", sep = ""), header = T, sep = "\t")
   
# only keep relevant AIO's (4 pictures and 1 arrows per speaker whole screen)  
  #Gaze_live_2 = Gaze_live[is.element(Gaze_live$IA_Label, c("B_A_1 ", "B_A_2 ", "B_O_1 ", "B_O_2 ", "B_O_3 ", "B_O_4 ", "T_A_1 ", "T_A_2 ", "T_O_1 ", "T_O_2 ", "T_O_3 ", "T_O_4 ", "Whole_Screen ", "Anticipation", "RECTANGLE_INTERESTAREA ")), ]
  Gaze_live_2 = Gaze_live

# make sure variables are in the correct format  
  Gaze_live_2[,c("Gaze_Start", "Gaze_End")] %<>% lapply(function(x) as.numeric(as.character(x)))  
  Gaze_live_2[,c("IA_Label", "Participant", "Display")] %<>% lapply(function(x) as.character(x))  
  
    

# create 'empty'dataset that will be filled in for loop
  Participant = rep("x", times = 0)
  Display = rep(0, times = 0)
  Sample = rep(0, times = 0)
  IntAr = rep("None", times = 0)
  Time = rep(0, times = 0)
  Data_Frame  <- data.frame(cbind(Participant, Display, Sample, IntAr, Time))
  
  DisplayList = sort(unique(Gaze_live_2$Display))
  ParticipantList = sort(unique(Gaze_live_2$Participant))
  
  print("Generating sample report:")
  
  for (l in ParticipantList)
    #for (l in c("11"))
  {
    print(l)
    
    fixations_temp = Gaze_live_2[Gaze_live_2$Participant == l, ]
    
    for (m in DisplayList)
      #for (m in c(65))
    {
      #print(m)
      
      fixations_temp2 = fixations_temp[fixations_temp$Display == m, ]
      
      Participant = rep(l, times = trial_samples)
      Display = rep(m, times = trial_samples)
      Sample = rep(c(1:trial_samples))
      temp_data  <- data.frame(cbind(Participant, Display, Sample))
      temp_data$IntAr = "None"
      
      temp_data$Sample = as.numeric(as.character(temp_data$Sample))
      temp_data$Display = as.character(temp_data$Display)
      temp_data$Participant = as.character(temp_data$Participant)
      temp_data$Time = (temp_data$Sample * ((1/frequency)*1000))
      
      for (n in c(1:trial_samples))
        #for (n in c(1:60))
      {
        #n = 63
        Test_Name = (temp_data$Participant[temp_data$Sample == n])
        Test_Display = (temp_data$Display[temp_data$Sample == n])
        Test_Sample = temp_data$Sample[temp_data$Sample == n]
        Test_Time = (temp_data$Time[temp_data$Sample == n])
        
        #print("----------------")
        Test_AOI_2 = "None"
        #print(Test_AOI_2)
        
        #Test_AOI = fixations_raw_2$CURRENT_FIX_INTEREST_AREA_LABEL[((fixations_raw_2$Subject == Test_Name) & (fixations_raw_2$Scene == Test_Scene) & ((fixations_raw_2$CURRENT_FIX_START < Test_Time) & (fixations_raw_2$RUN_END > Test_Time)))]
        Test_AOI = fixations_temp2$IA_Label[((fixations_temp2$Participant == Test_Name) & (fixations_temp2$Display == Test_Display) & ((fixations_temp2$Gaze_Start < Test_Time) & (fixations_temp2$Gaze_End > Test_Time)))]
        
        #print(Test_Sample)
        #print(Test_Time)
        #print(Test_AOI)
        
        if (length (Test_AOI) == 1)
        {
          Test_AOI_2 = Test_AOI
        }
        if (length (Test_AOI) > 1)
        {
          Test_AOI = Test_AOI[Test_AOI != "Whole_Screen "]
          Test_AOI_2 = Test_AOI
          if (length (Test_AOI) > 1)
          {
            print(l)
            print(m) 
            print(n)
            print(Test_AOI)
            print("---")
          }
        }
        
        
        temp_data$IntAr[temp_data$Sample == n] = Test_AOI_2
        
      }
      Data_Frame = rbind(Data_Frame, temp_data)
    }
  }
  
  
  print(unique(Data_Frame$IntAr))
  count = 1: nrow(Data_Frame) 
  Data_Frame$count = count
  
 
  
  # merge with condition information
  Cond_key = read.table(paste(analysisFolder, "02_Datafiles/", Experiment, "_ConditionKeyAll.txt", sep = ""), header = T, sep = "\t")
  SampReport <- merge(Cond_key, Data_Frame,  by.x = c("Participant", "Display"), by.y = c("Participant", "Display"))
  SampReport = SampReport[order(SampReport$count), ]
  
  write.table(Data_Frame, file = paste(analysisFolder, "02_Datafiles/", Experiment, "_SampReport.txt", sep = ""), append = FALSE, quote = FALSE, row.names = FALSE, sep = "\t", na = "")
  


}