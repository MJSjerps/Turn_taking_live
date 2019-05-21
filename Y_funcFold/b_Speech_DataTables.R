b_Speech_DataTables <- function() {

speech_data_Exp2 = read.table(paste(analysisFolder, "01_PreProcInput/Exp2/Exp2_SpeechData_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
Exp2_CondKey = read.table(paste(analysisFolder, "02_Datafiles/Exp2_ConditionKeyAll.txt", sep = ""), header = TRUE, sep = "\t")
Exp2_CondKey = Exp2_CondKey[c("Participant", "Display", "Task")]
speech_data_Exp2_2 = merge(speech_data_Exp2, Exp2_CondKey, by.x = c("Participant", "Display"), by.y= c("Participant", "Display") )
speech_data_Exp2_2$Exp = as.factor('Exp2')

# add task order for extra analysis
TaskOrder_Exp2 = read.table(paste(analysisFolder, "01_PreProcInput/Exp2/Exp2_TaskOrder_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
speech_data_Exp2_2 = merge(speech_data_Exp2_2, TaskOrder_Exp2, by.x = "Participant", by.y = "Participant")



speech_data_Exp3 = read.table(paste(analysisFolder, "01_PreProcInput/Exp3/Exp3_SpeechData_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
Exp3_CondKey = read.table(paste(analysisFolder, "02_Datafiles/Exp3_ConditionKeyAll.txt", sep = ""), header = TRUE, sep = "\t")
Exp3_CondKey = Exp3_CondKey[c("Participant", "Display", "Task")]
speech_data_Exp3_2 = merge(speech_data_Exp3, Exp3_CondKey, by.x = c("Participant", "Display"), by.y= c("Participant", "Display") )
speech_data_Exp3_2$Exp = as.factor('Exp3')

# add task order for extra analysis
TaskOrder_Exp3 = read.table(paste(analysisFolder, "01_PreProcInput/Exp3/Exp3_TaskOrder_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
speech_data_Exp3_2 = merge(speech_data_Exp3_2, TaskOrder_Exp3, by.x = "Participant", by.y = "Participant")


speech_data = rbind(speech_data_Exp2_2, speech_data_Exp3_2)



return(speech_data)
}