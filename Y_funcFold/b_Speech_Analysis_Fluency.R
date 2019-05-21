b_Speech_Analysis_Fluency  <- function(speech_data_2) {
  
  #speech_data_2 = SpeechDataTables
  
  textfile = paste(analysisFolder, "03_Output/b_SpeechAnalyses.txt", sep = "") 
  write("File with Speech behavior analyses",file=textfile,append=FALSE)
  
  
  speech_data_2$HesTest = 0
  speech_data_2$HesTest[grepl(pattern = "h", speech_data_2$S2correct)] = 1
  line = print("\nperceptage of trials with hesitations:")
  write(line,file=textfile,append=TRUE)
  line = print(round(with(speech_data_2, tapply(HesTest, list(Exp, Task), mean))*100, 2))
  write.table(line,file=textfile,append=TRUE, sep = "\t")
  
  speech_data_2$WrongTest = 0
  speech_data_2$WrongTest[grepl(pattern = "w", speech_data_2$S2correct)] = 1
  line = print("\nperceptage of trials with wrong noun:")
  write(line,file=textfile,append=TRUE)
  line = print(round(with(speech_data_2, tapply(WrongTest, list(Exp, Task), mean))*100, 2))
  write.table(line,file=textfile,append=TRUE, sep = "\t")
  
  speech_data_2$TimeOutTest = 0
  speech_data_2$TimeOutTest[grepl(pattern = "w", speech_data_2$S2correct)] = 1
  line = print("\nperceptage of trials with a timeout:")
  write(line,file=textfile,append=TRUE)
  line = print(round(with(speech_data_2, tapply(TimeOutTest, list(Exp, Task), mean))*100, 2))
  write.table(line,file=textfile,append=TRUE, sep = "\t")
  
  speech_data_2$FluentTest = 0
  speech_data_2$FluentTest[grepl(pattern = "g", speech_data_2$S2correct)] = 1
  line = print("\nperceptage of trials with a fluent response:")
  write(line,file=textfile,append=TRUE)
  line = print(round(with(speech_data_2, tapply(FluentTest, list(Exp, Task), mean))*100, 2))
  write.table(line,file=textfile,append=TRUE, sep = "\t")

  # then the analysis part:
  #speech_data_2$ExpCen[speech_data_2$Exp == "Exp2"] = as.numeric(-1)
  #speech_data_2$ExpCen[speech_data_2$Exp == "Exp3"] = as.numeric(1)
  #speech_data_2$TaskCen[speech_data_2$Task == "SO"] = as.numeric(-1)
  #speech_data_2$TaskCen[speech_data_2$Task == "TS"] = as.numeric(1)
  #speech_data_2[, c("Participant", "Display", "Task", "Exp", "FirstTask")] %<>% lapply(function(x) as.factor(as.character(x)))
  #Fluency_model_full = glmer(FluentTest ~ ExpCen*TaskCen + (1+TaskCen||Participant) + (1|Display), data = speech_data_2, family = binomial)
  #Fluency_model_2main = glmer(FluentTest ~ ExpCen + TaskCen  +(1+TaskCen||Participant) + (1|Display), data = speech_data_2, family = binomial)
  #Fluency_model_Task = glmer(FluentTest ~  TaskCen + (1+TaskCen||Participant) + (1|Display), data = speech_data_2, family = binomial)
  
  
  Fluency_model_full = glmer(FluentTest ~ Exp*Task + TaskOrder + (1+Task|Participant) + (1|Display), data = speech_data_2, family = binomial)
  Fluency_model_noTO = glmer(FluentTest ~ Exp*Task + (1+Task|Participant) + (1|Display), data = speech_data_2, family = binomial)
  Fluency_model_3main = glmer(FluentTest ~ Exp + Task + TaskOrder + (1+Task|Participant) + (1|Display), data = speech_data_2, family = binomial)
  Fluency_model_2main= glmer(FluentTest ~ Exp + Task + (1+Task|Participant) + (1|Display), data = speech_data_2, family = binomial)
  
  anova(Fluency_model_full, Fluency_model_noTO, Fluency_model_3main, Fluency_model_2main)
  
  #summary(Fluency_model_full)
  summary(Fluency_model_3main)
  plot(effect('TaskOrder', Fluency_model_3main))
  plot(effect('Task', Fluency_model_3main))
  
  summaryBy(FluentTest ~ Task + TaskOrder, data = speech_data_2, FUN = c(mean, sd))
  
  
  write("\n########## Output Optimal Model after model comparison ###########",file=textfile,append=TRUE)
  capture.output(summary(Fluency_model_3main), file = textfile, append = TRUE) 
  
  
  
  
  
  
# EXTRA: interaction Task*Task is not signficiant and didn't make a difference in model comparison (so effect of TaskOrder is difference in fluency between the two groups,   not because of change in fluency over time)
  #Fluency_model_full_extra = glmer(FluentTest ~ Exp*Task + TaskOrder*Exp + (1+Task|Participant) + (1|Display), data = speech_data_2, family = binomial)
  #summary(Fluency_model_full_extra)
  #anova(Fluency_model_full_extra, Fluency_model_full)
  
 

  
 
  
 
 }