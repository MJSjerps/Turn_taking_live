b_Speech_Analysis_TurnsAndRates  <- function(speech_data_2) {

  #speech_data_2 = SpeechDataTables
  
  ## parameters:
  RespTypesToKeep = c("h", "g")
  sd_cutoff = 2.5
  
  #speech_data_2$ExpCen[speech_data_2$Exp == "Exp2"] = as.numeric(-1)
  #speech_data_2$ExpCen[speech_data_2$Exp == "Exp3"] = as.numeric(1)
  #speech_data_2$TaskCen[speech_data_2$Task == "SO"] = as.numeric(-1)
  #speech_data_2$TaskCen[speech_data_2$Task == "TS"] = as.numeric(1)

  textfile = paste(analysisFolder, "03_Output/b_SpeechAnalyses_TurnsAndRates.txt", sep = "") 
  write("File with Speech behavior analyses",file=textfile,append=FALSE)
  
  ## first exclude incorrect responses 
  write("\n########## Data exclusions: ###########",file=textfile,append=TRUE)
  speech_data_3 = speech_data_2[is.element(speech_data_2$S2correct, RespTypesToKeep), ]
  write("Experiment 2 incorrect responses",file=textfile,append=TRUE)
  line = round(1- nrow(speech_data_3[speech_data_3$Exp == "Exp2",])/nrow(speech_data_2[speech_data_2$Exp == "Exp2",]), 4)*100
  write(line,file=textfile,append=TRUE)
  write("Experiment 3 incorrect responses",file=textfile,append=TRUE)
  line = round(1- nrow(speech_data_3[speech_data_3$Exp == "Exp3",])/nrow(speech_data_2[speech_data_2$Exp == "Exp3",]), 4)*100
  write(line,file=textfile,append=TRUE)
  
  ################### Analysis of turns: #####################
  write(paste("\n################### Analysis of turns ###################", sd_cutoff, " SD"),file=textfile,append=TRUE)
  # first exclude turns that are more than 2.5sd away from the mean
  write(paste("\nExperiment 2 turns not between ", sd_cutoff, " SD"),file=textfile,append=TRUE)
  Exp2Dat = speech_data_3[abs(scale(speech_data_3$TurnGap[speech_data_3$Exp == "Exp2"]))<sd_cutoff, ]
  line = round(1-nrow(Exp2Dat[Exp2Dat$Exp == "Exp2",])/nrow(speech_data_3[speech_data_3$Exp == "Exp2",]), 4)*100
  write(line,file=textfile,append=TRUE)
  write(paste("\nExperiment 3 turns not between ", sd_cutoff, " SD"),file=textfile,append=TRUE)
  Exp3Dat = speech_data_3[abs(scale(speech_data_3$TurnGap[speech_data_3$Exp == "Exp3"]))<sd_cutoff, ]
  line = round(1-nrow(Exp3Dat[Exp3Dat$Exp == "Exp3",])/nrow(speech_data_3[speech_data_3$Exp == "Exp3",]), 4)*100
  write(line,file=textfile,append=TRUE)
  
  speech_data_4 = rbind(Exp2Dat, Exp3Dat)
  with(speech_data_4, tapply(TurnGap, list(Exp, Task), mean))
  
  
  
  # then the analysis part:
  #Turns_model_full = lmer(TurnGap ~ ExpCen*TaskCen  +(1+TaskCen||Participant) + (1|Display), data = speech_data_4)
  #Turns_model_2main = lmer(TurnGap ~ ExpCen + TaskCen  +(1+TaskCen||Participant) + (1|Display), data = speech_data_4)
  #Turns_model_Task = lmer(TurnGap ~  TaskCen + (1+TaskCen||Participant) + (1|Display), data = speech_data_4)
  #anova(Turns_model_full, Turns_model_2main, Turns_model_Task)
  
  
  Turns_model_full = lmer(TurnGap ~ Exp*Task + TaskOrder   +(1+Task|Participant) + (1|Display), data = speech_data_4)
  Turns_model_noTO = lmer(TurnGap ~ Exp*Task  +(1+Task|Participant) + (1|Display), data = speech_data_4)
  Turns_model_2main = lmer(TurnGap ~ Exp+Task  +(1+Task|Participant) + (1|Display), data = speech_data_4)
  Turns_model_3main = lmer(TurnGap ~ Exp + Task + TaskOrder +(1+Task|Participant) + (1|Display), data = speech_data_4)
  
  
  anova(Turns_model_full, Turns_model_noTO, Turns_model_2main, Turns_model_3main)

  summary(Turns_model_full)
  
  plot(effect('Task', Turns_model_full))
  plot(effect('Exp*Task', Turns_model_full))
  plot(effect('Exp', Turns_model_full))
  
 
  
  
  
  write("\n########## Output Optimal Model after model comparison) ###########",file=textfile,append=TRUE)
  capture.output(summary(Turns_model_full), file = textfile, append = TRUE) 
  
  write("\n APPROXIMATED P-VALUES:",file=textfile,append=TRUE)
  tVals <- fixef(Turns_model_full) / sqrt(diag(vcov(Turns_model_full)))
  pVals <- 2*(1-pnorm(abs(tVals)))
  write.table(round(rbind(tVals, pVals),3),file=textfile,append=TRUE, sep = "\t")

  


  
  
 ################### Analysis of durations: #####################
  write(paste("\n################### Analysis of Sp2 turn durations: ###################", sd_cutoff, " SD"),file=textfile,append=TRUE)
  # first exclude turn durations that are more than 2.5sd away from the mean
  write(paste("\nExperiment 2 Sp2 turn durations not between ", sd_cutoff, " SD"),file=textfile,append=TRUE)
  Exp2DurDat = speech_data_3[abs(scale(speech_data_3$Sp2Dur[speech_data_3$Exp == "Exp2"]))<sd_cutoff, ]
  line = round(1-nrow(Exp2DurDat[Exp2DurDat$Exp == "Exp2",])/nrow(speech_data_3[speech_data_3$Exp == "Exp2",]), 4)*100
  write(line,file=textfile,append=TRUE)
  write(paste("\nExperiment 3 Sp2 turn durations not between ", sd_cutoff, " SD"),file=textfile,append=TRUE)
  Exp3DurDat = speech_data_3[abs(scale(speech_data_3$Sp2Dur[speech_data_3$Exp == "Exp3"]))<sd_cutoff, ]
  line = round(1-nrow(Exp3DurDat[Exp3DurDat$Exp == "Exp3",])/nrow(speech_data_3[speech_data_3$Exp == "Exp3",]), 4)*100
  write(line,file=textfile,append=TRUE)
  
  speech_data_5 = rbind(Exp2DurDat, Exp3DurDat)
  with(speech_data_5, tapply(Sp2Dur, list(Exp, Task), mean))
  #with(speech_data_5, tapply(Sp2Dur, list(Exp), mean))
  #with(speech_data_5, tapply(Sp2Dur, list(Task), mean))
  #with(speech_data_5, tapply(Sp2Dur, list(Control), mean))
  
  
  # then the analysis part:
  #S2TurnDur_model_full = lmer(Sp2Dur ~ ExpCen*TaskCen  +(1+TaskCen||Participant) + (1|Display), data = speech_data_5)
  #S2TurnDur_model_2main = lmer(Sp2Dur ~ ExpCen + TaskCen  +(1+TaskCen||Participant) + (1|Display), data = speech_data_5)
  #S2TurnDur_model_Task = lmer(Sp2Dur ~  TaskCen + (1+TaskCen||Participant) + (1|Display), data = speech_data_5)
  #anova(S2TurnDur_model_full, S2TurnDur_model_2main, S2TurnDur_model_Task)
  
  
  
  S2TurnDur_model_full = lmer(Sp2Dur ~ Exp*Task + TaskOrder   +(1+Task|Participant) + (1|Display), data = speech_data_5)
  S2TurnDur_model_noTO = lmer(Sp2Dur ~ Exp*Task  +(1+Task|Participant) + (1|Display), data = speech_data_5)
  S2TurnDur_model_2main = lmer(Sp2Dur ~ Exp+Task  +(1+Task|Participant) + (1|Display), data = speech_data_5)
  S2TurnDur_model_3main = lmer(Sp2Dur ~ Exp + Task + TaskOrder +(1+Task|Participant) + (1|Display), data = speech_data_5)
  
  anova(S2TurnDur_model_full, S2TurnDur_model_noTO, S2TurnDur_model_2main, S2TurnDur_model_3main)
  summary(S2TurnDur_model_noTO)
  
  
  plot(effect('Exp*Task', S2TurnDur_model_noTO))
  plot(effect('Task', S2TurnDur_model_noTO))
  plot(effect('Exp', S2TurnDur_model_noTO))
  
  
  summaryBy(Sp2Dur ~ Exp + Task, data = speech_data_5, FUN = c(mean, sd))
  
  
  write("\n########## model output optimal model after model comparision ###########",file=textfile,append=TRUE)
  capture.output(summary(S2TurnDur_model_noTO), file = textfile, append = TRUE) 
  
  write("\n APPROXIMATED P-VALUES:",file=textfile,append=TRUE)
  tVals <- fixef(S2TurnDur_model_noTO) / sqrt(diag(vcov(S2TurnDur_model_noTO)))
  pVals <- 2*(1-pnorm(abs(tVals)))
  write.table(round(rbind(tVals, pVals),3),file=textfile,append=TRUE, sep = "\t")
  
}