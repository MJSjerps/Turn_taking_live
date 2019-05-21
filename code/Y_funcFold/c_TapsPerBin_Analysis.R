
c_TapsPerBin_Analysis <- function(TapAnalysisData, RespTypesToKeep) {

 #### BIN ANALYSIS ####
  TapAnalysisData = Bin_Effects_Table
  
  # set parameters:
  textfile = paste(analysisFolder, "03_Output/c_TappingAnalyses_Bins.txt", sep = "") 
  write("File with tapping analyses",file=textfile,append=FALSE)
  
  
  # add task order
  TaskOrder_Exp2 = read.table(paste(analysisFolder, "01_PreProcInput/Exp2/Exp2_TaskOrder_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
  TaskOrder_Exp3 = read.table(paste(analysisFolder, "01_PreProcInput/Exp3/Exp3_TaskOrder_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
  TaskOrder <- rbind(TaskOrder_Exp2, TaskOrder_Exp3)
  TapAnalysisData = merge(TapAnalysisData, TaskOrder, by.x = "Participant", by.y = "Participant")
  
  
  ### create the necessary datatables: 
  # only keep data with 0.5 taps/sec or more during baseline (select baseline, filter, merge again with dataset, calculate proportion removed): 
  RelBase_ID_1 = TapAnalysisData[TapAnalysisData$SpAlignedBins < -89, ]
  RelBase_ID_2 = RelBase_ID_1[RelBase_ID_1$Tap_Rate >= 0.5, ]  
  RelBase_ID_3 = RelBase_ID_2[,(names(RelBase_ID_2) %in% c("Participant", "Display", "Task", "Exp", "TaskOrder"))] # keep
  TapAnalysisData_2 <- merge(TapAnalysisData, RelBase_ID_3, by.x = c("Participant", "Display", "Task", "Exp", "TaskOrder"), by.y = c("Participant", "Display", "Task", "Exp", "TaskOrder"))
  1 - nrow(TapAnalysisData_2)/nrow(TapAnalysisData) ## proportion removed
  
  # remove trials with incorrect responses
  #TapAnalysisData_3 = TapAnalysisData_2[(TapAnalysisData_2$S2correct == "g" |TapAnalysisData_2$S2correct == "h"), ]
  TapAnalysisData_3 = TapAnalysisData_2[is.element(TapAnalysisData_2$S2correct, RespTypesToKeep), ]
  
  # restrict to specific steps:
  TapAnalysisData_4 = TapAnalysisData_3[is.element( TapAnalysisData_3$SpAlignedBins, c(-99, seq(-4, 4.5, 0.5))), ]
  
  #add centered predictor for "Experiment"
  #TapAnalysisData_4$ExpCen[TapAnalysisData_4$Exp == "Exp2"] = as.numeric(-1)
  #TapAnalysisData_4$ExpCen[TapAnalysisData_4$Exp == "Exp3"] = as.numeric(1)

  TapAnalysisData_4$Exp <- as.factor(as.character(TapAnalysisData_4$Exp))
  TapAnalysisData_4$SpAlignedBins <- as.factor(as.character(TapAnalysisData_4$SpAlignedBins))
  
  contrasts(TapAnalysisData_4$SpAlignedBins) <- contr.Sum

  TapAnalysisData_4$SpAlignedBins <- relevel (TapAnalysisData_4$SpAlignedBins, ref = "-99")
    
# check if there is an effect of time step on Tap Rate, save output to file
  #Taps_Bin_model_full = lmer(Tap_Rate ~ as.factor(SpAlignedBins) * ExpCen +(1|Participant) + (1|Display), data = TapAnalysisData_4)
  #Taps_Bin_model_2main = lmer(Tap_Rate ~ as.factor(SpAlignedBins) + ExpCen +(1|Participant) + (1|Display), data = TapAnalysisData_4)
  #Taps_Bin_model_time = lmer(Tap_Rate ~ as.factor(SpAlignedBins)  +(1|Participant) + (1|Display), data = TapAnalysisData_4)
  #anova(Taps_Bin_model_time, Taps_Bin_model_2main, Taps_Bin_model_full )
  #TapsBin_modelOutput <- summary(Taps_Bin_model_2main)
  
  
  Taps_Bin_model_full = lmer(Tap_Rate ~ SpAlignedBins * Exp +(1|Participant) + (1|Display), data = TapAnalysisData_4)
  summary(Taps_Bin_model_full)
  
  
  
  write("\n########## FULL MODEL ###########",file=textfile,append=TRUE)
  capture.output(summary(Taps_Bin_model_full), file = textfile, append = TRUE) 
  
  #write("\n########## 2 mains model (optimal) ###########",file=textfile,append=TRUE)
  %capture.output(summary(Taps_Bin_model_2main), file = textfile, append = TRUE) 
  
  
  write("\n APPROXIMATED P-VALUES:",file=textfile,append=TRUE)
  TapsBin_tValues <- fixef(Taps_Bin_model_full) / sqrt(diag(vcov(Taps_Bin_model_full)))
  TapsBin_pValues <- 2*(1-pnorm(abs(TapsBin_tValues)))
  print(round(TapsBin_pValues,4))
  write.table(round(TapsBin_pValues,4),file=textfile,append=TRUE, sep = "\t")
  
# Bonferroni corrected steps
  write("\n BONF. CORR. P-VALUES:",file=textfile,append=TRUE)
  bonf_p = 0.05/(length(unique(TapAnalysisData_4$SpAlignedBins))-1)
  write(bonf_p,file=textfile,append=TRUE)
  print(bonf_p)
  print(bonf_p > TapsBin_pValues)

 bonf_Pvals = data.frame(bonf_p > TapsBin_pValues)
 #write.table(bonf_Pvals, file = paste(outputfolder, "c_TapTimeStep_modelOutput_bonfPvals.txt", sep = ""))
 write.table(bonf_Pvals,file=textfile,append=TRUE, sep = "\t")
 
}








