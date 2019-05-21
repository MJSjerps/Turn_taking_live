c_TapsPerBin_Plotting <- function(TapPlotData, RespTypesToKeep) {

  #TapPlotData = Bin_Effects_Table

 # set parameters: 
  plotfolder = paste(analysisFolder, "03_Output/", sep = "") 
  
 
 ### create the necessary datatables: 
 # only keep data with 0.5 taps/sec or more during baseline (select baseline, filter, merge again with dataset, calculate proportion removed): 
  RelBase_ID_1 = TapPlotData[TapPlotData$SpAlignedBins < -89, ]
  RelBase_ID_2 = RelBase_ID_1[RelBase_ID_1$Tap_Rate >= 0.5, ]  
  RelBase_ID_3 = RelBase_ID_2[,(names(RelBase_ID_2) %in% c("Participant", "Display", "Task", "Exp"))] # keep
  TapPlotData_2 <- merge(TapPlotData, RelBase_ID_3, by.x = c("Participant", "Display", "Task", "Exp"), by.y = c("Participant", "Display", "Task", "Exp"))
  1 - nrow(TapPlotData_2)/nrow(TapPlotData) ## proportion removed
  
  # remove trials with incorrect responses
  #TapPlotData_3 = TapPlotData_2[(TapPlotData_2$S2correct == "g" |TapPlotData_2$S2correct == "h"), ]
  TapPlotData_3 = TapPlotData_2[is.element(TapPlotData_2$S2correct, RespTypesToKeep), ]
  
  # caluate mean tap rate per pp
  TapRate_Subj_Bin_Means = aggregate(TapPlotData_3$Tap_Rate, by = list(TapPlotData_3$Exp, TapPlotData_3$Task, TapPlotData_3$SpAlignedBins, TapPlotData_3$Participant), FUN = "mean", na.rm = TRUE )
  colnames(TapRate_Subj_Bin_Means) = c("Exp", "Task", "SpAlignedBins", "Participant", "Tap_Rate")
  
  # calculate mean overall
  TapRate_overall_Bin_Means = aggregate(TapRate_Subj_Bin_Means$Tap_Rate, by = list(TapRate_Subj_Bin_Means$Exp, TapRate_Subj_Bin_Means$Task, TapRate_Subj_Bin_Means$SpAlignedBins), FUN = "mean", na.rm = TRUE )
  colnames(TapRate_overall_Bin_Means) = c("Exp", "Task", "SpAlignedBins", "Tap_Rate")

  # compute SD and SE
  TapRate_Bin_SD = aggregate(TapRate_Subj_Bin_Means$Tap_Rate, by = list(TapRate_Subj_Bin_Means$Exp, TapRate_Subj_Bin_Means$Task, TapRate_Subj_Bin_Means$SpAlignedBins), FUN = "sd", na.rm = TRUE )
  colnames(TapRate_Bin_SD) = c("Exp", "Task", "SpAlignedBins", "Tap_RateSD")
  # convert SD to SE for both experiments
  TapRate_Bin_SD$Tap_RateSE = NA
  Exp2_nSubj = length(unique(TapRate_Subj_Bin_Means$Participant[TapRate_Subj_Bin_Means$Exp == "Exp2"]))
  TapRate_Bin_SD$Tap_RateSE[TapRate_Bin_SD$Exp == "Exp2"] = (TapRate_Bin_SD$Tap_RateSD[TapRate_Bin_SD$Exp == "Exp2"]/(sqrt(Exp2_nSubj)))
  Exp3_nSubj = length(unique(TapRate_Subj_Bin_Means$Participant[TapRate_Subj_Bin_Means$Exp == "Exp3"]))
  TapRate_Bin_SD$Tap_RateSE[TapRate_Bin_SD$Exp == "Exp3"] = (TapRate_Bin_SD$Tap_RateSD[TapRate_Bin_SD$Exp == "Exp3"]/(sqrt(Exp3_nSubj)))
  

### plot settings


  pdf(paste(plotfolder, "/c_TappingRatesToS1Offset.pdf", sep = ""),width=7,height=7)


  par(mfrow = c(2, 1), 	      # number of panels
  oma=c(1,1,1,1), 	      # outer margins: whole figure
  mar=c(3,3,1,2), 	      # inner margin: seperate panels order: bottom, left, top, right
  mgp =c(2,1,0))		      # position of the axes (text, ticks, line)



  c_TapsPerBin_PlotPanel("Exp2", TapRate_overall_Bin_Means, TapRate_Bin_SD, Exp2_nSubj)
  c_TapsPerBin_PlotPanel("Exp3", TapRate_overall_Bin_Means, TapRate_Bin_SD, Exp3_nSubj)


  dev.off()

}
