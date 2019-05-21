d__EyeTracking_Main <-function() {
  
  # set parameters:
  align_flag = "ConfOffset" # Options: "ParticOnset" "ConfOffset"
  RespTypesToKeep = c("h", "g") # keep fluent (g) and hesitated (h; which are ultimately correct) response types
  
  # Generating the input datatables:
  outputExp2 <- d_Fixations_Datatables(align_flag, RespTypesToKeep, "Exp2")
  outputExp3 <- d_Fixations_Datatables(align_flag, RespTypesToKeep, "Exp3")
  
  # for the analyses use the individual trials data
  Samp_Report_Aligned = rbind(outputExp2[[1]], outputExp3[[1]])
  Fixations_AggTrialParts = rbind(outputExp2[[2]], outputExp3[[2]])
  TurnPtsTable <- d_Fixations_Analyses(Samp_Report_Aligned, Fixations_AggTrialParts)
  
  # for plotting use the aggregated data
  SampRep_Agg = rbind(outputExp2[[3]], outputExp3[[3]])
  d_Fixations_Plotting(SampRep_Agg, TurnPtsTable)
  
}