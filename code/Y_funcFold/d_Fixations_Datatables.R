d_Fixations_Datatables <- function(align_flag, RespTypesToKeep, Experiment) {
  
  ## The first output (Samp_Report_Aligned) is a by-participant and by-Display fixation table (for the analyses) 
  # read in data
  Samp_Report = read.table(paste(analysisFolder, "02_Datafiles/", Experiment, "_SampReport.txt", sep = ""), header = T, sep = "")
  Samp_Report_Aligned = d_AlignAndCodeSampReport(Samp_Report, align_flag, Experiment) # 
  Samp_Report_Aligned$Exp = Experiment
  Samp_Report_Aligned = Samp_Report_Aligned[is.element(Samp_Report_Aligned$S2correct, RespTypesToKeep), ]
  
  # make a dummy-coded matrix of looks to objects
  Samp_Report_Dummy = model.matrix(~IntAr -1, data = Samp_Report_Aligned)
  colnames(Samp_Report_Dummy) <- gsub("IntAr", "", colnames(Samp_Report_Dummy))
  Samp_Report_Aligned$Sp1_Sum = rowSums(Samp_Report_Dummy[,c("T_A_1", "T_A_2", "T_O_1", "T_O_2", "T_O_3", "T_O_4")]) 
  Samp_Report_Aligned$Sp2_Sum = rowSums(Samp_Report_Dummy[,c("B_A_1", "B_A_2", "B_O_1", "B_O_2", "B_O_3", "B_O_4")]) 
  Samp_Report_Aligned$FixPref_All = 0
  Samp_Report_Aligned$FixPref_All[Samp_Report_Aligned$Sp1_Sum == 1] = 1
  Samp_Report_Aligned$FixPref_All[Samp_Report_Aligned$Sp2_Sum == 1] = -1
  
  
  ## The second output (Fixations_TrialParts) is a by-participant and by-display report that indicates the proportion of looks to both speakers objects but discretized by art (confederate part or participant part)
  Fixations_TrialParts = d_AlignAndCodeSampReport(Samp_Report, "RecOnset", Experiment) # 
  Fixations_TrialParts = Fixations_TrialParts[is.element(Fixations_TrialParts$S2correct, RespTypesToKeep), ]
  
  Fixations_TrialParts$Parts = "irrelevant"
  Fixations_TrialParts$Parts[(Fixations_TrialParts$Aligned_Gaze >= 4) & ((Fixations_TrialParts$Aligned_Gaze - Fixations_TrialParts$S1W4e) < 0) ] = "Sp1_Part"
  Fixations_TrialParts$Parts[((Fixations_TrialParts$Aligned_Gaze - Fixations_TrialParts$S1W4e) >= 0) & ((Fixations_TrialParts$Aligned_Gaze - Fixations_TrialParts$S2W4e) < 0) ] = "Sp2_Part"
  
  # make a dummy-coded matrix of looks to objects
  Samp_Report_Dummy2 = model.matrix(~IntAr -1, data = Fixations_TrialParts)
  colnames(Samp_Report_Dummy2) <- gsub("IntAr", "", colnames(Samp_Report_Dummy2))
  Fixations_TrialParts$Sp1_Sum = rowSums(Samp_Report_Dummy2[,c("T_A_1", "T_A_2", "T_O_1", "T_O_2", "T_O_3", "T_O_4")]) 
  Fixations_TrialParts$Sp2_Sum = rowSums(Samp_Report_Dummy2[,c("B_A_1", "B_A_2", "B_O_1", "B_O_2", "B_O_3", "B_O_4")]) 
  
  Fixations_AggTrialParts_A = aggregate(Fixations_TrialParts[,c("Sp1_Sum", "Sp2_Sum")],
    by = list(Parts = Fixations_TrialParts$Parts, Participant = Fixations_TrialParts$Participant, Task = Fixations_TrialParts$Task, Display = Fixations_TrialParts$Display),
    FUN = mean)
  Fixations_AggTrialParts = Fixations_AggTrialParts_A[is.element(Fixations_AggTrialParts_A$Parts, c("Sp1_Part", "Sp2_Part")), ]
  Fixations_AggTrialParts$Exp = Experiment
  
  ## The third output (SampRep_Agg) contains a sample report thats aggregated over displays and participants, but which does display the by-participant SE (for visualization)
  # aggregate per participant:
  SampRep_AggPart = aggregate(Samp_Report_Aligned[,c("Sp1_Sum", "Sp2_Sum", "FixPref_All")],
    by = list(Participant = Samp_Report_Aligned$Participant, Task = Samp_Report_Aligned$Task, Aligned_Gaze = Samp_Report_Aligned$Aligned_Gaze),
    FUN = mean)
  
  # obtain SE
  SampRep_SD = aggregate(SampRep_AggPart[,c("Sp1_Sum", "Sp2_Sum", "FixPref_All")],
    by = list(Task = SampRep_AggPart$Task, Aligned_Gaze = SampRep_AggPart$Aligned_Gaze),
    FUN = sd)
  
  nSubj = length(unique(SampRep_AggPart$Participant))
  SampRep_SD$Sp1_Sum_SE = SampRep_SD$Sp1_Sum/sqrt(nSubj)
  SampRep_SD$Sp2_Sum_SE = SampRep_SD$Sp2_Sum/sqrt(nSubj)
  SampRep_SD$FixPref_All_SE = SampRep_SD$FixPref_All/sqrt(nSubj)
  SampRep_SD$Sp1_Sum_SE[is.na(SampRep_SD$Sp1_Sum_SE)] = 0
  SampRep_SD$Sp2_Sum_SE[is.na(SampRep_SD$Sp2_Sum_SE)] = 0
  SampRep_SD$FixPref_All_SE[is.na(SampRep_SD$FixPref_All_SE)] = 0
  SampRep_SE = SampRep_SD[c("Task", "Aligned_Gaze", "Sp1_Sum_SE", "Sp2_Sum_SE", "FixPref_All_SE")]
  
  # overall mean SE
  SampRep_Mean = aggregate(SampRep_AggPart[,c("Sp1_Sum", "Sp2_Sum", "FixPref_All")],
    by = list(Task = SampRep_AggPart$Task, Aligned_Gaze = SampRep_AggPart$Aligned_Gaze),
    FUN = mean)
  
  

  ### merge: fixation proportions
  SampRep_Agg = merge(SampRep_Mean, SampRep_SE, by.x = c("Task", "Aligned_Gaze"), by.y = c("Task", "Aligned_Gaze"))
  SampRep_Agg$Aligned_Gaze = as.numeric(as.character(SampRep_Agg$Aligned_Gaze))
  # add preferences:
  SampRep_Agg = SampRep_Agg[order(c(SampRep_Agg$Aligned_Gaze)), ]
  SampRep_Agg$Exp = Experiment
  
  # select only the relevant columns from the by-trial data.frame:
  Samp_Report_Aligned = Samp_Report_Aligned[c("Participant", "Display", "Task", "Aligned_Gaze", "Exp", "Sp1_Sum", "Sp2_Sum")]
  
  output = list(Samp_Report_Aligned, Fixations_AggTrialParts, SampRep_Agg)
  return(output)
}