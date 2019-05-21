d_Fixations_Analyses <- function(Samp_Report_Aligned, Fixations_AggTrialParts) {
  ## This function generates the output for two analyses: 
  ## 1) an analysis of the turning points (when to gazes transtion from confederate objcts to participants' own objects)
  ## 2) an analysis of the proportions of looks per trial part (speaker 1 part, or participant part)
  
  ## GLOBAL: open a textfile to report the statistics
  textfile = paste(analysisFolder, "03_Output/d_FixationAnalyses.txt", sep = "") 
  write("File with fixation analyses",file=textfile,append=FALSE)
  
  ## ANALYSIS 1) The first analysis computes the turning point for each participant and tests whether the turning point differs depending on task and experiment.
  Samp_Report_Aligned_2 = Samp_Report_Aligned[Samp_Report_Aligned$Sp1_Sum == 1 | Samp_Report_Aligned$Sp2_Sum == 1, ]
  Samp_Report_Aligned_2[, c("Participant", "Display")] %<>% lapply(function(x) as.factor(as.character(x)))
  
  # discretize into 0.5-second timesteps & reduce range
  #Samp_Report_Aligned_2$Aligned_Gaze_Disc = round(Samp_Report_Aligned_2$Aligned_Gaze*2)/2
  #Samp_Report_Aligned_2 = Samp_Report_Aligned_2[(Samp_Report_Aligned_2$Aligned_Gaze_Disc> -4.0) & (Samp_Report_Aligned_2$Aligned_Gaze_Disc < 5.0), ]
  #Samp_Report_Aligned_2[, c("Participant", "Display", "Aligned_Gaze_Disc")] %<>% lapply(function(x) as.factor(as.character(x)))
  
  # compute turning point per participant and per condition within the -2 to 2 sec range:
  Samp_Report_TurnPt = Samp_Report_Aligned_2[(Samp_Report_Aligned_2$Aligned_Gaze> -2.1) & (Samp_Report_Aligned_2$Aligned_Gaze < 2.1), ]
  
  # predefine the to-be-filled turning points table
  TurnPtsMat <- matrix(NA, ncol = 4, nrow = length(unique(Samp_Report_Aligned_2$Participant))*2)
  TurnPtsTable <- data.frame(TurnPtsMat)
  colnames(TurnPtsTable) <- c("Participant", "Task", "Exp", "TransPt")
  
  # loop over participants and tasks and estimate transition point
  ParticList = unique(Samp_Report_Aligned_2$Participant)
  TaskList = c("TS", "SO")
  cnt = 1
  for (pp in ParticList)
  {
    for (tk in TaskList)
    {
      #pp = "157"
      #tk = "SO"
      print(pp)
      subset = Samp_Report_TurnPt[Samp_Report_TurnPt$Participant == pp & Samp_Report_TurnPt$Task == tk, ]
      subset$Control = 1
      #with(subset, tapply(Sp1_Sum, list(Aligned_Gaze), mean))
      
      PpTaskModel = glm(Sp1_Sum ~ Aligned_Gaze , family = binomial, data = subset)
      
      # use the model to predict a more continuous function that allows a precise estimate of the transition point 
      range <- data.frame(Aligned_Gaze= seq(from = -6, to = 6, by = 0.01))
      range$PredictedLogit = predict(PpTaskModel, range, family = binomial)
      range$Predicted <- exp(range$PredictedLogit) / (1 + exp(range$PredictedLogit))
      TransPt = range[which.min(abs(range$Predicted - 0.5)), ]
      
      #fill the table
      TurnPtsTable$Participant[cnt] = pp
      TurnPtsTable$Task[cnt] = tk
      TurnPtsTable$Exp[cnt] = unique(subset$Exp)
      TurnPtsTable$TransPt[cnt] = TransPt$Aligned_Gaze
      cnt = cnt+1
      
      ## plot observed and fitted functions:
      #plot(range$Aligned_Gaze, range$Predicted)
      #aggdata = aggregate(subset$Sp1_Sum, by = list(subset$Aligned_Gaze), FUN=mean, na.rm=TRUE)
      #lines(aggdata$Group.1, aggdata$x)
      #aggdata[which.min(abs(aggdata$x - 0.5)), ]
      }
  }
  
 #with(TurnPtsTable, tapply(TransPt, list(Task, Exp), mean))

 ## Run analysis of transition points
 # define centered predictor for Experiment
  #TurnPtsTable$ExpC = NaN
  #TurnPtsTable$ExpC[TurnPtsTable$Exp == "Exp2"] = -1
  #TurnPtsTable$ExpC[TurnPtsTable$Exp == "Exp3"] = 1
 # set correct class types
  #TurnPtsTable$ExpC = as.numeric(as.character( TurnPtsTable$ExpC))
  #TurnPtsTable$Task =  as.factor(as.character(TurnPtsTable$Task))
 # run model
  #TransitionModel = lm(TransPt ~ Task * ExpC ,  data = TurnPtsTable)
  #print(round(coef(summary(TransitionModel)), 2))
 #write("\n #### Transition-Time analyses ### ",file=textfile,append=TRUE)
 #capture.output(summary(TransitionModel), file = textfile, append = TRUE) 
 
 
 ## The second analysis assesses whether the proportion of looks to the speakers in the two parts differs from each other
 Fixations_AggTrialParts$Sp2Prop = Fixations_AggTrialParts$Sp1_Sum/(Fixations_AggTrialParts$Sp1_Sum + Fixations_AggTrialParts$Sp2_Sum)
 
 ## Run analysis of fixation preferences
 # define centered predictor for Experiment
  #Fixations_AggTrialParts$ExpC = NaN
  #Fixations_AggTrialParts$ExpC[Fixations_AggTrialParts$Exp == "Exp2"] = -1
  #Fixations_AggTrialParts$ExpC[Fixations_AggTrialParts$Exp == "Exp3"] = 1
 # set correct class types
  #Fixations_AggTrialParts$ExpC = as.numeric(as.character(Fixations_AggTrialParts$ExpC))
  
 
 
 # add task order
 TaskOrder_Exp2 = read.table(paste(analysisFolder, "01_PreProcInput/Exp2/Exp2_TaskOrder_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
 TaskOrder_Exp3 = read.table(paste(analysisFolder, "01_PreProcInput/Exp3/Exp3_TaskOrder_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
 TaskOrder <- rbind(TaskOrder_Exp2, TaskOrder_Exp3)
 Fixations_AggTrialParts = merge(Fixations_AggTrialParts, TaskOrder, by.x = "Participant", by.y = "Participant")
 
 
 
  Fixations_AggTrialParts[, c("Participant", "Display", "Task", "Parts", "Exp", "TaskOrder")] %<>% lapply(function(x) as.factor(as.character(x)))
 
 
  
 
  #FixModel = lmer(Sp2Prop~ Task * ExpC * Parts + (1|Participant) + (1|Display) , data = Fixations_AggTrialParts)
  
  
  
  FixModel_full = lmer(Sp2Prop~ Task * Exp * Parts + TaskOrder+ (1+Task|Participant) + (1|Display) , data = Fixations_AggTrialParts)
  FixModel_noTO = lmer(Sp2Prop~ Task * Exp * Parts + (1+Task|Participant) + (1|Display) , data = Fixations_AggTrialParts)
  FixModel_4mains = lmer(Sp2Prop~ Task + Exp + Parts + TaskOrder+ (1+Task|Participant) + (1|Display) , data = Fixations_AggTrialParts)
  FixModel_3mains = lmer(Sp2Prop~ Task + Exp + Parts + (1+Task|Participant) + (1|Display) , data = Fixations_AggTrialParts)
  
  anova(FixModel_full, FixModel_noTO, FixModel_4mains, FixModel_3mains)
  
  
  summary(FixModel_noTO)
  plot(effect('Task', FixModel_noTO))
  plot(effect('Exp', FixModel_noTO))
  plot(effect('Parts', FixModel_noTO))
  plot(effect('Task*Exp', FixModel_noTO))
  plot(effect('Task*Parts', FixModel_noTO))
  plot(effect('Exp*Parts', FixModel_noTO))
  plot(effect('Task*Exp*Parts', FixModel_noTO))
 
   
 #ooh <- emmeans(FixModel_noTO, pairwise ~ Exp | Parts)
 
 
 
 

 #summaryBy(Sp2Prop ~ TaskOrder + Exp, data = Fixations_AggTrialParts, FUN = c(mean, sd), na.rm = TRUE)
 
 
  # save model output to file  
 write("\n########## FIXATIONS PER PART ANALYSIS: optimal model ###########",file=textfile,append=TRUE)
 capture.output(summary(FixModel_noTO), file = textfile, append = TRUE) 
 
 write("\n APPROXIMATED P-VALUES:",file=textfile,append=TRUE)
 FixPart_tValues <- fixef(FixModel_noTO) / sqrt(diag(vcov(FixModel_noTO)))
 FixPart_pValues <- 2*(1-pnorm(abs(FixPart_tValues)))
 Output_table = cbind(coef(summary(FixModel_noTO)), FixPart_pValues)
 print(round(Output_table, 2))
 write.table(round(Output_table,4),file=textfile,append=TRUE, sep = "\t")
 
 
 
 
 
 
 
 
 
 

 
 
 
  return(TurnPtsTable)
}