
c_TapsPerPart_Analysis <- function(TapsPerPart, RespTypesToKeep) {

  ### PART ANALYSIS
  TapsPerPart = Part_Effects_Table
  
  # add task order
  TaskOrder_Exp2 = read.table(paste(analysisFolder, "01_PreProcInput/Exp2/Exp2_TaskOrder_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
  TaskOrder_Exp3 = read.table(paste(analysisFolder, "01_PreProcInput/Exp3/Exp3_TaskOrder_INPUT.txt", sep = ""), header = TRUE, sep = "\t")
  TaskOrder <- rbind(TaskOrder_Exp2, TaskOrder_Exp3)
  Part_Effects_Table = merge(Part_Effects_Table, TaskOrder, by.x = "Participant", by.y = "Participant")
  
  

  # set parameters:
  setnames(TapsPerPart, old = c('Parts'), new = c('Window'))
  textfile = paste(analysisFolder, "03_Output/c_TappingAnalyses_Parts.txt", sep = "") 
  write("File with tapping analyses",file=textfile,append=FALSE)
  
  ## first exclude incorrect responses 
  write("\n########## Data exclusions: ###########",file=textfile,append=TRUE)
  TapsPerPart_2 = TapsPerPart[is.element(TapsPerPart$S2correct, RespTypesToKeep), ]
  write("Experiment 2 incorrect verbal responses",file=textfile,append=TRUE)
  line = round(1- nrow(TapsPerPart_2[TapsPerPart_2$Exp == "Exp2",])/nrow(TapsPerPart[TapsPerPart$Exp == "Exp2",]), 4)*100
  write(line,file=textfile,append=TRUE)
  write("Experiment 3 incorrect verbal responses",file=textfile,append=TRUE)
  line = round(1- nrow(TapsPerPart_2[TapsPerPart_2$Exp == "Exp3",])/nrow(TapsPerPart[TapsPerPart$Exp == "Exp3",]), 4)*100
  write(line,file=textfile,append=TRUE)
  
# select relevant trial parts
  Taps_TestParts = TapsPerPart_2[is.element(TapsPerPart_2$Window, c("Base_Part", "Sp1_Part", "Sp2_Part")), ]

# remove data with tap rate of less than 1 at baseline
  AllBaseCnt = Taps_TestParts[(Taps_TestParts$Window == "Base_Part"), ] # only computed as a reference for the proportion removed
  Baseline_OK = Taps_TestParts[(Taps_TestParts$Window == "Base_Part" & Taps_TestParts$Tap_Rate >= 1), ]
  Baseline_OK = Baseline_OK[,(names(Baseline_OK) %in% c("Participant", "Display", "Task", "Exp", "TaskOrder"))] # keep these columns
  
  BaselineTooSlowExp2 = round(((1-nrow(Baseline_OK[Baseline_OK$Exp == "Exp2", ])/nrow(AllBaseCnt[AllBaseCnt$Exp == "Exp2", ]))*100),1)
  #print(BaselineTooSlowExp2)
  write("Experiment 2 slow baseline",file=textfile,append=TRUE)
  write(print(BaselineTooSlowExp2),file=textfile,append=TRUE)
  BaselineTooSlowExp3 = round(((1-nrow(Baseline_OK[Baseline_OK$Exp == "Exp3", ])/nrow(AllBaseCnt[AllBaseCnt$Exp == "Exp3", ]))*100),1)
  #print(BaselineTooSlowExp3)
  write("Experiment 3 slow baseline",file=textfile,append=TRUE)
  write(print(BaselineTooSlowExp3),file=textfile,append=TRUE)
  
  Taps_TestParts_2 <- merge(Taps_TestParts, Baseline_OK, by.x = c("Participant", "Display", "Task", "Exp", "TaskOrder"), by.y = c("Participant", "Display", "Task", "Exp", "TaskOrder"))
  
# calculate mean tapping rate per part  
  AvTapRate <- with(Taps_TestParts_2, tapply(Tap_Rate, list(Window, Exp), mean))
  print(round(AvTapRate,2)) 
  write("\nMeans per condition:",file=textfile,append=TRUE)
  write.table(round(AvTapRate,2),file=textfile,append=TRUE, sep = "\t")
  
  
  #Taps_TestParts_2[, c("Participant", "Display", "Task", "Exp", "TaskOrder", "Window")] %<>% lapply(function(x) as.factor(as.character(x)))
 
  
# LMER: CHECK IF THERE IS AN EFFECT OF TIME-WINDOW (PART) ON TAPPING RATE  
  # create a centered predictor for Experiment
  #Taps_TestParts_2$ExpCen[Taps_TestParts_2$Exp == "Exp2"] = as.numeric(1)
  #Taps_TestParts_2$ExpCen[Taps_TestParts_2$Exp == "Exp3"] = as.numeric(-1)
  Taps_TestParts_2$Exp <- as.factor(as.character(Taps_TestParts_2$Exp))
  Taps_TestParts_2$Window <- as.factor(as.character(Taps_TestParts_2$Window))
  
 
  contrasts(Taps_TestParts_2$Window) <- contr.Treatment
  #Taps_TestParts_2$Window <- relevel (Taps_TestParts_2$Window, ref = "Base_Part")
  
  #Taps_Parts_model_full = lmer(Tap_Rate ~ as.factor(Window)*ExpCen + (1+Window||Participant) + (1|Display), data = Taps_TestParts_2)
  #Taps_Parts_model_2mains = lmer(Tap_Rate ~ as.factor(Window)+ExpCen + (1+Window||Participant) + (1|Display), data = Taps_TestParts_2)
  
  
  
  Taps_Parts_model_full = lmer(Tap_Rate ~ Window*Exp + TaskOrder+ (1|Participant) + (1|Display), data = Taps_TestParts_2)
  Taps_Parts_model_noTO = lmer(Tap_Rate ~ Window*Exp + (1|Participant) + (1|Display), data = Taps_TestParts_2)
  Taps_Parts_model_3mains = lmer(Tap_Rate ~ Window +Exp + TaskOrder + (1|Participant) + (1|Display), data = Taps_TestParts_2)
  Taps_Parts_model_2mains = lmer(Tap_Rate ~ Window +Exp + (1|Participant) + (1|Display), data = Taps_TestParts_2)
  
  anova(Taps_Parts_model_full, Taps_Parts_model_noTO, Taps_Parts_model_3mains, Taps_Parts_model_2mains)
  summary(Taps_Parts_model_noTO)
  
  plot(effect('Window', Taps_Parts_model_noTO))
  plot(effect('Exp', Taps_Parts_model_noTO))
  
 # save model output to file  
  write("\n########## Model output optimal model after model comparison ###########",file=textfile,append=TRUE)
  capture.output(summary(Taps_Parts_model_noTO), file = textfile, append = TRUE) 
  
# calculate and print p-values
  write("\n APPROXIMATED P-VALUES:",file=textfile,append=TRUE)
  TapsPart_tValues <- fixef(Taps_Parts_model_noTO) / sqrt(diag(vcov(Taps_Parts_model_noTO)))
  TapsPart_pValues <- 2*(1-pnorm(abs(TapsPart_tValues)))
  print(round(TapsPart_pValues,4))
  write.table(round(rbind(TapsPart_tValues, TapsPart_pValues),3),file=textfile,append=TRUE, sep = "\t")
  
  write("\n########## T.2 in model output = Sp1Part, T.3 = Sp2Part ###########",file=textfile,append=TRUE)
  
  
  
  
  #Taps_Parts_model_full_Extra = lmer(Tap_Rate ~ Window*Exp + TaskOrder + Display + (1|Participant) + (1|Display), data = Taps_TestParts_2)
  #summary(Taps_Parts_model_full_Extra) 
  
  
  
  
  
  
}








