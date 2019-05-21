### MAIN SCRIPT ########################################################################################
  #install.packages("vioplot")
### PACKAGES: load from library (and update) ###########################################################
  libList = c("languageR", "reshape", "lme4", "magrittr", "ggplot2", "data.table", "languageR", "devtools", "vioplot", "car", "effects", "doBy")
  lapply(libList, require, character.only = TRUE)
  #update.packages()
  
  
options(contrasts=c("contr.Sum","contr.poly"))
  
### ENVIRONMENT  ########################################################################################
  
 # remove all objects from current workspace
  rm(list=ls())
    
  analysisFolder = "K:/Tapping_Analyses_OSF/Tapping_Analyses_OSF/"

 # load all subfunctions  
  functionFolder = paste(analysisFolder, "Y_funcFold",sep = "")
  file.sources = list.files(functionFolder, pattern="*.R", full.names=TRUE, ignore.case=TRUE)  
  sapply(file.sources,source,.GlobalEnv)
      
### PREPROCESSING  ########################################################################################
 # runs the preprocessing and stores generated datafiles for plotting and analyses
  a__Preprocess_Main(analysisFolder)
  
### ANALYSES AND FIGURES #################################################################################
  ExpCol = c("#E69F00", "#999999") # the figure colors associated with the two experiments. Live condererate and pre-recorded one respectively
  
  ## Speech data
  b__Speech_Main()
  
  ## Tapping Data
  c__Taps_Main() # 

  # Eye Tracking data
  d__EyeTracking_Main()
  
  