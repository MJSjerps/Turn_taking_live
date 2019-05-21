a__Preprocess_Main <- function(analysisFolder) {
  

  ### FIRST GENERATE AN OVERALL CONDITION KEY FILE (is used in the preprocessing steps below)
  a_mergeConditionKeys("Exp2")
  a_mergeConditionKeys("Exp3")
  
  ### PREPROCESS TAPPING DATA  ########################################################################################
  a_tappingPreProc("Exp2")
  a_tappingPreProc("Exp3")
  
  ### PREPROCESS EYE-TRACKING DATA  ########################################################################################
  a_EyeTrackingPreProc("Exp2")
  a_EyeTrackingPreProc("Exp3")
  
  
  #return("")
}