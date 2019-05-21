a_EyeTrackingPreProc  <- function(Experiment) {
  
  ### Go from fixations to a Gaze report with conditions:
  a_FixToGaze(Experiment) # 
  
  ### Make a sample report
  a_GazeSampReport(Experiment)
  
}