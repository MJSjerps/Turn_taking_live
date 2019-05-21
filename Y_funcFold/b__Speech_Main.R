b__Speech_Main <- function() {
  
  SpeechDataTables <- b_Speech_DataTables()
  
  # Speech Analysis with glmer #
  b_Speech_Analysis_Fluency(SpeechDataTables)

  # Speech Analysis with glmer #
  b_Speech_Analysis_TurnsAndRates(SpeechDataTables)
  
  #### Parts Plotting #####
  b_Speech_PlotTurns(SpeechDataTables)
  
}
