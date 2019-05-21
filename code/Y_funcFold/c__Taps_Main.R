c__Taps_Main <- function() {
 
  # set parameters:
  align_flag = "ConfOffset" # Options: "ParticOnset" "ConfOffset"
  RespTypesToKeep = c("h", "g") # keep correct and hesitated response types
  
  ##### TAPS PER PART #####
  # Generating the input datatables: 
  Exp2_Part_Effects_Table = c_TapsPerPart_Datatables("Exp2", align_flag) 
  Exp3_Part_Effects_Table = c_TapsPerPart_Datatables("Exp3", align_flag) 
  Part_Effects_Table = rbind(Exp2_Part_Effects_Table, Exp3_Part_Effects_Table)
  
  # select only baseline part and save to file for the 0.5s bin analysis (next step in main script)
  Part_Effects_3_baseline = Part_Effects_Table[Part_Effects_Table$Parts == "Base_Part", ]
  write.table(Part_Effects_3_baseline, paste(analysisFolder, "02_Datafiles/", "BaseTapRate.txt", sep = ""), append = FALSE, quote = FALSE, row.names = FALSE, sep = "\t", na = "NA")
  
  
  
  # Parts Analysis with glmer #
  c_TapsPerPart_Analysis(Part_Effects_Table, RespTypesToKeep)
  
  
  
  
  #### TAPS PER BIN ######
  # Generating and merging the necessary datatables: 
  Exp2_Bin_Effects_Table = c_TapsPerBin_Datatables(align_flag, "Exp2") 
  Exp3_Bin_Effects_Table = c_TapsPerBin_Datatables(align_flag, "Exp3") 
  Bin_Effects_Table = rbind(Exp2_Bin_Effects_Table, Exp3_Bin_Effects_Table)
  
  # Bin Analysis Plotting 
  c_TapsPerBin_Plotting(Bin_Effects_Table, RespTypesToKeep)
  
  # Bins Analysis with glmer
  c_TapsPerBin_Analysis(Bin_Effects_Table, RespTypesToKeep)
  
}
