c_TapsPerBin_PlotPanel <- function(Experiment, TapRate_overall_Bin_Means, TapRate_Bin_SD, nSubj) {

  ### Set plotting parameters:
  X_Range = c(-4.5, 5)
  Y_Range = c(1.5, 5)
  
  if (Experiment == "Exp2") {
    MainText = "Live confederate (exp2)"
    xTitle = ""
    SetExpCol = ExpCol[1]
    xLabValBool = FALSE
  } else if (Experiment == "Exp3") {
    MainText = "Pre-recorded confederate (exp3)"
    xTitle = "Time (in seconds) from confederate offset"
    SetExpCol = ExpCol[2]
    xLabValBool = TRUE
  }
  
  speech_data = read.table(paste(analysisFolder, "01_PreProcInput/", Experiment, "/", Experiment, "_SpeechData_INPUT.txt", sep = ""), header = T, sep = "")
  
  # restrict visualized tap rate bins between -6 and 5
  TapRate_overall_Bin_Means_Exp = TapRate_overall_Bin_Means[TapRate_overall_Bin_Means$Exp == Experiment & TapRate_overall_Bin_Means$SpAlignedBins > X_Range[1] & TapRate_overall_Bin_Means$SpAlignedBins < X_Range[2], ]
  TapRate_Bin_SD_Exp = TapRate_Bin_SD[TapRate_Bin_SD$Exp == Experiment & TapRate_Bin_SD$SpAlignedBins > X_Range[1] & TapRate_Bin_SD$SpAlignedBins < X_Range[2], ]
  

### plot mean tap rate per bin + SD (arrows)
plot(TapRate_overall_Bin_Means_Exp$SpAlignedBins, TapRate_overall_Bin_Means_Exp$Tap_Rate
     , ylim = c(Y_Range[1], Y_Range[2])
     , xlim = c(-7, 7)
     , ylab = "Tapping rate (taps per second)"
     , xlab = xTitle
     , xaxt='n'
     , pch = 15
     , col = SetExpCol
     , main = MainText)
axis(1, at = c(-4 : 5), labels = xLabValBool)
lines(TapRate_overall_Bin_Means_Exp$SpAlignedBins, TapRate_overall_Bin_Means_Exp$Tap_Rate)    # connect data points with line
arrows(TapRate_overall_Bin_Means_Exp$SpAlignedBins, TapRate_overall_Bin_Means_Exp$Tap_Rate, 
    x1 = TapRate_overall_Bin_Means_Exp$SpAlignedBins, 
    y1 = (TapRate_overall_Bin_Means_Exp$Tap_Rate + TapRate_Bin_SD_Exp$Tap_RateSE), 
    length = 0.05, angle = 90, code = 2, col = par("fg"), lty = par("lty"), lwd = par("lwd"))


### plot turn duration markers:
TurnDurations <- speech_data[(speech_data$S2correct == "g" | speech_data$S2correct == "h"),]

TurnDurations$S1 = TurnDurations$S1W4e-TurnDurations$S1s
sp1min = quantile(TurnDurations$S1, .025) 
sp1max = quantile(TurnDurations$S1, .975) 

TurnDurations$S2 = TurnDurations$S2W4e-TurnDurations$S2s
sp2min = quantile(TurnDurations$S2, .025) 
sp2max = quantile(TurnDurations$S2, .975) 

TurnDurations$turn = TurnDurations$S2s - TurnDurations$S1W4e
turnmin = quantile(TurnDurations$turn, .025)
turnmax = quantile(TurnDurations$turn, .975)

bottom = Y_Range[1]
top = Y_Range[2]
# vertical line at 0 (S1 offset)
lines(x = c(0,0), y = c(bottom, (top*0.75)), lty = 1, lwd = 1) 

# horizontal lines (dashes) SP1 and SP2 turndurations
lines(x = c(-sp1max, 0), y = c((top*0.75), (top*0.75)), lty = 2, lwd = 1) 
lines(x = c(turnmin,sp2max), y = c((top*0.80), (top*0.80)), lty = 2, lwd = 1) 

text(-3, (top*0.81), expression(italic("confederate turn durations")))
text(3, (top*0.86), expression(italic("participant turn durations")))

# mark min and max S1/S2 turn duration, gap between turns
lines(x = c(-sp1min,-sp1min), y = c((top*0.75), (top*0.78)), lty = 1, lwd = 1) 
lines(x = c(-sp1max,-sp1max), y = c((top*0.75), (top*0.78)), lty = 1, lwd = 1) 
lines(x = c(sp2min,sp2min), y = c((top*0.78), (top*0.80)), lty = 1, lwd = 1) 
lines(x = c(sp2max,sp2max), y = c((top*0.78), (top*0.80)), lty = 1, lwd = 1) 
lines(x = c(turnmin,turnmin), y = c((top*0.80), (top*0.83)), lty = 1, lwd = 1) 
lines(x = c(turnmax,turnmax), y = c((top*0.80), (top*0.83)), lty = 1, lwd = 1) 


### add baseline to graph

TapPlotData_Baseline = TapRate_overall_Bin_Means[TapRate_overall_Bin_Means$Exp == Experiment & TapRate_overall_Bin_Means$SpAlignedBins == -99, ]
TapPlotData_Baseline$SpAlignedBins = -7
TapRate_Bin_SD_Exp = TapRate_Bin_SD[TapRate_Bin_SD$Exp == Experiment & TapRate_Bin_SD$SpAlignedBins == -99, ]
TapRate_Bin_SD_Exp$SpAlignedBins = -7

Baseline_Mean = TapPlotData_Baseline$Tap_Rate
Baseline_SE = TapRate_Bin_SD_Exp$Tap_RateSE

rect(-7, Baseline_Mean*0.98, -5, Baseline_Mean*1.02, col = SetExpCol, border = NA)
lines(x = c(-6, 5), y = c(Baseline_Mean,Baseline_Mean), lty = 2, lwd = 1) 
text(-6, (Baseline_Mean*0.95), expression(italic("base rate")))

arrows(-6, Baseline_Mean,  x1 = -6,  y1 = (Baseline_Mean + Baseline_SE), 
length = 0.05, angle = 90, code = 2, col = par("fg"), lty = par("lty"), lwd = par("lwd"))



}
