d_Fixations_Plotting <- function(SampRep_Agg, TurnPtsTable) {


  # set parameters:
 

# split data in SO and TS condition and by Experiment (to make plot scriptshorter)  
SampRep_Exp2_SO = SampRep_Agg[SampRep_Agg$Task == "SO" & SampRep_Agg$Exp == "Exp2",]
SampRep_Exp2_TS = SampRep_Agg[SampRep_Agg$Task == "TS" & SampRep_Agg$Exp == "Exp2",]
SampRep_Exp3_SO = SampRep_Agg[SampRep_Agg$Task == "SO" & SampRep_Agg$Exp == "Exp3",]
SampRep_Exp3_TS = SampRep_Agg[SampRep_Agg$Task == "TS" & SampRep_Agg$Exp == "Exp3",]


pdf(paste(analysisFolder, "03_Output/d_FixationPreferencePlots.pdf", sep = ""),width=9,height=8)

### preferences plot:
# plot settings
par(mfrow = c(2, 1), 	 # number of panels
    oma=c(1,1,1,1), 	     # outer margins: whole figure
    mar=c(3,3,2,1), 	     # inner margin: seperate panels order: bottom, left, top, right
    mgp =c(2,1,0))		     # position of the axes (text, ticks, line)
#top = 1.2              # max on y-axis
#bottom = 0             # min on y-axis



xlim = c(-4,6)
ylim = c(-1.05, 1.05)

colorVals = ExpCol[1] # 
plot(0,0, type = "n", xlim, ylim, pch = 19, cex = 0.5, main = "Fixation preferences: Live confederate", ylab = "Fixation preferences", xlab = "", yaxt = "n")
axis(1, at = c(-4 : 6), labels = TRUE)
axis(2, at = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, cex.axis=0.8)
plotNewFlag = 0
d_shadedErrorRegion(SampRep_Exp2_SO$Aligned_Gaze, SampRep_Exp2_SO$FixPref_All, SampRep_Exp2_SO$FixPref_All_SE, colorVals, 'solid', plotNewFlag) 
d_shadedErrorRegion(SampRep_Exp2_TS$Aligned_Gaze, SampRep_Exp2_TS$FixPref_All, SampRep_Exp2_TS$FixPref_All_SE, colorVals, 'dashed', plotNewFlag) 
abline(a = 0, b = 0, lty = "dotted", lwd = 1.5)
legend(1, 1.1, c( "Speaking Only", "Tapping and Speaking"), lwd = c("2","2"), lty = c("solid", "dashed"), bty = "n", col = c(ExpCol[1], ExpCol[1]))


colorVals = ExpCol[2] # 
plot(0,0, type = "n", xlim, ylim, pch = 19, cex = 0.5, main = "Fixation preferences: Pre-recorded confederate", ylab = "Fixation preferences", xlab = "Time to confederate offset (seconds)", yaxt = "n")
axis(1, at = c(-4 : 6), labels = TRUE)
axis(2, at = c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, cex.axis=0.8)
plotNewFlag = 0
d_shadedErrorRegion(SampRep_Exp3_SO$Aligned_Gaze, SampRep_Exp3_SO$FixPref_All, SampRep_Exp3_SO$FixPref_All_SE, colorVals, 'solid', plotNewFlag) 
d_shadedErrorRegion(SampRep_Exp3_TS$Aligned_Gaze, SampRep_Exp3_TS$FixPref_All, SampRep_Exp3_TS$FixPref_All_SE, colorVals, 'dashed', plotNewFlag) 
abline(a = 0, b = 0, lty = "dotted", lwd = 1.5)
legend(1, 1.1, c( "Speaking Only", "Tapping and Speaking"), lwd = c("2","2"), lty = c("solid", "dashed"), bty = "n", col = c(ExpCol[2], ExpCol[2]))

dev.off()



### fixation proportions:
pdf(paste(analysisFolder, "03_Output/d_FixationProportionPlots.pdf", sep = ""),width=9,height=5)

# plot settings
par(mfrow = c(2, 2), 	 # number of panels
    oma=c(1,1,1,1), 	     # outer margins: whole figure
    mar=c(3,3,2,1), 	     # inner margin: seperate panels order: bottom, left, top, right
    mgp =c(2,1,0))		     # position of the axes (text, ticks, line)
#top = 1.2              # max on y-axis
#bottom = 0             # min on y-axis

xlim = c(-7,7)
ylim = c(0,1.05)

# plot fixations in SO condition of Experiment 2 (real confederate)
colorVals = ExpCol[1] # 
plot(0,0, type = "n", xlim, ylim, pch = 19, cex = 0.5, main = "Live confederate: Speaking Only", ylab = "Proportion of fixations", xlab = "Time to confederate offset (seconds)", yaxt = "n")
axis(1, at = c(-6 : 7), labels = TRUE)
axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, cex.axis=0.8)
plotNewFlag = 0
d_shadedErrorRegion(SampRep_Exp2_SO$Aligned_Gaze, SampRep_Exp2_SO$Sp1_Sum, SampRep_Exp2_SO$Sp1_Sum_SE, colorVals, 'dashed', plotNewFlag) 
d_shadedErrorRegion(SampRep_Exp2_SO$Aligned_Gaze, SampRep_Exp2_SO$Sp2_Sum, SampRep_Exp2_SO$Sp2_Sum_SE, colorVals, 'solid', plotNewFlag) 

legend(-7.5, 1.1, c( "Confederate Objects", "Own Objects"), lwd = c("2","2"), lty = c("dashed", "solid"), bty = "n", col = c(ExpCol[1], ExpCol[1]))


# plot fixations in  TS condition of Experiment 2 (real confederate)
plot(0,0, type = "n", xlim, ylim, pch = 19, cex = 0.5, main = "Live confederate: Tapping and Speaking", ylab = "Proportion of fixations", xlab = "Time to confederate offset (seconds)", yaxt = "n")
axis(1, at = c(-6 : 7), labels = TRUE)
axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, cex.axis=0.8)
d_shadedErrorRegion(SampRep_Exp2_TS$Aligned_Gaze, SampRep_Exp2_TS$Sp1_Sum, SampRep_Exp2_TS$Sp1_Sum_SE, colorVals, 'dashed', plotNewFlag) 
d_shadedErrorRegion(SampRep_Exp2_TS$Aligned_Gaze, SampRep_Exp2_TS$Sp2_Sum, SampRep_Exp2_TS$Sp2_Sum_SE, colorVals, 'solid', plotNewFlag) 



# plot fixations in SO condition of Experiment 3 (pre-recorded confederate)
colorVals = ExpCol[2] # 
plot(0,0, type = "n", xlim, ylim, pch = 19, cex = 0.5, main = "Pre-recorded confederate: Speaking Only", ylab = "Proportion of fixations", xlab = "Time to confederate offset (seconds)", yaxt = "n")
axis(1, at = c(-6 : 7), labels = TRUE)
axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, cex.axis=0.8)
d_shadedErrorRegion(SampRep_Exp3_SO$Aligned_Gaze, SampRep_Exp3_SO$Sp1_Sum, SampRep_Exp3_SO$Sp1_Sum_SE, colorVals, 'dashed', plotNewFlag) 
d_shadedErrorRegion(SampRep_Exp3_SO$Aligned_Gaze, SampRep_Exp3_SO$Sp2_Sum, SampRep_Exp3_SO$Sp2_Sum_SE, colorVals, 'solid', plotNewFlag) 

legend(-7.5, 1.1, c("Confederate Objects", "Own Objects"), lwd = c("2","2"), lty = c("dashed", "solid"), bty = "n", col = c(ExpCol[2], ExpCol[2]))



# plot fixations to participant objects in Experiment 3:

plot(0,0, type = "n", xlim, ylim, pch = 19, cex = 0.5, main = "Exp2 - Tapping and Speaking", ylab = "Proportion of fixations", xlab = "Time to confederate offset (seconds)", yaxt = "n")
axis(1, at = c(-6 : 7), labels = TRUE)
axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = TRUE, cex.axis=0.8)
d_shadedErrorRegion(SampRep_Exp3_TS$Aligned_Gaze, SampRep_Exp3_TS$Sp1_Sum, SampRep_Exp3_TS$Sp1_Sum_SE, colorVals, 'dashed', plotNewFlag) 
d_shadedErrorRegion(SampRep_Exp3_TS$Aligned_Gaze, SampRep_Exp3_TS$Sp2_Sum, SampRep_Exp3_TS$Sp2_Sum_SE, colorVals, 'solid', plotNewFlag) 


dev.off()

### make frequency dirstribution plot of turning points:

pdf(paste(analysisFolder, "03_Output/d_FixationTurningPoints.pdf", sep = ""),width=9,height=8)

DensityPlot= ggplot(data=TurnPtsTable[TurnPtsTable$Exp == "Exp2", ], aes(x = TransPt)) + 
  geom_freqpoly(aes(linetype = Task), binwidth = 0.5, color = ExpCol[1], size=2) +
  scale_x_continuous(limits = c(-7, 3), breaks = seq(-7,3, by = 1)) + scale_y_continuous(limits = c(0, 25)) +
  ggtitle("Live Confederate") + xlab("Transition point per participant") + ylab("Count")

DensityPlot2 = ggplot(data=TurnPtsTable[TurnPtsTable$Exp == "Exp3", ], aes(x = TransPt)) + 
  geom_freqpoly(aes(linetype = Task), binwidth = 0.5, color = ExpCol[2], size=2) +
  scale_x_continuous(limits = c(-7, 3), breaks = seq(-7,3, by = 1)) + scale_y_continuous(limits = c(0, 25)) +
  ggtitle("Pre-recorded Confederate") + xlab("Transition point per participant") + ylab("Count")

multiplot(DensityPlot, DensityPlot2, nrow = 2, cols = 1)

dev.off()

}
