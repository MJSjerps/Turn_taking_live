b_Speech_PlotTurns <- function(speech_data) {
  
 #speech_data = SpeechDataTables
 speech_data_2 = speech_data[is.element(speech_data$S2correct, c("g", "h")), ]
 plotfolder = paste(analysisFolder, "03_Output/", sep = "") 
  
speech_data_2$Exp = relevel(speech_data_2$Exp, ref = "Exp3")

## plot turn intervals
pdf(paste(plotfolder, "/b_TurnIntervalsViolin.pdf", sep = ""),width=9,height=5)

TurnPlot = ggplot(speech_data_2, aes(x=interaction(Task, Exp), y=TurnGap, fill=Exp)) + 
  geom_violin(trim=FALSE, position=position_dodge(0.5))
TurnPlot2 = TurnPlot +scale_fill_manual(values= rev(ExpCol)) + coord_flip(ylim = c(-0.5, 2))  +
  geom_boxplot(width=.3, position=position_dodge(0.5)) + geom_hline(yintercept = 0, linetype = "solid") +
  labs(x = "Speaking condition", y = "Inter-turn interval (seconds)")

print(TurnPlot2)
dev.off()

## plot turn durations
pdf(paste(plotfolder, "/b_TurnDurationsViolin.pdf", sep = ""),width=9,height=5)

TurnDurPlot = ggplot(speech_data_2, aes(x=interaction(Task, Exp), y=Sp2Dur, fill=Exp)) + 
  geom_violin(trim=FALSE, position=position_dodge(0.5))
TurnDurPlot2 = TurnDurPlot +scale_fill_manual(values= rev(ExpCol)) + coord_flip(ylim = c(2, 6))  +
  geom_boxplot(width=.3, position=position_dodge(0.5)) + geom_hline(yintercept = 0, linetype = "solid") +
  labs(x = "Speaking condition", y = "Participant turn durations (seconds)")

print(TurnDurPlot2)


dev.off()


}

