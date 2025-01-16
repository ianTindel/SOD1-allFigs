#####
#Figure 1
#####
library(ggplot2)
#despite the name of this file, it eventually came to encompass the grip strength assay used as well. Replace the .csv file name with whatever your all-raw data file name is
bmsRawValues <- read.csv(file = "bwbms_P150.csv")
ages=data.frame(ticks=c(25,50,90,125,150))

#1A is just raw data, so has no script

#1B: Body weights
ggplot(data=bmsRawValues, aes(x=mouseAge, y=BWAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(0,35)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("Body Weight (grams)") +
  xlab("Mouse Age (days since born)")+
  labs(title="Mouse Body Weights Over Time")+
  geom_errorbar(aes(ymin=BWAvg-BWSD, ymax=BWAvg+BWSD), width=.5)+
  geom_ribbon(aes(ymin=BWNegConInt, ymax=BWConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#1C: overall BMS
ggplot(data=bmsRawValues, aes(x=mouseAge, y=AvgAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(2,22)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Hindlimb + Forelimb BMS Over Time")+
  geom_errorbar(aes(ymin=AvgAvg-AvgSD, ymax=AvgAvg+AvgSD), width=.5)+
  geom_ribbon(aes(ymin=AvgNegConInt, ymax=AvgConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#1D: forelimb BMS
ggplot(data=bmsRawValues, aes(x=mouseAge, y=FLAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(2,22)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Forelimb BMS Over Time")+
  geom_errorbar(aes(ymin=FLAvg-FLSD, ymax=FLAvg+FLSD), width=.5)+
  geom_ribbon(aes(ymin=FLNegConInt, ymax=FLConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#1E: hindlimb BMS
ggplot(data=bmsRawValues, aes(x=mouseAge, y=HLAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(2,22)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Hindlimb BMS Over Time")+
  geom_errorbar(aes(ymin=HLAvg-HLSD, ymax=HLAvg+HLSD), width=.5)+
  geom_ribbon(aes(ymin=HLNegConInt, ymax=HLConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#1F was carried out by M.L.

#1G: coordination
ggplot(data=bmsRawValues, aes(x=mouseAge, y=CoordAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(0,2.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Coordination Over Time")+
  geom_errorbar(aes(ymin=CoordAvg-CoordSD, ymax=CoordAvg+CoordSD), width=.5)+
  geom_ribbon(aes(ymin=CoordNegConInt, ymax=CoordConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#1H: trunk stability
ggplot(data=bmsRawValues, aes(x=mouseAge, y=TrunkAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(0,2.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Trunk Stability Over Time")+
  geom_errorbar(aes(ymin=TrunkAvg-TrunkSD, ymax=TrunkAvg+TrunkSD), width=.5)+
  geom_ribbon(aes(ymin=TrunkNegConInt, ymax=TrunkConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#1I: Tail use
ggplot(data=bmsRawValues, aes(x=mouseAge, y=TailAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(0,2.5)+
  scale_color_manual(values=c("#833C0C", "#4472C4", "#F8CBAD", "#676767"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Tail Lifting Over Time")+
  geom_errorbar(aes(ymin=TailAvg-TailSD, ymax=TailAvg+TailSD), width=.5)+
  geom_ribbon(aes(ymin=TailNegConInt, ymax=TailConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))


#####
#Fig 2
#####
#2A: Joint movement
ggplot(data=bmsRawValues, aes(x=mouseAge, y=JointFLAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,2.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Forelimb Joint Movement")+
  geom_errorbar(aes(ymin=JointFLAvg-JointFLSD, ymax=JointFLAvg+JointFLSD), width=.5)+
  geom_ribbon(aes(ymin=JointFLNegConInt, ymax=JointFLConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))
ggplot(data=bmsRawValues, aes(x=mouseAge, y=JointHLAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,2.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Hindlimb Joint Movement")+
  geom_errorbar(aes(ymin=JointHLAvg-JointHLSD, ymax=JointHLAvg+JointHLSD), width=.5)+
  geom_ribbon(aes(ymin=JointHLNegConInt, ymax=JointHLConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#2B: Weight Support
ggplot(data=bmsRawValues, aes(x=mouseAge, y=WeightFLAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,3.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Forelimb Weight Support")+
  geom_errorbar(aes(ymin=WeightFLAvg-WeightFLSD, ymax=WeightFLAvg+WeightFLSD), width=.5)+
  geom_ribbon(aes(ymin=WeightFLNegConInt, ymax=WeightFLConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))
ggplot(data=bmsRawValues, aes(x=mouseAge, y=WeightHLAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,3.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Hindlimb Weight Support")+
  geom_errorbar(aes(ymin=WeightHLAvg-WeightHLSD, ymax=WeightHLAvg+WeightHLSD), width=.5)+
  geom_ribbon(aes(ymin=WeightHLNegConInt, ymax=WeightHLConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#2C: Paw Placement
ggplot(data=bmsRawValues, aes(x=mouseAge, y=PawPlaceFLavg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,3.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Forelimb Paw Placement")+
  geom_errorbar(aes(ymin=PawPlaceFLavg-PawPlaceFLsd, ymax=PawPlaceFLavg+PawPlaceFLsd), width=.5)+
  geom_ribbon(aes(ymin=PawPlaceFLnegconint, ymax=PawPlaceFLconint), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))
ggplot(data=bmsRawValues, aes(x=mouseAge, y=PawPlaceHLavg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,3.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Hindlimb Paw Placement")+
  geom_errorbar(aes(ymin=PawPlaceHLavg-PawPlaceHLsd, ymax=PawPlaceHLavg+PawPlaceHLsd), width=.5)+
  geom_ribbon(aes(ymin=PawPlaceHLnegconint, ymax=PawPlaceHLconint), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#2D: Dorsal stepping
ggplot(data=bmsRawValues, aes(x=mouseAge, y=DorsalFLavg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,3.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Forelimb Dorsal Stepping")+
  geom_errorbar(aes(ymin=DorsalFLavg-DorsalFLsd, ymax=DorsalFLavg+DorsalFLsd), width=.5)+
  geom_ribbon(aes(ymin=DorsalFLnegconint, ymax=DorsalFLconint), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))
ggplot(data=bmsRawValues, aes(x=mouseAge, y=DorsalHLavg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,3.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Hindlimb Dorsal Stepping")+
  geom_errorbar(aes(ymin=DorsalHLavg-DorsalHLsd, ymax=DorsalHLavg+DorsalHLsd), width=.5)+
  geom_ribbon(aes(ymin=DorsalHLnegconint, ymax=DorsalHLconint), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#2E: Missing Steps
ggplot(data=bmsRawValues, aes(x=mouseAge, y=MissingFLavg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,2.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Forelimb Missing Steps")+
  geom_errorbar(aes(ymin=MissingFLavg-MissingFLsd, ymax=MissingFLavg+MissingFLsd), width=.5)+
  geom_ribbon(aes(ymin=MissingFLnegconint, ymax=MissingFLconint), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))
ggplot(data=bmsRawValues, aes(x=mouseAge, y=MissingHLavg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,2.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Hindlimb Missing Steps")+
  geom_errorbar(aes(ymin=MissingHLavg-MissingHLsd, ymax=MissingHLavg+MissingHLsd), width=.5)+
  geom_ribbon(aes(ymin=MissingHLnegconint, ymax=MissingHLconint), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#2F: Paw Position
ggplot(data=bmsRawValues, aes(x=mouseAge, y=PPLOICFLavg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,4.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Forelimb Paw Position")+
  geom_errorbar(aes(ymin=PPLOICFLavg-PPLOICFLsd, ymax=PPLOICFLavg+PPLOICFLsd), width=.5)+
  geom_ribbon(aes(ymin=PPLOICFLnegconint, ymax=PPLOICFLconint), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))
ggplot(data=bmsRawValues, aes(x=mouseAge, y=PPLOICHLavg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(-0.5,4.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Hindlimb Paw Position")+
  geom_errorbar(aes(ymin=PPLOICHLavg-PPLOICHLsd, ymax=PPLOICHLavg+PPLOICHLsd), width=.5)+
  geom_ribbon(aes(ymin=PPLOICHLnegconint, ymax=PPLOICHLconint), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#####
#Fig 3
#####
### add gaps to your y-axis if it does not go to zero
#install.packages("plotrix")
library('plotrix')
#grip strengths were calculated by applying Microsoft Excel's SLOPE function to the measurements/time of each mouse (provided that mouse had >=2 measurements)
mouseSlope <- read.table("gs_slopes_tgOnly_see202307.txt", h = T)
#ensure the column sex is a factor
str(mouseSlope$Sex)
mouseSlope$Sex <- factor(mouseSlope$Sex, levels = c("Female", "Male"))

#3A is just raw data

#3B: Grip strength at each tp
ggplot(data=bmsRawValues, aes(x=mouseAge, y=GSAvg, color=factor(Type)))+
  geom_point(size=2, aes(shape=factor(Type)))+
  ylim(0,7.5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("Wt-normalized grip strength (gram-force/grams)") +
  xlab("Mouse Age (days since born)")+
  labs(title="Mouse Grip Strength Over Time")+
  geom_errorbar(aes(ymin=GSAvg-GSSD, ymax=GSAvg+GSSD), width=.5)+
  geom_ribbon(aes(ymin=GSNegConInt, ymax=GSConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#3C was done by M.L.

#3D: Grip strength over time
boxplot(mouseSlope$GS[mouseSlope$Sex == "Female"],
        mouseSlope$GS[mouseSlope$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-0.14, 0.17), frame.plot = FALSE, las = 1, ylab = "Gram-force/Day")
box( bty = "L")
stripchart(GS ~ Sex, vertical = TRUE, data = mouseSlope, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
###title(paste("Rate of Grip Strength Change"))

#we're done with the grip strength figures, now it's time for ink pawprint figures
#######once again, these are just raw data files. you can use the provided supplementary table 1. while the first two are truly raw data, "lenvar" and "widvar" are the variances of that data (which can also be found in the raw data file).
pawprintLenWid <- read.csv(file = "pawprints_10_short_lenAndWid.csv")
pawprintAng <- read.csv(file = "pawprints_10_short_angOnly.csv")
lenvar <- read.csv(file = "pawprintsLenvar_short.csv")
widvar <- read.csv(file = "pawprintsWidvar_short.csv")

#3E is just raw data

#3F: stride lengths
ggplot(data=pawprintLenWid, aes(x=mouseAge, y=LenAvg, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(0,8)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1, show.legend = FALSE)+
  ylab("Stride Length (cm)") +
  xlab("Mouse Age (days since born)")+
  labs(title="Mouse Stride Length from Ink Pawprint Assays")+
  geom_errorbar(aes(ymin=LenAvg-LenSD, ymax=LenAvg+LenSD), show.legend = FALSE, width=.5)+
  geom_ribbon(aes(ymin=LenNegConInt, ymax=LenConInt), show.legend = FALSE, linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25", "P50", "P90", "P125", "P150"))

#3G: stride widths
ggplot(data=pawprintLenWid, aes(x=mouseAge, y=SWidAvg, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(0,6)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1, show.legend = FALSE)+
  ylab("Stride Width (cm)") +
  xlab("Mouse Age (days since born)")+
  labs(title="Mouse Stride Width from Ink Pawprint Assays")+
  geom_errorbar(aes(ymin=SWidAvg-SWidSD, ymax=SWidAvg+SWidSD), show.legend = FALSE, width=.5)+
  geom_ribbon(aes(ymin=SWidNegConInt, ymax=SWidConInt), show.legend = FALSE, linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25", "P50", "P90", "P125", "P150"))

#3H: stride length variances
ggplot(data=lenvar, aes(x=mouseAge, y=LenVar, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(0,1)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1, show.legend = FALSE)+
  ylab("% Variance in Stride Length") +
  xlab("Mouse Age (days since born)")+
  labs(title="Variance in Mouse Ink Pawprint Stride Length")+
  ##geom_errorbar(aes(ymin=LenAvg-LenSD, ymax=LenAvg+LenSD), width=.5)+
  ##geom_ribbon(aes(ymin=LenNegConInt, ymax=LenConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25", "P50", "P90", "P125", "P150"))
#3H: stride width variances
ggplot(data=widvar, aes(x=mouseAge, y=WidVar, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(0,1)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1, show.legend = FALSE)+
  ylab("% Variance in Stride Length") +
  xlab("Mouse Age (days since born)")+
  labs(title="Variance in Mouse Ink Pawprint Stride Width")+
  ##geom_errorbar(aes(ymin=LenAvg-LenSD, ymax=LenAvg+LenSD), width=.5)+
  ##geom_ribbon(aes(ymin=LenNegConInt, ymax=LenConInt), linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25", "P50", "P90", "P125", "P150"))

#values for the data in 3J-M come from the second sheet in the supplemental data table, "leadingPawData"
#3J: leading paw angles
ggplot(data=domPawprintAng, aes(x=mouseAge, y=AngAvg, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(20,60)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("Paw Angle (degrees)") +
  xlab("Mouse Age (days since born)")+
  labs(title="Non-leading Paw Angles")+
  geom_errorbar(aes(ymin=AngAvg-AngSD, ymax=AngAvg+AngSD), show.legend = FALSE, width=.5)+
  geom_ribbon(aes(ymin=AngNegConInt, ymax=AngConInt), show.legend = FALSE, linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25", "P50", "P90", "P125", "P150"))

#3K: non-leading paw angles
ggplot(data=nonDomPawprintAng, aes(x=mouseAge, y=AngAvg, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(20,60)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("Paw Angle (degrees)") +
  xlab("Mouse Age (days since born)")+
  labs(title="Non-leading Paw Angles")+
  geom_errorbar(aes(ymin=AngAvg-AngSD, ymax=AngAvg+AngSD), show.legend = FALSE, width=.5)+
  geom_ribbon(aes(ymin=AngNegConInt, ymax=AngConInt), show.legend = FALSE, linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25", "P50", "P90", "P125", "P150"))

#3L: leading paw angle variance
ggplot(data=domPawprintAng, aes(x=mouseAge, y=Var, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(10,150)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1, show.legend = FALSE)+
  ylab("% Variance in Leading Paw Angle") +
  xlab("Mouse Age (days since born)")+
  labs(title="Leading Paw Angle Variances")+
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25", "P50", "P90", "P125", "P150"))

#3M: non-leading paw angle variance
ggplot(data=nonDomPawprintAng, aes(x=mouseAge, y=Var, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(10,225)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1, show.legend = FALSE)+
  ylab("% Variance in Non-leading angles") +
  xlab("Mouse Age (days since born)")+
  labs(title="Non-leading Angle Variances")+
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25", "P50", "P90", "P125", "P150"))

#3N: degrees of parallel stepping
ggplot(data=dops, aes(x=mouseAge, y=dopsAvg, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(0.5,5)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1)+
  ylab("Parallelicity") +
  xlab("Mouse Age (days since born)")+
  labs(title="Degrees of Parallel Stepping")+
  geom_errorbar(aes(ymin=dopsAvg-dopsSD, ymax=dopsAvg+dopsSD), show.legend = FALSE, width=.5)+
  geom_ribbon(aes(ymin=dopsNegConInt, ymax=dopsConInt), show.legend = FALSE, linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(agesDops$ticks), labels=c("P90", "P125", "P150"))

#####
#Fig 4
#####
# make jitter plots translucent
library(scales)
#for asterisk significances
library(ggsignif)
#slopes are again from Excel's SLOPE function applied to each subject w/ >=2 timepoints w/ measurements
bmsSlope <- read.csv("BMS_slopes.csv")

#Fig 4A. Left and Right BMS scores
ggplot(data=bmsRawValues, aes(x=mouseAge, y=LLAvg, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(1,16)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1, show.legend = FALSE)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Left BMS Score Over Time")+
  geom_errorbar(aes(ymin=LLAvg-LLSD, ymax=LLAvg+LLSD), show.legend = FALSE, width=.5)+
  geom_ribbon(aes(ymin=LLNegConInt, ymax=LLConInt), show.legend = FALSE, linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))
ggplot(data=bmsRawValues, aes(x=mouseAge, y=RLAvg, color=factor(Type)))+
  geom_point(size=2, show.legend = FALSE, aes(shape=factor(Type)))+
  ylim(1,16)+
  scale_color_manual(values=c("darkorange3", "blue", "#4AA248", "darkorchid1"), name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  scale_shape_discrete(name= "Mouse Sex and Genotype", labels=c("Female tg", "Female wt", "Male tg", "Male wt"))+
  theme_bw()+
  geom_line(linewidth=1, show.legend = FALSE)+
  ylab("BMS Score") +
  xlab("Mouse Age (days since born)")+
  labs(title="Right BMS Score Over Time")+
  geom_errorbar(aes(ymin=RLAvg-RLSD, ymax=RLAvg+RLSD), show.legend = FALSE, width=.5)+
  geom_ribbon(aes(ymin=RLNegConInt, ymax=RLConInt), show.legend = FALSE, linetype=2, alpha=0.1) +
  scale_x_continuous(breaks = c(ages$ticks), labels=c("P25","P50","P90","P125","P150+"))

#Fig 4B: slopes of left and right
boxplot(bmsSlope$SlopeLL[bmsSlope$Sex == "Ftg"],
        bmsSlope$SlopeLL[bmsSlope$Sex == "Fwt"],
        bmsSlope$SlopeLL[bmsSlope$Sex == "Mtg"],
        bmsSlope$SlopeLL[bmsSlope$Sex == "Mwt"], notch = F,
        names = c("ALS Female", "Wt Female", "ALS Male", "Wt Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-0.11, 0.03), frame.plot = FALSE, las = 1, ylab = "BMS Score/Day")
stripchart(SlopeLL ~ Sex, vertical = TRUE, data = bmsSlope, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "blue", "#4AA248", "darkorchid1"), 0.7),
           jitter = 0.2)
title(paste("Rate of Left-side BMS Change"))
box( bty = "L")
boxplot(bmsSlope$SlopeRL[bmsSlope$Sex == "Ftg"],
        bmsSlope$SlopeRL[bmsSlope$Sex == "Fwt"],
        bmsSlope$SlopeRL[bmsSlope$Sex == "Mtg"],
        bmsSlope$SlopeRL[bmsSlope$Sex == "Mwt"], notch = F,
        names = c("ALS Female", "Wt Female", "ALS Male", "Wt Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-0.11, 0.03), frame.plot = FALSE, las = 1, ylab = "BMS Score/Day")
stripchart(SlopeRL ~ Sex, vertical = TRUE, data = bmsSlope, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "blue", "#4AA248", "darkorchid1"), 0.7),
           jitter = 0.2)
title(paste("Rate of Right-side BMS Change"))
box( bty = "L")

##p25
t.test(bmsSlope$LP25[203:292],bmsSlope$RP25[203:292], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP25[293:394],bmsSlope$RP25[293:394], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP25[1:82],bmsSlope$RP25[1:82], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP25[83:202],bmsSlope$RP25[83:202], alternative = "two.sided", var.equal = FALSE)$p.value
##p50
t.test(bmsSlope$LP50[203:292],bmsSlope$RP50[203:292], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP50[293:394],bmsSlope$RP50[293:394], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP50[1:82],bmsSlope$RP50[1:82], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP50[83:202],bmsSlope$RP50[83:202], alternative = "two.sided", var.equal = FALSE)$p.value
##p90
t.test(bmsSlope$LP90[203:292],bmsSlope$RP90[203:292], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP90[293:394],bmsSlope$RP90[293:394], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP90[1:82],bmsSlope$RP90[1:82], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP90[83:202],bmsSlope$RP90[83:202], alternative = "two.sided", var.equal = FALSE)$p.value
##p125
t.test(bmsSlope$LP125[203:292],bmsSlope$RP125[203:292], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP125[293:394],bmsSlope$RP125[293:394], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP125[1:82],bmsSlope$RP125[1:82], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP125[83:202],bmsSlope$RP125[83:202], alternative = "two.sided", var.equal = FALSE)$p.value
##p150
t.test(bmsSlope$LP150[203:292],bmsSlope$RP150[203:292], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP150[293:394],bmsSlope$RP150[293:394], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP150[1:82],bmsSlope$RP150[1:82], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$LP150[83:202],bmsSlope$RP150[83:202], alternative = "two.sided", var.equal = FALSE)$p.value
#####slopes
t.test(bmsSlope$SlopeLL[101:145],bmsSlope$SlopeRL[101:145], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$SlopeLL[147:195],bmsSlope$SlopeRL[147:195], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$SlopeLL[1:41],bmsSlope$SlopeRL[1:41], alternative = "two.sided", var.equal = FALSE)$p.value
t.test(bmsSlope$SlopeLL[43:99],bmsSlope$SlopeRL[43:99], alternative = "two.sided", var.equal = FALSE)$p.value

#Fig 4C+4D: left-right correlations. This is much longer, code-wise, than non-correlation figures. to that end, I'm preserving its original spacing and comments, though the directory names for list.files commands have been redacted as you will need to substitute your own anyways
library(dplyr)
#for correlation tests
library("ggpubr")

#like any of the other tables that haven't been provided, these are just selective repackagings of data from the supplementary data table. However, since the structure of these is REALLY important for the next 700 lines of code, here's a guide on how to reproduce these tables in particular:
#the column structure for these looks like (taking LRcorr_FLF as an example; that name is short for 'left-right correlation: forelimbs of females')
##ID | tp | jmFLL | jmFLR | wsFLL | wsFLR | pplFLL | pplFLR | dsFLL | dsFLR | msFLL | msFLR | ppoFLL | ppoFLR
##tp = timepoint, FLL is forelimb-left, FLR is forelimb-right, and 'jm', 'ws', 'ppl', 'ds', 'ms', and 'ppo' correspond to the limb-specific measures of BMS. PPO is the aggregate value of paw position at both liftoff and initial contact
#so you'd copy paste the supp table data as necessary. The other tables have slightly different naming conventions as befit whichever sex and limb pair they address, but are otherwise identical
fLF <- read.delim("LRcorr_FLF.txt", h = T)
fLM <- read.delim("LRcorr_FLM.txt", h = T)
hLF <- read.delim("LRcorr_HLF.txt", h = T)
hLM <- read.delim("LRcorr_HLM.txt", h = T)
hSLF <- read.delim("LRcorr_striLenF.txt", h = T)
hSLM <- read.delim("LRcorr_striLenM.txt", h = T)

plot(hSLF$rSL, hSLF$lSL)
cor(hSLF$rSL, hSLF$lSL, method = "pearson", use = "pairwise.complete.obs")

fccor <- cor(hSLF[,2:ncol(hSLF)], method = "pearson", use = "pairwise.complete.obs")

plot(hSLM$rSL, hSLM$lSL)
cor(hSLM$rSL, hSLM$lSL, method = "pearson", use = "pairwise.complete.obs")


###generate the csv correlation tables for each subject
#####
##JM
fLFidList <- unique(fLF$ID)
for (i in 1:length(fLFidList)) {
  table1 <- fLF %>% filter(ID == fLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,wsFLL,wsFLR,pplFLL,pplFLR,dsFLL,dsFLR,msFLL,msFLR,ppoFLL,ppoFLR))
  corrFLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLF, 2)
  form = sprintf('fLF_%s.csv', fLFidList[i])
  write.csv(corrFLF, file = form)
}
fLMidList <- unique(fLM$ID)
for (i in 1:length(fLMidList)) {
  table1 <- fLM %>% filter(ID == fLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,wsFLL,wsFLR,pplFLL,pplFLR,dsFLL,dsFLR,msFLL,msFLR,ppoFLL,ppoFLR))
  corrFLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLM, 2)
  form = sprintf('fLM_%s.csv', fLMidList[i])
  write.csv(corrFLM, file = form)
}
hLFidList <- unique(hLF$ID)
for (i in 1:length(hLFidList)) {
  table1 <- hLF %>% filter(ID == hLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,wsHLL,wsHLR,pplHLL,pplHLR,dsHLL,dsHLR,msHLL,msHLR,ppoHLL,ppoHLR))
  corrHLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLF, 2)
  form = sprintf('hLF_%s.csv', hLFidList[i])
  write.csv(corrHLF, file = form)
}
hLMidList <- unique(hLM$ID)
for (i in 1:length(hLMidList)) {
  table1 <- hLM %>% filter(ID == hLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,wsHLL,wsHLR,pplHLL,pplHLR,dsHLL,dsHLR,msHLL,msHLR,ppoHLL,ppoHLR))
  corrHLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLM, 2)
  form = sprintf('hLM_%s.csv', hLMidList[i])
  write.csv(corrHLM, file = form)
}
#####
##WS
fLFidList <- unique(fLF$ID)
for (i in 1:length(fLFidList)) {
  table1 <- fLF %>% filter(ID == fLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,pplFLL,pplFLR,dsFLL,dsFLR,msFLL,msFLR,ppoFLL,ppoFLR))
  corrFLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLF, 2)
  form = sprintf('fLF_%s.csv', fLFidList[i])
  write.csv(corrFLF, file = form)
}
fLMidList <- unique(fLM$ID)
for (i in 1:length(fLMidList)) {
  table1 <- fLM %>% filter(ID == fLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,pplFLL,pplFLR,dsFLL,dsFLR,msFLL,msFLR,ppoFLL,ppoFLR))
  corrFLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLM, 2)
  form = sprintf('fLM_%s.csv', fLMidList[i])
  write.csv(corrFLM, file = form)
}
hLFidList <- unique(hLF$ID)
for (i in 1:length(hLFidList)) {
  table1 <- hLF %>% filter(ID == hLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,pplHLL,pplHLR,dsHLL,dsHLR,msHLL,msHLR,ppoHLL,ppoHLR))
  corrHLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLF, 2)
  form = sprintf('hLF_%s.csv', hLFidList[i])
  write.csv(corrHLF, file = form)
}
hLMidList <- unique(hLM$ID)
for (i in 1:length(hLMidList)) {
  table1 <- hLM %>% filter(ID == hLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,pplHLL,pplHLR,dsHLL,dsHLR,msHLL,msHLR,ppoHLL,ppoHLR))
  corrHLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLM, 2)
  form = sprintf('hLM_%s.csv', hLMidList[i])
  write.csv(corrHLM, file = form)
}
#####
##PPL
fLFidList <- unique(fLF$ID)
for (i in 1:length(fLFidList)) {
  table1 <- fLF %>% filter(ID == fLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,wsFLL,wsFLR,dsFLL,dsFLR,msFLL,msFLR,ppoFLL,ppoFLR))
  corrFLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLF, 2)
  form = sprintf('fLF_%s.csv', fLFidList[i])
  write.csv(corrFLF, file = form)
}
fLMidList <- unique(fLM$ID)
for (i in 1:length(fLMidList)) {
  table1 <- fLM %>% filter(ID == fLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,wsFLL,wsFLR,dsFLL,dsFLR,msFLL,msFLR,ppoFLL,ppoFLR))
  corrFLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLM, 2)
  form = sprintf('fLM_%s.csv', fLMidList[i])
  write.csv(corrFLM, file = form)
}
hLFidList <- unique(hLF$ID)
for (i in 1:length(hLFidList)) {
  table1 <- hLF %>% filter(ID == hLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,wsHLL,wsHLR,dsHLL,dsHLR,msHLL,msHLR,ppoHLL,ppoHLR))
  corrHLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLF, 2)
  form = sprintf('hLF_%s.csv', hLFidList[i])
  write.csv(corrHLF, file = form)
}
hLMidList <- unique(hLM$ID)
for (i in 1:length(hLMidList)) {
  table1 <- hLM %>% filter(ID == hLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,wsHLL,wsHLR,dsHLL,dsHLR,msHLL,msHLR,ppoHLL,ppoHLR))
  corrHLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLM, 2)
  form = sprintf('hLM_%s.csv', hLMidList[i])
  write.csv(corrHLM, file = form)
}
#####
##DS
fLFidList <- unique(fLF$ID)
for (i in 1:length(fLFidList)) {
  table1 <- fLF %>% filter(ID == fLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,wsFLL,wsFLR,pplFLL,pplFLR,msFLL,msFLR,ppoFLL,ppoFLR))
  corrFLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLF, 2)
  form = sprintf('fLF_%s.csv', fLFidList[i])
  write.csv(corrFLF, file = form)
}
fLMidList <- unique(fLM$ID)
for (i in 1:length(fLMidList)) {
  table1 <- fLM %>% filter(ID == fLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,wsFLL,wsFLR,pplFLL,pplFLR,msFLL,msFLR,ppoFLL,ppoFLR))
  corrFLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLM, 2)
  form = sprintf('fLM_%s.csv', fLMidList[i])
  write.csv(corrFLM, file = form)
}
hLFidList <- unique(hLF$ID)
for (i in 1:length(hLFidList)) {
  table1 <- hLF %>% filter(ID == hLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,wsHLL,wsHLR,pplHLL,pplHLR,msHLL,msHLR,ppoHLL,ppoHLR))
  corrHLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLF, 2)
  form = sprintf('hLF_%s.csv', hLFidList[i])
  write.csv(corrHLF, file = form)
}
hLMidList <- unique(hLM$ID)
for (i in 1:length(hLMidList)) {
  table1 <- hLM %>% filter(ID == hLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,wsHLL,wsHLR,pplHLL,pplHLR,msHLL,msHLR,ppoHLL,ppoHLR))
  corrHLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLM, 2)
  form = sprintf('hLM_%s.csv', hLMidList[i])
  write.csv(corrHLM, file = form)
}
#####
##MS
fLFidList <- unique(fLF$ID)
for (i in 1:length(fLFidList)) {
  table1 <- fLF %>% filter(ID == fLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,wsFLL,wsFLR,pplFLL,pplFLR,dsFLL,dsFLR,ppoFLL,ppoFLR))
  corrFLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLF, 2)
  form = sprintf('fLF_%s.csv', fLFidList[i])
  write.csv(corrFLF, file = form)
}
fLMidList <- unique(fLM$ID)
for (i in 1:length(fLMidList)) {
  table1 <- fLM %>% filter(ID == fLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,wsFLL,wsFLR,pplFLL,pplFLR,dsFLL,dsFLR,ppoFLL,ppoFLR))
  corrFLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLM, 2)
  form = sprintf('fLM_%s.csv', fLMidList[i])
  write.csv(corrFLM, file = form)
}
hLFidList <- unique(hLF$ID)
for (i in 1:length(hLFidList)) {
  table1 <- hLF %>% filter(ID == hLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,wsHLL,wsHLR,pplHLL,pplHLR,dsHLL,dsHLR,ppoHLL,ppoHLR))
  corrHLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLF, 2)
  form = sprintf('hLF_%s.csv', hLFidList[i])
  write.csv(corrHLF, file = form)
}
hLMidList <- unique(hLM$ID)
for (i in 1:length(hLMidList)) {
  table1 <- hLM %>% filter(ID == hLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,wsHLL,wsHLR,pplHLL,pplHLR,dsHLL,dsHLR,ppoHLL,ppoHLR))
  corrHLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLM, 2)
  form = sprintf('hLM_%s.csv', hLMidList[i])
  write.csv(corrHLM, file = form)
}
#####
##PPO
fLFidList <- unique(fLF$ID)
for (i in 1:length(fLFidList)) {
  table1 <- fLF %>% filter(ID == fLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,wsFLL,wsFLR,pplFLL,pplFLR,dsFLL,dsFLR,msFLL,msFLR))
  corrFLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLF, 2)
  form = sprintf('fLF_%s.csv', fLFidList[i])
  write.csv(corrFLF, file = form)
}
fLMidList <- unique(fLM$ID)
for (i in 1:length(fLMidList)) {
  table1 <- fLM %>% filter(ID == fLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmFLL,jmFLR,wsFLL,wsFLR,pplFLL,pplFLR,dsFLL,dsFLR,msFLL,msFLR))
  corrFLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrFLM, 2)
  form = sprintf('fLM_%s.csv', fLMidList[i])
  write.csv(corrFLM, file = form)
}
hLFidList <- unique(hLF$ID)
for (i in 1:length(hLFidList)) {
  table1 <- hLF %>% filter(ID == hLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,wsHLL,wsHLR,pplHLL,pplHLR,dsHLL,dsHLR,msHLL,msHLR))
  corrHLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLF, 2)
  form = sprintf('hLF_%s.csv', hLFidList[i])
  write.csv(corrHLF, file = form)
}
hLMidList <- unique(hLM$ID)
for (i in 1:length(hLMidList)) {
  table1 <- hLM %>% filter(ID == hLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,jmHLL,jmHLR,wsHLL,wsHLR,pplHLL,pplHLR,dsHLL,dsHLR,msHLL,msHLR))
  corrHLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHLM, 2)
  form = sprintf('hLM_%s.csv', hLMidList[i])
  write.csv(corrHLM, file = form)
}
#####
##StriLen
hSLFidList <- unique(hSLF$ID)
for (i in 1:length(hSLFidList)) {
  table1 <- hSLF %>% filter(ID == hSLFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,rPA,lPA))
  corrHSLF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHSLF, 2)
  form = sprintf('hSLF_%s.csv', hSLFidList[i])
  write.csv(corrHSLF, file = form)
}
hSLMidList <- unique(hSLM$ID)
for (i in 1:length(hSLMidList)) {
  table1 <- hSLM %>% filter(ID == hSLMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,rPA,lPA))
  corrHSLM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHSLM, 2)
  form = sprintf('hSLM_%s.csv', hSLMidList[i])
  write.csv(corrHSLM, file = form)
}
#####
##SwayAng
hPAFidList <- unique(hSLF$ID)
for (i in 1:length(hPAFidList)) {
  table1 <- hSLF %>% filter(ID == hPAFidList[i])
  table1 <- subset(table1,select = -c(ID,tp,rSL,lSL))
  corrHPAF <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHPAF, 2)
  form = sprintf('hPAF_%s.csv', hPAFidList[i])
  write.csv(corrHPAF, file = form)
}
hPAMidList <- unique(hSLM$ID)
for (i in 1:length(hPAMidList)) {
  table1 <- hSLM %>% filter(ID == hPAMidList[i])
  table1 <- subset(table1,select = -c(ID,tp,rSL,lSL))
  corrHPAM <- cor(table1, use = "pairwise.complete.obs", method = "pearson")
  round(corrHPAM, 2)
  form = sprintf('hPAM_%s.csv', hPAMidList[i])
  write.csv(corrHPAM, file = form)
}
#####
###to reverse-engineer a dataframe for the boxplot...
#####
##JM
fFLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\jm\\flf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fFLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\jm\\flm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\jm\\hlf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\jm\\hlm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
flJM <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fFLF)) {
  table1 <- read.csv(file = fFLF[i])
  flJM[nrow(flJM) + 1,] = list(basename(fFLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fFLM)) {
  table1 <- read.csv(file = fFLM[i])
  flJM[nrow(flJM) + 1,] = list(basename(fFLM[i]),"Male",table1[2,2])
}
hlJM <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fHLF)) {
  table1 <- read.csv(file = fHLF[i])
  hlJM[nrow(hlJM) + 1,] = list(basename(fHLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fHLM)) {
  table1 <- read.csv(file = fHLM[i])
  hlJM[nrow(hlJM) + 1,] = list(basename(fHLM[i]),"Male",table1[2,2])
}
#####
##WS
fFLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ws\\flf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fFLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ws\\flm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ws\\hlf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ws\\hlm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
flWS <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fFLF)) {
  table1 <- read.csv(file = fFLF[i])
  flWS[nrow(flWS) + 1,] = list(basename(fFLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fFLM)) {
  table1 <- read.csv(file = fFLM[i])
  flWS[nrow(flWS) + 1,] = list(basename(fFLM[i]),"Male",table1[2,2])
}
hlWS <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fHLF)) {
  table1 <- read.csv(file = fHLF[i])
  hlWS[nrow(hlWS) + 1,] = list(basename(fHLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fHLM)) {
  table1 <- read.csv(file = fHLM[i])
  hlWS[nrow(hlWS) + 1,] = list(basename(fHLM[i]),"Male",table1[2,2])
}
#####
##PPL
fFLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ppl\\flf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fFLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ppl\\flm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ppl\\hlf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ppl\\hlm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
flPPL <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fFLF)) {
  table1 <- read.csv(file = fFLF[i])
  flPPL[nrow(flPPL) + 1,] = list(basename(fFLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fFLM)) {
  table1 <- read.csv(file = fFLM[i])
  flPPL[nrow(flPPL) + 1,] = list(basename(fFLM[i]),"Male",table1[2,2])
}
hlPPL <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fHLF)) {
  table1 <- read.csv(file = fHLF[i])
  hlPPL[nrow(hlPPL) + 1,] = list(basename(fHLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fHLM)) {
  table1 <- read.csv(file = fHLM[i])
  hlPPL[nrow(hlPPL) + 1,] = list(basename(fHLM[i]),"Male",table1[2,2])
}
#####
##DS
fFLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ds\\flf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fFLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ds\\flm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ds\\hlf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ds\\hlm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
flDS <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fFLF)) {
  table1 <- read.csv(file = fFLF[i])
  flDS[nrow(flDS) + 1,] = list(basename(fFLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fFLM)) {
  table1 <- read.csv(file = fFLM[i])
  flDS[nrow(flDS) + 1,] = list(basename(fFLM[i]),"Male",table1[2,2])
}
hlDS <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fHLF)) {
  table1 <- read.csv(file = fHLF[i])
  hlDS[nrow(hlDS) + 1,] = list(basename(fHLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fHLM)) {
  table1 <- read.csv(file = fHLM[i])
  hlDS[nrow(hlDS) + 1,] = list(basename(fHLM[i]),"Male",table1[2,2])
}
#####
##MS
fFLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ms\\flf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fFLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ms\\flm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ms\\hlf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ms\\hlm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
flMS <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fFLF)) {
  table1 <- read.csv(file = fFLF[i])
  flMS[nrow(flMS) + 1,] = list(basename(fFLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fFLM)) {
  table1 <- read.csv(file = fFLM[i])
  flMS[nrow(flMS) + 1,] = list(basename(fFLM[i]),"Male",table1[2,2])
}
hlMS <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fHLF)) {
  table1 <- read.csv(file = fHLF[i])
  hlMS[nrow(hlMS) + 1,] = list(basename(fHLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fHLM)) {
  table1 <- read.csv(file = fHLM[i])
  hlMS[nrow(hlMS) + 1,] = list(basename(fHLM[i]),"Male",table1[2,2])
}
#####
##PPO
fFLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ppo\\flf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fFLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ppo\\flm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ppo\\hlf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\ppo\\hlm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
flPPO <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fFLF)) {
  table1 <- read.csv(file = fFLF[i])
  flPPO[nrow(flPPO) + 1,] = list(basename(fFLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fFLM)) {
  table1 <- read.csv(file = fFLM[i])
  flPPO[nrow(flPPO) + 1,] = list(basename(fFLM[i]),"Male",table1[2,2])
}
hlPPO <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fHLF)) {
  table1 <- read.csv(file = fHLF[i])
  hlPPO[nrow(hlPPO) + 1,] = list(basename(fHLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fHLM)) {
  table1 <- read.csv(file = fHLM[i])
  hlPPO[nrow(hlPPO) + 1,] = list(basename(fHLM[i]),"Male",table1[2,2])
}
#####
##StriLen
fHLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\sl\\hlf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\sl\\hlm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
hlSL <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fHLF)) {
  table1 <- read.csv(file = fHLF[i])
  hlSL[nrow(hlSL) + 1,] = list(basename(fHLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fHLM)) {
  table1 <- read.csv(file = fHLM[i])
  hlSL[nrow(hlSL) + 1,] = list(basename(fHLM[i]),"Male",table1[2,2])
}
#####
##SwayAng
fHLF <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\pang\\hlf", pattern="*.csv", full.names=TRUE, recursive=FALSE)
fHLM <- list.files(path="C:\\[whatever directory you're using]\\LR_CorrTables\\pang\\hlm", pattern="*.csv", full.names=TRUE, recursive=FALSE)
hlPA <- data.frame(ID = character(), Sex = character(), Correlation = numeric())
for (i in 1:length(fHLF)) {
  table1 <- read.csv(file = fHLF[i])
  hlPA[nrow(hlPA) + 1,] = list(basename(fHLF[i]),"Female",table1[2,2])
}
for (i in 1:length(fHLM)) {
  table1 <- read.csv(file = fHLM[i])
  hlPA[nrow(hlPA) + 1,] = list(basename(fHLM[i]),"Male",table1[2,2])
}

#####
###box city box plots
#####
boxplot(hlSL$Correlation[hlSL$Sex == "Female"],
        hlSL$Correlation[hlSL$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = hlSL, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Left/Right stride length"))
box( bty = "L")
boxplot(hlPA$Correlation[hlPA$Sex == "Female"],
        hlPA$Correlation[hlPA$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = hlPA, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Left/Right paw angle"))
box( bty = "L")
boxplot(flJM$Correlation[flJM$Sex == "Female"],
        flJM$Correlation[flJM$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = flJM, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Forelimb Left/Right JM"))
box( bty = "L")
boxplot(hlJM$Correlation[hlJM$Sex == "Female"],
        hlJM$Correlation[hlJM$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = hlJM, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Hindlimb Left/Right JM"))
box( bty = "L")
boxplot(flWS$Correlation[flWS$Sex == "Female"],
        flWS$Correlation[flWS$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = flWS, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Forelimb Left/Right WS"))
box( bty = "L")
boxplot(hlWS$Correlation[hlWS$Sex == "Female"],
        hlWS$Correlation[hlWS$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = hlWS, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Hindlimb Left/Right WS"))
box( bty = "L")
boxplot(flPPL$Correlation[flPPL$Sex == "Female"],
        flPPL$Correlation[flPPL$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = flPPL, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Forelimb Left/Right PPL"))
box( bty = "L")
boxplot(hlPPL$Correlation[hlPPL$Sex == "Female"],
        hlPPL$Correlation[hlPPL$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = hlPPL, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Hindlimb Left/Right PPL"))
box( bty = "L")
boxplot(flDS$Correlation[flDS$Sex == "Female"],
        flDS$Correlation[flDS$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = flDS, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Forelimb Left/Right DS"))
box( bty = "L")
boxplot(hlDS$Correlation[hlDS$Sex == "Female"],
        hlDS$Correlation[hlDS$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = hlDS, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Hindlimb Left/Right DS"))
box( bty = "L")
boxplot(flMS$Correlation[flMS$Sex == "Female"],
        flMS$Correlation[flMS$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = flMS, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Forelimb Left/Right MS"))
box( bty = "L")
boxplot(hlMS$Correlation[hlMS$Sex == "Female"],
        hlMS$Correlation[hlMS$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = hlMS, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Hindlimb Left/Right MS"))
box( bty = "L")
boxplot(flPPO$Correlation[flPPO$Sex == "Female"],
        flPPO$Correlation[flPPO$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = flPPO, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Forelimb Left/Right PPO"))
box( bty = "L")
boxplot(hlPPO$Correlation[hlPPO$Sex == "Female"],
        hlPPO$Correlation[hlPPO$Sex == "Male"], notch = F,
        names = c("Female", "Male"),
        boxwex = 0.5, staplewex = 0.5, outwex = 2, outline = F, col = NA,
        ylim = c(-1, 1), frame.plot = FALSE, las = 1, ylab = "Correlation ratio")
stripchart(Correlation ~ Sex, vertical = TRUE, data = hlPPO, method =
             "jitter", add = TRUE, pch = 20, cex = 1, col =
             alpha(c("darkorange3", "#4AA248"), 0.7),
           jitter = 0.2)
##title(paste("Hindlimb Left/Right PPO"))
box( bty = "L")

#####
###The Mega Correlation Table
#####
maleMegaCorr <- data.frame(FLjm = numeric(), HLjm = numeric(),FLws = numeric(), HLws = numeric(),FLppl = numeric(), HLppl = numeric(),FLds = numeric(), HLds = numeric(),FLms = numeric(), HLms = numeric(),FLppo = numeric(), HLppo = numeric(), HLsl = numeric(), HLpa = numeric())
var1 <- mean(flJM$Correlation[46:86], na.rm=TRUE)
var2 <- mean(hlJM$Correlation[46:86], na.rm=TRUE)
var3 <- mean(flWS$Correlation[46:86], na.rm=TRUE)
var4 <- mean(hlWS$Correlation[46:86], na.rm=TRUE)
var5 <- mean(flPPL$Correlation[46:86], na.rm=TRUE)
var6 <- mean(hlPPL$Correlation[46:86], na.rm=TRUE)
var7 <- mean(flDS$Correlation[46:86], na.rm=TRUE)
var8 <- mean(hlDS$Correlation[46:86], na.rm=TRUE)
var9 <- mean(flMS$Correlation[46:86], na.rm=TRUE)
var10 <- mean(hlMS$Correlation[46:86], na.rm=TRUE)
var11 <- mean(flPPO$Correlation[46:86], na.rm=TRUE)
var12 <- mean(hlPPO$Correlation[46:86], na.rm=TRUE)
var13 <- mean(hlSL$Correlation[16:28], na.rm=TRUE)
var14 <- mean(hlPA$Correlation[16:28], na.rm=TRUE)
maleMegaCorr[nrow(maleMegaCorr) + 1,] = list(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,var13,var14)
form = sprintf('mCorrTable.csv', i)
write.csv(maleMegaCorr, file = form)
femaleMegaCorr <- data.frame(FLjm = numeric(), HLjm = numeric(),FLws = numeric(), HLws = numeric(),FLppl = numeric(), HLppl = numeric(),FLds = numeric(), HLds = numeric(),FLms = numeric(), HLms = numeric(),FLppo = numeric(), HLppo = numeric(), HLsl = numeric(), HLpa = numeric())
var1 <- mean(flJM$Correlation[1:45], na.rm=TRUE)
var2 <- mean(hlJM$Correlation[1:45], na.rm=TRUE)
var3 <- mean(flWS$Correlation[1:45], na.rm=TRUE)
var4 <- mean(hlWS$Correlation[1:45], na.rm=TRUE)
var5 <- mean(flPPL$Correlation[1:45], na.rm=TRUE)
var6 <- mean(hlPPL$Correlation[1:45], na.rm=TRUE)
var7 <- mean(flDS$Correlation[1:45], na.rm=TRUE)
var8 <- mean(hlDS$Correlation[1:45], na.rm=TRUE)
var9 <- mean(flMS$Correlation[1:45], na.rm=TRUE)
var10 <- mean(hlMS$Correlation[1:45], na.rm=TRUE)
var11 <- mean(flPPO$Correlation[1:45], na.rm=TRUE)
var12 <- mean(hlPPO$Correlation[1:45], na.rm=TRUE)
var13 <- mean(hlSL$Correlation[1:15], na.rm=TRUE)
var14 <- mean(hlPA$Correlation[1:15], na.rm=TRUE)
femaleMegaCorr[nrow(femaleMegaCorr) + 1,] = list(var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12,var13,var14)
form = sprintf('fCorrTable.csv', i)
write.csv(femaleMegaCorr, file = form)

#####
###the comparisons, BH-correction is possible by adding them to this table
#####
#forelimbs
pCorrectionTable <- data.frame(wilt = numeric())
varlet1 <- NA
varlet2 <- wilcox.test(flWS$Correlation[1:45],flWS$Correlation[46:86], alternative = c("t"), mu = 0, paired = FALSE, exact = NULL, correct = FALSE, conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf)$p.value
varlet3 <- wilcox.test(flPPL$Correlation[1:45],flPPL$Correlation[46:86], alternative = c("t"), mu = 0, paired = FALSE, exact = NULL, correct = FALSE, conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf)$p.value
varlet4 <- NA
varlet5 <- wilcox.test(flMS$Correlation[1:45],flMS$Correlation[46:86], alternative = c("t"), mu = 0, paired = FALSE, exact = NULL, correct = FALSE, conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf)$p.value
varlet6 <- NA
pCorrectionTable[nrow(pCorrectionTable) + 1,] = list(varlet1)
pCorrectionTable[nrow(pCorrectionTable) + 1,] = list(varlet2)
pCorrectionTable[nrow(pCorrectionTable) + 1,] = list(varlet3)
pCorrectionTable[nrow(pCorrectionTable) + 1,] = list(varlet4)
pCorrectionTable[nrow(pCorrectionTable) + 1,] = list(varlet5)
pCorrectionTable[nrow(pCorrectionTable) + 1,] = list(varlet6)
#hindlimbs
pCorrectionTableL <- data.frame(wilt = numeric())
varlet1 <- NA
varlet2 <- wilcox.test(hlWS$Correlation[1:45],hlWS$Correlation[46:86], alternative = c("t"), mu = 0, paired = FALSE, exact = NULL, correct = FALSE, conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf)$p.value
varlet3 <- NA
varlet4 <- NA
varlet5 <- wilcox.test(hlMS$Correlation[1:45],hlMS$Correlation[46:86], alternative = c("t"), mu = 0, paired = FALSE, exact = NULL, correct = FALSE, conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf)$p.value
varlet6 <- wilcox.test(hlPPO$Correlation[1:45],hlPPO$Correlation[46:86], alternative = c("t"), mu = 0, paired = FALSE, exact = NULL, correct = FALSE, conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf)$p.value
varlet7 <- wilcox.test(hlSL$Correlation[1:15],hlSL$Correlation[16:28], alternative = c("t"), mu = 0, paired = FALSE, exact = NULL, correct = FALSE, conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf)$p.value
varlet8 <- wilcox.test(hlPA$Correlation[1:15],hlPA$Correlation[16:28], alternative = c("t"), mu = 0, paired = FALSE, exact = NULL, correct = FALSE, conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf)$p.value
pCorrectionTableL[nrow(pCorrectionTableL) + 1,] = list(varlet1)
pCorrectionTableL[nrow(pCorrectionTableL) + 1,] = list(varlet2)
pCorrectionTableL[nrow(pCorrectionTableL) + 1,] = list(varlet3)
pCorrectionTableL[nrow(pCorrectionTableL) + 1,] = list(varlet4)
pCorrectionTableL[nrow(pCorrectionTableL) + 1,] = list(varlet5)
pCorrectionTableL[nrow(pCorrectionTableL) + 1,] = list(varlet6)
pCorrectionTableL[nrow(pCorrectionTableL) + 1,] = list(varlet7)
pCorrectionTableL[nrow(pCorrectionTableL) + 1,] = list(varlet8)

p.adjust(p = pCorrectionTable$wilt, method = "BH")
p.adjust(p = pCorrectionTableL$wilt, method = "BH")

#ok that's the end of Figs 4C+4D.
#Fig 4E is obtained by taking the values from the output 'fCorrTable.csv' and 'mCorrTable.csv', arranging them as shown in Microsoft Excel, then creating a new rule in its Conditional Formatting rule to apply a three-color scale to them. The colors and their values are provided in the legend of the figure

#####
#Fig 5
#####
#Make a table for each sex (data in each criterion&tp cell = [avg score for ALS/avg score for WT])
#Use Excel's conditional formatting tool to implement a two-color scale as shown by the figure legend.
#cluster those tables here in R:
library(factoextra)
library(survminer)
library(ggcorrplot)
library(fastqcr)
####female data
##female dendrogram by all tp vals
femaleDataAll <- read.csv(file = "fAllOnly.csv")
rownames(femaleDataAll) <- femaleDataAll[,1]
femaleDataAll <- femaleDataAll[,-1]
#make dendrogram
fData2 <- dist(scale(femaleDataAll), method = "euclidean")
hcf2 <- hclust(fData2, method = "ward.D2")
plot(hcf2, hang = 0.2, main = "Female Aspects of Locomotion Cluster Dendrogram",
     sub="this is literally just the table")
####male data
##male dendrogram by all tp vals
maleDataAll <- read.csv(file = "mAllOnly.csv")
rownames(maleDataAll) <- maleDataAll[,1]
maleDataAll <- maleDataAll[,-1]
#make dendrogram
mData2 <- dist(scale(maleDataAll), method = "euclidean")
hcm2 <- hclust(mData2, method = "ward.D2")
plot(hcm2, hang = 0.2, main = "Male Aspects of Locomotion Cluster Dendrogram",
     sub="this is literally just the table")