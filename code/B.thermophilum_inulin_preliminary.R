library(ggplot2);library(tidyverse)

#load data manually---issues with setwd() perhaps due to windows computer?
mydf<-read_csv("C:\\Users\\rmoge\\Box Sync\\JTL_Lab\\Lab.Notebook\\20190820_Gut2\\data\\20190923_ODfinal_Bt.Ec\\cases.OD_600.csv")

#you can use the option outlier.size for the boxplot command http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization

#improve the carbon source values so that the figure has prettier labels
mydf <- mydf %>% mutate (carbon=replace(carbon, carbon == "CLR", "CLR (DP 7-9)"))
mydf <- mydf %>% mutate (carbon=replace(carbon, carbon == "TEX23", "Pure TEX (DP >23)"))
mydf <- mydf %>% mutate (carbon=replace(carbon, carbon == "gluc", "glucose (DP 1)"))
mydf <- mydf %>% mutate (carbon=replace(carbon, carbon == "kest", "kestose (DP 3)"))
mydf <- mydf %>% mutate (carbon=replace(carbon, carbon == "sensus", "Impure TEX (DP ~23)"))
mydf$carbon<-factor(mydf$carbon, levels=c("glucose (DP 1)", "kestose (DP 3)", "CLR (DP 7-9)", "Impure TEX (DP ~23)", "Pure TEX (DP >23)"))

#box + dot plot for OD data--- B ther
g <- ggplot(mydf, aes(carbon, OD.600))
mygg <- g +
  geom_boxplot() + 
  geom_dotplot(binaxis='y',stackdir='center', dotsize = 1, fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.5, size =26)) + 
  labs(x="Carbon source",y="OD 600",size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.key=element_blank(), axis.text=element_text(size=34),axis.title=element_text(size=36),legend.text=element_text(size=22),legend.title = element_text(size=34), axis.line.x = element_line(color="black", size = 1.5), axis.line.y = element_line(color="black", size = 1.5), axis.ticks.y = element_line(color = "black", size = 1.5), axis.ticks.x = element_blank())

plot(mygg)

#ANOVA for B therm OD 600
OD.aov <- aov(mydf$OD.600 ~ mydf$carbon)
summary(OD.aov)
ODHSD<-TukeyHSD(OD.aov)
ODHSD


#load data for B ther growth curves on kestose, CLR, and pure TEX
grcv1 <- read_csv("C:\\Users\\rmoge\\Box Sync\\JTL_Lab\\Lab.Notebook\\20190820_Gut2\\data\\20190919_grcv_Bt_3.types\\cases.grcv_20190919.csv")
grcv1 <- grcv1 %>% mutate (carbon=replace(carbon, carbon == "CLR", "CLR (DP 7-9)"))
grcv1 <- grcv1 %>% mutate (carbon=replace(carbon, carbon == "TEX23", "Pure TEX (DP >23)"))
grcv1 <- grcv1 %>% mutate (carbon=replace(carbon, carbon == "kest", "kestose (DP 3)"))
grcv1$carbon<-factor(grcv1$carbon, levels=c("kestose (DP 3)", "CLR (DP 7-9)", "Pure TEX (DP >23)"))

#box + dot plot for B ther umax
grcv1_plot <- ggplot(grcv1, aes(carbon, umax))
mygrcv1 <- grcv1_plot +
  geom_boxplot() + 
  geom_dotplot(binaxis='y',stackdir='center', dotsize = 1, fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.5, size =26)) + 
  labs(x="Carbon source",y="umax (1/hr)",size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.key=element_blank(), axis.text=element_text(size=34),axis.title=element_text(size=36),legend.text=element_text(size=22),legend.title = element_text(size=34), axis.line.x = element_line(color="black", size = 1.5), axis.line.y = element_line(color="black", size = 1.5), axis.ticks.y = element_line(color = "black", size = 1.5), axis.ticks.x = element_blank())

plot(mygrcv1)
#ANOVA for B ther umax
umax1.aov <- aov(grcv1$umax ~ grcv1$carbon)
summary(umax1.aov)
umax1HSD<-TukeyHSD(umax1.aov)
umax1HSD

#Jay asked for a comparison using only CLR and pure TEX--- subset the data
twocdtns<-filter(mydf, carbon=="CLR (DP 7-9)" | carbon=="Pure TEX (DP >23)")

#box + dot plot for subsetted data
g_CLR_TEX <- ggplot(twocdtns, aes(carbon, OD.600))
my_CLR_TEX <- g_CLR_TEX +
  geom_boxplot() + 
  geom_dotplot(binaxis='y',stackdir='center', dotsize = 1, fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.5, size =26)) + 
  labs(x="Carbon source",y="OD 600",size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits=c(0,6.5), expand = c(0,0)) +
  theme(legend.key=element_blank(), axis.text=element_text(size=34),axis.title=element_text(size=36),legend.text=element_text(size=22),legend.title = element_text(size=34), axis.line.x = element_line(color="black", size = 1.5), axis.line.y = element_line(color="black", size = 1.5), axis.ticks.y = element_line(color = "black", size = 1.5), axis.ticks.x = element_blank())

plot(my_CLR_TEX)

#box + jittered dot plot for umax data
grcv2_plot <- ggplot(grcv1, aes(carbon, umax))
mygrcv2 <- grcv2_plot +
  geom_boxplot() + 
  geom_point(fill=aes("red"), size=5, shape=21, colour="grey20", position=position_jitter(width=0.2, height=0.1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.5, size =26)) + 
  labs(x="Carbon source",y="umax (1/hr)",size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.key=element_blank(), axis.text=element_text(size=34),axis.title=element_text(size=36),legend.text=element_text(size=22),legend.title = element_text(size=34), axis.line.x = element_line(color="black", size = 1.5), axis.line.y = element_line(color="black", size = 1.5), axis.ticks.y = element_line(color = "black", size = 1.5), axis.ticks.x = element_blank())

plot(mygrcv2)

#box + jittered dot plot for Jay's subsetted data
g_CLR_TEX_jitter <- ggplot(twocdtns, aes(carbon, OD.600))
my_CLR_TEX_jitter <- g_CLR_TEX_jitter +
  geom_boxplot(outlier.size = 6) + 
  geom_point(fill=aes("red"), size=8.4, shape=21, colour="grey20", position=position_jitter(width=0.201, height=0.0)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.5, size =26)) + 
  labs(x="Carbon source",y="OD 600",size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #scale_y_continuous(breaks = c(0,1,2,3,4,5,6), limits=c(0,6.5), expand = c(0,0)) +
  theme(legend.key=element_blank(), axis.text=element_text(size=34),axis.title=element_text(size=36),legend.text=element_text(size=22),legend.title = element_text(size=34), axis.line.x = element_line(color="black", size = 1.5), axis.line.y = element_line(color="black", size = 1.5), axis.ticks.y = element_line(color = "black", size = 1.5), axis.ticks.x = element_blank())

plot(my_CLR_TEX_jitter)

CLR_TEX.aov <- aov(twocdtns$OD.600 ~ twocdtns$carbon)
summary(CLR_TEX.aov)
CLR_TEX_HSD<-TukeyHSD(CLR_TEX.aov)
CLR_TEX_HSD
#P = 7.76x10^-6